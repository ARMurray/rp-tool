# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(plotly)
library(shinyjs)
library(purrr)
library(ggplot2)
library(bslib)
library(tidyr)
library(writexl)
library(plotly)

# Load helper functions
source("R/functions.R")

# Write directory for tinytex
texmf_var <- file.path(Sys.getenv("HOME"), ".TinyTeX", "texmf-var")
dir.create(texmf_var, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TEXMFVAR = texmf_var, TEXMFCACHE = texmf_var)

# ── Findings-page display helpers ─────────────────────────────────────────
# Defined here (rather than in R/functions.R) so app.R is self-contained;
# safe to relocate into R/functions.R verbatim — nothing else changes.
# Compact outfall label for the Findings RP table: "E001" for external,
# "I001" for internal (INO). The full "External Outfall 001" label is still
# produced by format_outfall_label() and appears in the plot title, the
# report, and everywhere else. Internal outfalls additionally render red +
# bold in the table via DT::formatStyle keyed on the hidden
# perm_feature_type_code column, so the prefix and the color are redundant
# signals — either alone survives sorting, filtering, or colorblindness.
format_outfall_label_short <- function(
  perm_feature_nmbr,
  perm_feature_type_code = NA_character_
) {
  is_internal <- !is.na(perm_feature_type_code) &
    perm_feature_type_code == "INO"
  ifelse(
    is_internal,
    paste0("I", perm_feature_nmbr),
    paste0("E", perm_feature_nmbr)
  )
}

# Compact water-class label for the Findings RP table. Applied AFTER
# format_water_class(), so sub-class suffixes survive:
#   "class SB waters"                     -> "SB"
#   "class SD waters- drinking water"     -> "SD- drinking water"
#   "class SD waters \u2014 stream"       -> "SD \u2014 stream"
#   "surface waters"                      -> "surface"
# Display-only: every filter/emphasis join keys on CRITERION_ID or the raw
# class string in rv$rp_concentration, never on this label.
shorten_water_class <- function(x) {
  x <- gsub("^class\\s+", "", x)
  x <- gsub("\\s*waters\\s*", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Most recent numeric prior permit limit for a parameter x outfall, converted
# to WQS units so it can be drawn on the inspector plot's y-axis.
#
# IMPORTANT UNIT NOTE: dmr_value_nmbr is converted to WQS units at load time
# (see the get_unit_conversion() rowwise mutate in the DMR loaders), but the
# limit_value_nmbr / limit_unit_desc fields are NOT — they stay in the units
# recorded in ICIS. Chlorine at outfall 001 is the canonical example: limit
# 0.2 mg/L on a ug/L axis must plot at 200, not 0.2. This helper applies the
# same conversion contract as the loaders:
#     flag NA/PASS       -> multiply
#     CONVERT_F_TO_C     -> (F - 32) * 5/9
#     anything else      -> return NA (no line drawn; matches old "None")
#
# Returns list(y = <numeric in WQS units or NA>,
#              label = "Prior Permit Limit (0.2 mg/L)" or NA)
prior_limit_info <- function(obs, wqs_unit) {
  if (
    is.null(obs) ||
      nrow(obs) == 0 ||
      !"limit_value_nmbr" %in% names(obs)
  ) {
    return(list(y = NA_real_, label = NA_character_))
  }

  lim_row <- obs %>%
    dplyr::mutate(
      begin = if ("limit_begin_date" %in% names(.)) {
        suppressWarnings(
          dplyr::coalesce(
            lubridate::ymd(limit_begin_date), # ISO format from SQLite
            lubridate::mdy(limit_begin_date) # MM/DD/YYYY from ECHO/upload
          )
        )
      } else {
        as.Date(NA)
      },
      end = if ("limit_end_date" %in% names(.)) {
        suppressWarnings(
          dplyr::coalesce(
            lubridate::ymd(limit_end_date),
            lubridate::mdy(limit_end_date)
          )
        )
      } else {
        as.Date(NA)
      },
      lim_val_num = suppressWarnings(as.numeric(limit_value_nmbr))
    ) %>%
    dplyr::filter(is.finite(lim_val_num) & lim_val_num > 0) %>%
    dplyr::arrange(dplyr::desc(begin), dplyr::desc(end)) %>%
    dplyr::slice(1)

  if (nrow(lim_row) != 1) {
    return(list(y = NA_real_, label = NA_character_))
  }

  lim_unit <- if ("limit_unit_desc" %in% names(lim_row)) {
    lim_row$limit_unit_desc
  } else {
    NA_character_
  }

  conv <- get_unit_conversion(lim_unit, wqs_unit)
  y <- if (isTRUE(conv$flag == "CONVERT_F_TO_C")) {
    (lim_row$lim_val_num - 32) * 5 / 9
  } else if (
    (is.na(conv$flag) || isTRUE(conv$flag == "PASS")) &&
      is.finite(conv$mult)
  ) {
    lim_row$lim_val_num * conv$mult
  } else {
    NA_real_
  }

  list(
    y = y,
    # Label carries the ORIGINAL value + unit for honesty: the line sits at
    # the converted height, but the permit record says "0.2 mg/L".
    label = paste0(
      "Prior Permit Limit (",
      round(lim_row$lim_val_num, 3),
      " ",
      lim_unit %||% "",
      ")"
    )
  )
}


# Load crosswalk.
#
# The source CSV is built by joining the NPDES form pollutant list to the PR
# WQS criterion table via several independent strategies (Manual mapping,
# Name match, CAS match). Two artifacts of that build need cleaning here:
#
#   1. Temperature criterion 79218 appears as both "Temperature (summer)" and
#      "Temperature (winter)" — the same underlying criterion with different
#      seasonal labels. Collapse to "Temperature" so they deduplicate cleanly.
#   2. Some (parameter_code, CRITERION_ID) pairs appear under TWO different
#      NPDES_Pollutant labels because two join strategies both matched —
#      e.g. phosphorus 00665 / 79599 appears as "Phosphorus" (Manual join)
#      and "Total phosphorus" (Name join). dedupe_crosswalk() collapses these
#      to the Manual row while preserving intentional dual labels (pH
#      min/max) that share the same join method.
crosswalk <- read_csv(
  "data/crosswalk_v2.csv",
  col_types = cols(parameter_code = col_character())
) %>%
  mutate(parameter_code = str_pad(parameter_code, 5, "left", pad = "0")) %>%
  mutate(
    NPDES_Pollutant = ifelse(
      NPDES_Pollutant %in% c("Temperature (summer)", "Temperature (winter)"),
      "Temperature",
      NPDES_Pollutant
    )
  ) %>%
  distinct() %>%
  dedupe_crosswalk()

# Semantic statistic token -> ICIS (statistical_base_type_code,
# statistical_base_short_desc) pairs. Kept out of the crosswalk so new
# short_desc spellings are a one-line edit here rather than a 545-row
# migration.
statistic_lookup <- read_csv(
  "data/statistic_lookup.csv",
  show_col_types = FALSE
)

# Fail loudly at startup rather than producing empty joins at runtime.
stopifnot(all(
  stats::na.omit(crosswalk$statistic[nzchar(crosswalk$statistic)]) %in%
    statistic_lookup$statistic
))

# Ammonia codes that represent the same measurement as 00610 (Total Ammonia
# Nitrogen) and must be coalesced into it so they match crosswalk criterion
# 79613. 82230 = "Ammonia & ammonium - total"; 00609 = "Ammonia nitrogen,
# total [as N]". Permits use these interchangeably, and a permit reporting
# only 00609 previously produced no TAN result at all.
AMMONIA_ALIAS_CODES <- c("82230", "00609")

# Load NPDES forms
npdes_forms <- read_csv("data/NPDES_Forms_Pollutants_1.csv") %>%
  mutate(
    Pollutant = ifelse(
      Pollutant %in% c("Temperature (summer)", "Temperature (winter)"),
      "Temperature",
      Pollutant
    )
  ) %>%
  distinct()

# Load metals limits
metal_limits_cache <- read_csv("www/metal_limits.csv", show_col_types = FALSE)

# Load DMR parameter descriptions for flagging display names
dmr_parameters_lookup <- read_csv(
  "data/dmr_parameters.csv",
  col_types = cols(parameter_code = col_character())
) %>%
  dplyr::mutate(
    parameter_code = stringr::str_pad(parameter_code, 5, "left", "0")
  ) %>%
  dplyr::distinct(parameter_code, parameter_desc)


# ── SQLite database path ──────────────────────────────────────────────────────
# Set PR_RP_SQLITE env var on Connect, or falls back to data/pr_rp.sqlite
SQLITE_PATH <- Sys.getenv("PR_RP_SQLITE", unset = "data/pr_rp.sqlite")

# ── Load facility cache from SQLite ───────────────────────────────────────────
# Replaces the ECHO API call that was rate-limited on Connect.
# Data is refreshed weekly by refresh_db.R running on a schedule.

message("Loading facility cache from SQLite: ", SQLITE_PATH)

load_facilities_from_db <- function(path) {
  if (!file.exists(path)) {
    message("SQLite database not found at: ", path)
    return(NULL)
  }
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con))
      df <- DBI::dbGetQuery(con, "SELECT * FROM facilities")
      if (is.null(df) || nrow(df) == 0) {
        message("Facilities table is empty.")
        return(NULL)
      }
      # Ensure column names match what the app expects
      df <- df %>%
        dplyr::rename(
          SourceID = permit_id,
          CWPName = facility_name,
          FacLat = fac_lat,
          FacLong = fac_long
        ) %>%
        dplyr::mutate(
          FacLat = as.numeric(FacLat),
          FacLong = as.numeric(FacLong),
          display_label = paste0(SourceID, " - ", CWPName)
        )
      message(sprintf("Loaded %d facilities from SQLite.", nrow(df)))
      df
    },
    error = function(e) {
      message("Error loading facilities from SQLite: ", e$message)
      NULL
    }
  )
}

global_permit_data <- load_facilities_from_db(SQLITE_PATH)

# Read last refresh timestamp from sync_log
db_last_updated <- tryCatch(
  {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      SQLITE_PATH,
      flags = RSQLite::SQLITE_RO
    )
    on.exit(DBI::dbDisconnect(con))
    row <- DBI::dbGetQuery(
      con,
      "SELECT run_timestamp, n_facilities, n_dmr_records
     FROM sync_log
     ORDER BY run_timestamp DESC
     LIMIT 1"
    )
    if (nrow(row) == 0) NULL else row
  },
  error = function(e) NULL
)

# ---------------- UI ----------------
ui <- fluidPage(
  # Small gap between the title bar and the viewport top edge (full-screen
  # previously touched the edge); applies uniformly across pages.
  style = "padding-top: 12px;",
  useShinyjs(),
  #theme = bslib::bs_theme(),
  theme = bslib::bs_theme(
    bg = "#ffffff",
    primary = "#07648d",
    success = "#4d8055",
    fg = "#000",
    info = "#07648d"
  ),

  # Updated Title Panel with a Help Button
  div(
    class = "d-flex justify-content-between align-items-center",
    titlePanel(
      title = span(
        img(src = "epa_logo.png", height = 75, style = "margin-right: 15px;"),
        "Puerto Rico Reasonable Potential (RP) Calculator"
      )
    ),
    div(
      style = "display: flex; gap: 8px; margin-top: 20px; margin-right: 20px;",
      actionButton(
        "back_to_start",
        "← Back to Start",
        icon = icon("home"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        "show_guide",
        "User Guide",
        icon = icon("question-circle"),
        class = "btn-info"
      )
    )
  ),
  hr(),
  uiOutput("page_content")
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  #bs_themer()

  # User Guide
  observeEvent(input$show_guide, {
    showModal(modalDialog(
      title = "RPA User Guide",
      # Using an iframe to display the pre-rendered HTML
      tags$iframe(
        src = "RPA_User_Guide.html",
        width = "100%",
        height = "600px",
        style = "border: none;"
      ),
      size = "xl", # Extra large modal for better reading
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  metals_ids <- c("79228", "79238", "79236", "79253", "79248", "79251", "79264")

  # Reactive Values
  current_page <- reactiveVal("landing")
  rv <- reactiveValues(
    crosswalk = NULL, # form + water class filtered (for RP calc)
    crosswalk_full = NULL, # water class filtered only (for ECHO scope + flagging)
    crosswalk_effective = NULL, # runtime: crosswalk_full + manual rows - excluded
    form_lookup = NULL, # parameter_code -> associated_forms
    dmr = NULL,
    flagged_params = NULL, # parameters with DMR data but no WQS match
    wqs_overrides = NULL, # user decisions from the review panel
    quick_run_flag = FALSE,
    quick_run_params = character(0),
    selected_facility = NULL,
    ph_dmr = NULL,
    temp_dmr = NULL,
    dataset_source = "DMR Only",
    canonical_df_all = NULL,
    mf_by_outfall_all = NULL,
    rwc_sum_all = NULL,
    data_flags_all = NULL,
    wqs_df_canon = NULL,
    permit_limits_df = NULL,
    rp_comparisons = NULL,
    # Search-page state persistence. These hold the user's most recent
    # selections so re-entering the search page (Back to Search, navigating
    # back from a fetched permit, etc.) restores their inputs instead of
    # resetting to the hardcoded defaults. NULL on first visit so the
    # initial render falls back to defaults via %||%.
    saved_permit_id = NULL,
    saved_date_start = NULL,
    saved_date_end = NULL,
    saved_npdes_forms = NULL,
    saved_water_type = NULL
  )

  # Reactive wrapper around global_permit_data so it can be refreshed
  permit_data_rv <- reactiveVal(global_permit_data)

  # Facility load status banner — shown above the selectize when data is NULL
  output$facility_load_status <- renderUI({
    pd <- permit_data_rv()
    if (!is.null(pd) && nrow(pd) > 0) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      style = "padding: 8px 12px; margin-bottom: 8px;",
      tags$b("⚠ Facility list could not be loaded."),
      " The database file may be missing or not yet populated. Contact your administrator.",
      tags$br(),
      actionButton(
        "retry_facility_load",
        "↺ Retry",
        class = "btn-sm btn-warning",
        style = "margin-top: 6px;"
      )
    )
  })

  # Manual retry — fires when user clicks the retry button on the select page
  observeEvent(input$retry_facility_load, {
    showNotification(
      "Reloading facility list from database...",
      type = "message",
      duration = 3
    )
    refreshed <- load_facilities_from_db(SQLITE_PATH)
    permit_data_rv(refreshed)
    if (is.null(refreshed)) {
      showNotification(
        "Could not load facility list. The database file may be missing or corrupt. Contact your administrator.",
        type = "error",
        duration = 8
      )
    } else {
      showNotification(
        sprintf("Loaded %d facilities.", nrow(refreshed)),
        type = "message",
        duration = 3
      )
    }
  })

  # POPULATE SELECTIZE — fires once per page-entry, when permit_data_rv has data.
  #
  # This observer must NOT take a reactive dependency on rv$saved_permit_id.
  # The observeEvent further down writes to that slot every time the user
  # picks a permit; if we read it unisolated, this observer would re-fire on
  # every selection, calling updateSelectizeInput(..., server = TRUE) again
  # mid-typeahead — which re-initializes the server-side selectize widget
  # and breaks autocomplete. isolate() reads the saved value once at fire
  # time without subscribing to its changes. `selected` falls back to a
  # sensible default permit on first visit when no selection has been recorded.
  observe({
    req(current_page() == "select")
    pd <- permit_data_rv()
    req(!is.null(pd) && nrow(pd) > 0)
    choice_list <- setNames(pd$SourceID, pd$display_label)
    restored_id <- isolate(rv$saved_permit_id) %||% "PR0001031"
    session$onFlushed(function() {
      updateSelectizeInput(
        session,
        "permit_id",
        choices = choice_list,
        selected = restored_id,
        server = TRUE
      )
    })
  })

  # ── Capture search-page selections so they persist across navigation ──────
  # Each observer mirrors one search-page input into the corresponding rv$saved_*
  # slot. On a return visit, select_ui() and the selectize populater read from
  # rv$saved_* and re-apply the previous values. The observers ignore NULL
  # initial values, so the saved state is only updated when the user has
  # actually made a selection. ignoreInit = TRUE prevents capturing the
  # default values written by the UI re-render itself.
  observeEvent(
    input$permit_id,
    {
      if (!is.null(input$permit_id) && nzchar(input$permit_id)) {
        rv$saved_permit_id <- input$permit_id
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$date_start,
    {
      if (!is.null(input$date_start)) rv$saved_date_start <- input$date_start
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$date_end,
    {
      if (!is.null(input$date_end)) rv$saved_date_end <- input$date_end
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$npdes_forms,
    {
      rv$saved_npdes_forms <- input$npdes_forms
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(
    input$water_type_filter,
    {
      rv$saved_water_type <- input$water_type_filter
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # --- PAGE ROUTING ---
  output$page_content <- renderUI({
    switch(
      current_page(),
      "landing" = landing_ui(),
      "select" = select_ui(),
      "summary" = summary_ui(),
      "rp" = findings_ui(),
      "standalone" = standalone_ui()
    )
  })

  # --- UI PAGE DEFINITIONS ---
  landing_ui <- function() {
    div(
      class = "container",
      style = "margin-top: 50px; text-align: center;",
      br(),
      p(
        "This application is designed to aid permit writers in determining if reasonable potential exists for effluent discharging from NPDES permitted facilities to exceed published water quality standards. For detailed information on how to use this application, refer to the User Guide which can be accessed in the top right corner."
      ),
      br(),
      br(),
      h2("Select Analysis Workflow"),
      br(),
      fluidRow(
        column(
          6,
          wellPanel(
            icon("search", "fa-3x"),
            h4("Calculate RWC Using NPDES Data"),
            actionButton(
              "go_npdes",
              "Connect to ICIS-NPDES",
              class = "btn-primary"
            ),
            br(),
            br(),
            if (!is.null(db_last_updated)) {
              tagList(
                tags$small(
                  class = "text-muted",
                  icon("clock"),
                  sprintf(
                    " Data last updated: %s",
                    db_last_updated$run_timestamp
                  )
                ),
                br(),
                tags$small(
                  class = "text-muted",
                  sprintf(
                    "%s facilities | %s DMR records",
                    format(db_last_updated$n_facilities, big.mark = ","),
                    format(db_last_updated$n_dmr_records, big.mark = ",")
                  )
                )
              )
            } else {
              tags$small(class = "text-warning", "⚠ Database status unknown")
            },
            br(),
            p(
              "Choose this if you have the NPDES ID. You will have the option to add local data if needed."
            )
          )
        ),
        column(
          6,
          wellPanel(
            icon("file-upload", "fa-3x"),
            h4("Calculate RWC Using Local Data"),
            actionButton(
              "go_calc",
              "Standalone Calculator",
              class = "btn-primary"
            ),
            br(),
            br(),
            p("Choose this if no NPDES ID exists but you have data to use.")
          )
        )
      ),
      br(),
      br(),
      downloadButton(
        "download_user_guide",
        "Download User Guide PDF",
        icon = icon("file-pdf")
      )
    )
  }

  standalone_ui <- function() {
    sidebarLayout(
      sidebarPanel(
        h4("Standalone Analysis Settings"),

        # Use the same IDs as summary_ui so downstream code (coverage, RP) just works
        selectInput(
          "npdes_forms",
          "NPDES Forms",
          choices = sort(unique(npdes_forms$Form)),
          multiple = TRUE
        ),
        h5("Water Classifications"),
        selectInput(
          inputId = "water_type_filter",
          label = "Water Type",
          choices = c(
            "All" = "all",
            "Class SB" = "class SB waters",
            "Class SD" = "SD",
            "Class SG" = "SG",
            "Surface Waters" = "surface waters"
          ),
          selected = "all",
          multiple = TRUE
        ),
        hr(),
        # Reuse the same download id so existing downloadHandler works
        div(
          style = "margin-bottom: 8px;",
          downloadButton("download_dmr_template", "Download DMR Template")
        ),
        # Dedicated standalone upload input (separate from append flow)
        fileInput(
          "standalone_csv",
          "Upload Completed DMR Template",
          accept = ".csv"
        ),
        hr(),
        numericInput("rp_dr", "Dilution Ratio", value = 1, min = 1),
        numericInput(
          "confidence_level",
          "Confidence Level",
          value = 0.95,
          min = 0.5,
          max = 0.999,
          step = 0.01
        ),
        numericInput(
          "target_percentile",
          "Target Percentile (upper bound)",
          value = 0.95,
          min = 0.8,
          max = 0.999,
          step = 0.01
        ),
        hr(),
        # In summary_ui(), replace the Hardness Setting + pH/temp blocks with:

        tags$hr(),
        conditionalPanel(
          # Show only if Class SD is selected in Water Type multi-select
          condition = "Array.isArray(input.water_type_filter) && input.water_type_filter.indexOf('SD') >= 0",
          h5("Receiving Water Inputs (Class SD)"),
          # Hardness (numeric only; used for RP calculation)
          numericInput(
            inputId = "hardness_value",
            label = "Hardness (mg/L as CaCO3)",
            value = 100,
            min = 1,
            max = 1000,
            step = 1
          ),
          checkboxInput(
            inputId = "hardness_show_range",
            label = "Show hardness range analysis (plots only)",
            value = FALSE
          ),
          # pH and Temperature (numeric only; receiving water for TAN)
          numericInput(
            inputId = "ph_value",
            label = "Receiving Water pH (SU)",
            value = 6,
            min = 0,
            max = 14,
            step = 0.1
          ),
          numericInput(
            inputId = "temp_value",
            label = "Receiving Water Temperature (°C)",
            value = 30,
            min = 0,
            max = 50,
            step = 0.1
          )
        ),
        hr(),
        actionButton(
          "run_rp",
          "Run RP Analysis",
          class = "btn-success",
          style = "width:100%;"
        ),
        hr(),
        actionButton(
          "back_home_from_standalone",
          "← Back to Home",
          class = "btn-outline-secondary",
          style = "width:100%;"
        ),
      ),
      mainPanel(
        h4("Data Overview"),
        dataTableOutput("coverage_summary")
      )
    )
  }

  select_ui <- function() {
    # Read the saved search-page state ONCE at render time, without taking
    # a reactive dependency. Without isolate(), every write to rv$saved_*
    # (which happens on every input change) would re-fire the parent
    # renderUI({...}) — destroying the entire UI and rebuilding it mid-
    # interaction. That breaks the selectize typeahead and wipes the
    # default selection. isolate() lets us pick up the latest values when
    # the page is (re-)entered, without subscribing to subsequent changes.
    default_date_start <- isolate(rv$saved_date_start) %||%
      (Sys.Date() - years(5))
    default_date_end <- isolate(rv$saved_date_end) %||% Sys.Date()
    default_npdes <- isolate(rv$saved_npdes_forms) %||% character(0)
    default_water_type <- isolate(rv$saved_water_type) %||% "class SB waters"

    sidebarLayout(
      sidebarPanel(
        # Show retry UI when facility cache failed to load
        uiOutput("facility_load_status"),
        selectizeInput(
          "permit_id",
          "Search Permit ID or Facility Name",
          choices = "",
          options = list(placeholder = 'Type to search...')
        ),
        fluidRow(
          column(
            6,
            dateInput("date_start", "Start Date", value = default_date_start)
          ),
          column(6, dateInput("date_end", "End Date", value = default_date_end))
        ),
        hr(),
        actionButton("back_home", "Return Home"),
        hr(),
        tags$a(
          href = "https://www.epa.gov/sites/default/files/2014-12/documents/prwqs.pdf",
          target = "_blank",
          "Puerto Rico Water Quality Standards"
        )
      ),
      mainPanel(
        fluidRow(
          column(6, leafletOutput("map", height = "400px")),
          column(6, uiOutput("facility_details"))
        ),
        hr(),
        shinyjs::hidden(
          div(
            id = "filters_block",
            fluidRow(
              column(2, div(h4("Settings"))),
              column(
                10,
                fluidRow(
                  column(
                    6,
                    selectInput(
                      "npdes_forms",
                      "NPDES Forms",
                      choices = sort(unique(npdes_forms$Form)),
                      selected = default_npdes,
                      multiple = TRUE
                    )
                  ),
                  h5("Water Classifications"),
                  selectInput(
                    inputId = "water_type_filter",
                    label = "Water Type",
                    choices = c(
                      "All" = "all",
                      "Class SB" = "class SB waters",
                      "Class SD" = "SD",
                      "Class SG" = "SG",
                      "Surface Waters" = "surface waters"
                    ),
                    selected = default_water_type,
                    multiple = TRUE
                  )
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          actionButton(
            "fetch_dmr",
            "Fetch DMR Data",
            class = "btn-success",
            style = "width:100%;"
          )
        )
      )
    )
  }

  summary_ui <- function() {
    fluidPage(
      fluidRow(
        # ── Left panel: settings + run buttons ────────────────────────────────
        column(
          4,
          wellPanel(
            h4("Analysis Settings"),
            p(
              tags$b("NPDES Forms:"),
              paste(input$npdes_forms, collapse = ", ")
            ),
            numericInput("rp_dr", "Dilution Ratio", value = 1, min = 1),
            numericInput(
              "confidence_level",
              "Confidence Level",
              value = 0.95,
              min = 0.5,
              max = 0.999,
              step = 0.01
            ),
            numericInput(
              "target_percentile",
              "Target Percentile (upper bound)",
              value = 0.95,
              min = 0.8,
              max = 0.999,
              step = 0.01
            ),
            tags$hr(),
            conditionalPanel(
              condition = "Array.isArray(input.water_type_filter) && input.water_type_filter.indexOf('SD') >= 0",
              h5("Receiving Water Inputs (Class SD)"),
              numericInput(
                "hardness_value",
                "Hardness (mg/L as CaCO3)",
                value = 100,
                min = 1,
                max = 1000,
                step = 1
              ),
              checkboxInput(
                "hardness_show_range",
                "Show hardness range analysis (plots only)",
                value = FALSE
              ),
              numericInput(
                "ph_value",
                "Receiving Water pH (SU)",
                value = 6,
                min = 0,
                max = 14,
                step = 0.1
              ),
              numericInput(
                "temp_value",
                "Receiving Water Temperature (°C)",
                value = 30,
                min = 0,
                max = 50,
                step = 0.1
              )
            ),
            tags$hr(),
            downloadLink("downloadDmr", "Download Raw DMR"),
            tags$hr(),
            # Run RP — disabled until all flagged params are resolved
            uiOutput("run_rp_buttons_ui"),
            tags$hr(),
            h5("Append DMR Data"),
            div(
              style = "margin-bottom: 8px;",
              downloadButton("download_dmr_template", "Download DMR Template")
            ),
            fileInput(
              "append_dmr_csv",
              "Upload CSV to Append",
              accept = ".csv"
            ),
            actionButton(
              "back_to_select",
              "← Back to Search",
              class = "btn-outline-secondary",
              style = "width:100%;"
            )
          )
        ),

        # ── Right panel: tabbed data overview ─────────────────────────────────
        column(
          8,
          tabsetPanel(
            id = "summary_tabs",

            # Tab 1: Coverage Summary (existing)
            tabPanel(
              "Data Overview",
              br(),
              h4("Coverage Summary"),
              uiOutput("coverage_summary_header"),
              dataTableOutput("coverage_summary")
            ),

            # Tab 2: Parameters Needing Attention (conditional)
            tabPanel(
              "⚠ Needs Attention",
              br(),
              uiOutput("needs_attention_panel")
            ),

            # Tab 3: WQS Reference (read-only searchable crosswalk)
            tabPanel(
              "WQS Reference",
              br(),
              h4("Water Quality Standards Reference"),
              p(
                "Full crosswalk of all parameters with WQS criteria. Use this table to look up
                 criterion IDs, values, and water classes before making manual entries in the
                 Needs Attention tab."
              ),
              DTOutput("wqs_reference_table")
            )
          )
        )
      )
    )
  }

  findings_ui <- function() {
    fluidPage(
      # Bottom padding keeps the bottom bar off the viewport edge in
      # full-screen; compact table styling so 6 visible columns fit in col-5
      # (~1000 px on a 2560 px laptop) without horizontal scroll. scrollX
      # remains enabled as a safety valve for narrower windows.
      style = "padding-bottom: 18px;",
      tags$style(HTML(
        "
        #rp_table table.dataTable td { font-size: 12.5px; padding: 4px 8px; }
        #rp_table table.dataTable th { font-size: 12.5px; padding: 4px 8px; }
        #rp_table .form-control { font-size: 11.5px; height: 26px; padding: 2px 6px; }
        #findings_bottom_bar .alert { margin-bottom: 0; padding: 6px 12px; }
        "
      )),

      # ── Main panels first: table (5) beside plot (7) ────────────────────────
      # The old top utility bar left dead space in the upper middle and pushed
      # the content down; the controls now live in a slim bottom bar so the
      # table and plot start at the top of the page.
      fluidRow(
        column(
          5,
          DTOutput("rp_table")
        ),
        column(
          7,
          uiOutput("inspector_plot_container"),
          uiOutput("criterion_note") # rp_note callout; zero height when absent
        )
      ),

      # ── Bottom bar: Back | exceedance banner | Download ─────────────────────
      fluidRow(
        id = "findings_bottom_bar",
        style = "margin-top: 14px; align-items: center;",
        column(
          2,
          actionButton(
            "back_to_summary",
            "\u2190 Back to Settings",
            class = "btn-outline-secondary",
            style = "width:100%;"
          )
        ),
        column(7, uiOutput("finding_header")),
        column(
          3,
          div(
            style = "text-align: right;",
            downloadButton("download_report", "Download Report"),
            div(
              style = "display:inline-block; margin-left:10px; vertical-align:middle;",
              checkboxInput(
                "include_data",
                "Include Data in Download",
                value = FALSE
              )
            )
          )
        )
      )
    )
  }

  # --- LOGIC OBSERVERS ---

  observeEvent(input$back_to_start, {
    current_page("landing")
  })
  observeEvent(input$go_npdes, {
    current_page("select")
  })
  observeEvent(input$back_home, {
    current_page("landing")
  })
  observeEvent(input$back_to_select, {
    current_page("select")
  })
  observeEvent(input$go_calc, {
    current_page("standalone")
  })
  observeEvent(input$back_home_from_standalone, {
    current_page("landing")
  })

  # Reveal filters and fetch button after loading facility
  observeEvent(input$permit_id, {
    req(input$permit_id)
    facility_match <- permit_data_rv() %>%
      filter(SourceID == input$permit_id) %>%
      slice(1)
    if (nrow(facility_match) == 0) {
      showNotification(
        "Error: Facility not found in local cache.",
        type = "error"
      )
      shinyjs::hide("filters_block")
      shinyjs::hide("fetch_dmr")
      return()
    }
    rv$selected_facility <- facility_match
    shinyjs::show("filters_block")
    shinyjs::show("fetch_dmr")
  })

  # Active crosswalk filtered by selected NPDES forms
  active_crosswalk <- reactive({
    req(input$npdes_forms)
    crosswalk %>%
      filter(Form %in% input$npdes_forms)
  })

  # Map the user selection to the full set of detailed strings
  target_water_types <- reactive({
    req(input$water_type_filter)
    sel <- input$water_type_filter
    # If "all" is chosen (alone or with others), return all classes
    if ("all" %in% sel) {
      return(unique(crosswalk$USE_CLASS_NAME_LOCATION_ETC))
    }

    base <- character(0)
    map_one <- function(x) {
      switch(
        x,
        "SD" = c("class SD waters", "class SD waters- drinking water"),
        "SG" = c("class SG waters", "class SG waters- drinking water"),
        "class SB waters" = "class SB waters",
        "surface waters" = "surface waters",
        character(0)
      )
    }
    # "surface waters" carries the PRWQS Rule 1303.1 general standards, which
    # apply to ALL waters regardless of class — temperature, asbestos,
    # radium-226, strontium-90, gross beta. It is not a user-selectable class,
    # so it must always be appended or those criteria silently disappear
    # whenever a specific class is chosen.
    c(
      unname(unique(unlist(lapply(sel, map_one)))),
      "surface waters"
    ) %>%
      unique()
  })

  observe({
    req(current_page() == "standalone")
    req(input$npdes_forms)
    # Filter crosswalk based on chosen forms and water type
    crosswalk_filt <- crosswalk %>%
      dplyr::left_join(
        npdes_forms,
        by = c("NPDES_Pollutant" = "Pollutant"),
        relationship = "many-to-many"
      ) %>%
      # The form filter is display-scoped: it labels coverage rows with the
      # forms that include each pollutant. It does NOT gate which parameters
      # enter the analysis — every parameter with DMR data and a matching
      # WQS criterion is evaluated regardless of its form association.
      # pH (00400) and temperature (00010) used to be force-included here
      # regardless of class; that bypass has been removed so they are
      # evaluated against the selected class like every other parameter.
      dplyr::filter(Form %in% input$npdes_forms) %>%
      dplyr::filter(USE_CLASS_NAME_LOCATION_ETC %in% target_water_types())
    rv$crosswalk <- crosswalk_filt
  })

  observeEvent(input$standalone_csv, {
    req(input$standalone_csv)
    req(rv$crosswalk)

    # Read
    df <- readr::read_csv(input$standalone_csv$datapath, show_col_types = FALSE)

    # Required columns
    needed <- c(
      "parameter_code",
      "dmr_value_nmbr",
      "dmr_unit_desc",
      "monitoring_period_end_date",
      "perm_feature_nmbr"
    )
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) {
      showNotification(
        paste("Missing required columns:", paste(missing, collapse = ", ")),
        type = "error"
      )
      return()
    }

    # Standardize and coerce
    df <- df %>%
      dplyr::mutate(
        parameter_code = stringr::str_pad(
          as.character(parameter_code),
          5,
          pad = "0"
        ),
        dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
        monitoring_period_end_date = suppressWarnings(lubridate::mdy(
          monitoring_period_end_date
        )),
        perm_feature_nmbr = as.character(perm_feature_nmbr)
      )

    # Join expected WQS units and convert
    df_conv <- df %>%
      dplyr::left_join(
        rv$crosswalk %>%
          dplyr::select(parameter_code, UNIT_NAME) %>%
          dplyr::distinct(),
        by = "parameter_code"
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
        Conv_Flag = conv_data$flag,
        # Apply conversion. The value transform branches on Conv_Flag:
        #   NA / PASS       -> multiply by conv_data$mult (identity = *1)
        #   CONVERT_F_TO_C  -> apply (F-32)*5/9 offset
        #   EXCLUDED        -> retain as-is (mass-load / flow; dropped from RP)
        #   NO_WQS          -> retain as-is (no criterion in selected class)
        #   FAIL            -> retain as-is (genuine unit problem; flagged in
        #                      exported DMR but dropped from RP math)
        # Rows other than NA/PASS are kept in rv$dmr so they appear in the
        # downloadable DMR table; the RP-stage filter (~line 2354) drops
        # anything that isn’t NA or PASS before the RWC calculation.
        dmr_value_nmbr = dplyr::case_when(
          Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
          Conv_Flag %in% c("EXCLUDED", "NO_WQS", "FAIL") ~ dmr_value_nmbr,
          !is.na(conv_data$mult) ~ dmr_value_nmbr * conv_data$mult,
          TRUE ~ dmr_value_nmbr
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-conv_data, -UNIT_NAME)

    # ── Coalesce 82230 into 00610 (same logic as SQLite path) ───────────────
    if (any(AMMONIA_ALIAS_CODES %in% df_conv$parameter_code)) {
      has_00610 <- df_conv %>%
        dplyr::filter(parameter_code == "00610") %>%
        dplyr::distinct(perm_feature_nmbr, monitoring_period_end_date) %>%
        dplyr::mutate(has_canonical = TRUE)

      df_conv <- df_conv %>%
        dplyr::mutate(orig_parameter_code = parameter_code) %>%
        dplyr::left_join(
          has_00610,
          by = c("perm_feature_nmbr", "monitoring_period_end_date")
        ) %>%
        dplyr::filter(
          !parameter_code %in% AMMONIA_ALIAS_CODES |
            (parameter_code %in% AMMONIA_ALIAS_CODES & is.na(has_canonical))
        ) %>%
        dplyr::mutate(
          parameter_code = dplyr::if_else(
            parameter_code %in% AMMONIA_ALIAS_CODES,
            "00610",
            parameter_code
          )
        ) %>%
        dplyr::select(-has_canonical)
    } else {
      df_conv <- df_conv %>%
        dplyr::mutate(orig_parameter_code = parameter_code)
    }

    # ── Coalesce 00011 (temp °F) into 00010 (temp °C) ───────────────────────
    if ("00011" %in% df_conv$parameter_code) {
      has_00010 <- df_conv %>%
        dplyr::filter(parameter_code == "00010") %>%
        dplyr::distinct(perm_feature_nmbr, monitoring_period_end_date) %>%
        dplyr::mutate(has_celsius = TRUE)

      df_conv <- df_conv %>%
        dplyr::left_join(
          has_00010,
          by = c("perm_feature_nmbr", "monitoring_period_end_date")
        ) %>%
        dplyr::filter(
          !(parameter_code == "00010" &
            !is.na(has_celsius) &
            perm_feature_nmbr %in%
              (df_conv %>%
                dplyr::filter(parameter_code == "00011") %>%
                dplyr::pull(perm_feature_nmbr))) |
            parameter_code == "00011" |
            (!parameter_code %in% c("00010", "00011"))
        ) %>%
        dplyr::mutate(
          orig_parameter_code = dplyr::if_else(
            parameter_code == "00011",
            "00011",
            orig_parameter_code
          ),
          dmr_value_nmbr = dplyr::if_else(
            parameter_code == "00011",
            (dmr_value_nmbr - 32) * 5 / 9,
            dmr_value_nmbr
          ),
          dmr_unit_desc = dplyr::if_else(
            parameter_code == "00011",
            "deg c",
            dmr_unit_desc
          ),
          Conv_Flag = dplyr::if_else(
            # NEW: F->C handled here, clear stale FAIL
            parameter_code == "00011",
            "PASS",
            Conv_Flag
          ),
          parameter_code = dplyr::if_else(
            # keep LAST
            parameter_code == "00011",
            "00010",
            parameter_code
          )
        ) %>%
        dplyr::select(-has_celsius)
    }

    rv$dmr <- tag_statistic(df_conv, statistic_lookup)
    rv$dataset_source <- "Standalone"

    # Per-parameter unit-conversion status. Rolls the row-level Conv_Flag
    # counts into a single user-visible badge. EXCLUDED and NO WQS are
    # tracked separately from FAIL because they indicate different things:
    #   EXCLUDED — every row is a mass-load / flow unit (by design)
    #   NO WQS   — no WQS criterion exists for the selected water class
    #   FAIL/(Partial) — a real unit problem (genuine data-quality issue)
    # The ordering of branches matters: EXCLUDED first (whole-parameter), then
    # NO WQS (whole-parameter for the WQS-eligible subset), then FAIL variants.
    rv$parameter_status <- rv$dmr %>%
      dplyr::group_by(parameter_code) %>%
      dplyr::summarize(
        total_samples = dplyr::n(),
        excluded = sum(Conv_Flag == "EXCLUDED", na.rm = TRUE),
        no_wqs = sum(Conv_Flag == "NO_WQS", na.rm = TRUE),
        fails = sum(Conv_Flag == "FAIL", na.rm = TRUE),
        passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Samples available for WQS analysis: total minus rows we already
        # know cannot be compared to any concentration criterion.
        wqs_samples = total_samples - excluded,
        Unit_Status = dplyr::case_when(
          excluded == total_samples ~ "EXCLUDED", # all mass/flow
          no_wqs == wqs_samples ~ "NO WQS", # no criterion for class
          fails == wqs_samples ~ "FAIL", # all eligible failed
          fails > 0 ~ "FAIL (Partial)", # some failed
          passes > 0 ~ "PASS", # at least one converted
          TRUE ~ "MATCH" # exact unit match
        )
      )

    showNotification(
      "Standalone data loaded. Review summary and click Run RP.",
      type = "message"
    )
  })

  # Fetch DMR, apply basic cleaning, store for summary
  observeEvent(input$fetch_dmr, {
    req(
      rv$selected_facility,
      input$date_start,
      input$date_end,
      input$permit_id,
      input$npdes_forms
    )

    start_fmt <- format(input$date_start, "%m/%d/%Y")
    end_fmt <- format(input$date_end, "%m/%d/%Y")

    # Resolve selected water class strings
    wt_sel <- input$water_type_filter
    selected_water_classes <- if ("all" %in% wt_sel) {
      unique(crosswalk$USE_CLASS_NAME_LOCATION_ETC)
    } else {
      map_one_wt <- function(x) {
        switch(
          x,
          "SD" = c("class SD waters", "class SD waters- drinking water"),
          "SG" = c("class SG waters", "class SG waters- drinking water"),
          "class SB waters" = "class SB waters",
          "surface waters" = "surface waters",
          character(0)
        )
      }
      # surface waters is selectable; include it only when selected (see the
      # reactive helper above).
      unname(unique(unlist(lapply(wt_sel, map_one_wt))))
    }

    # ── crosswalk_full: water class only, no form filter ─────────────────────
    # Filtered only by the user's selected water classes (no form filter).
    # Drives the database query scope and the unmatched-parameter flagging.
    # pH (00400) and temperature (00010) used to be force-included here
    # regardless of class; that bypass has been removed so they are evaluated
    # against the selected class like every other parameter. If the user
    # picks a class for which pH or temperature has no WQS criterion, those
    # parameters will be surfaced on the Needs Attention tab.
    crosswalk_full_filt <- crosswalk %>%
      dplyr::filter(USE_CLASS_NAME_LOCATION_ETC %in% selected_water_classes)
    rv$crosswalk_full <- crosswalk_full_filt

    # ── crosswalk (form + water class): used for RP calc and coverage table ──
    crosswalk_filt <- crosswalk %>%
      dplyr::left_join(
        npdes_forms,
        by = c("NPDES_Pollutant" = "Pollutant"),
        relationship = "many-to-many"
      ) %>%
      dplyr::filter(Form %in% input$npdes_forms) %>%
      dplyr::filter(USE_CLASS_NAME_LOCATION_ETC %in% selected_water_classes)
    rv$crosswalk <- crosswalk_filt

    # ── form lookup: parameter_code -> "Form A & Form B" label ───────────────
    rv$form_lookup <- build_form_lookup(crosswalk_filt, input$npdes_forms)

    # Reset override/flagging state on each fresh fetch
    rv$flagged_params <- NULL
    rv$wqs_overrides <- NULL
    rv$quick_run_flag <- FALSE
    rv$quick_run_params <- character(0)

    withProgress(
      message = 'Loading DMR Records from Database...',
      value = 0.5,
      {
        dmr_raw <- tryCatch(
          {
            # Query SQLite instead of ECHO API — no rate limiting, no external dependency
            db_con <- DBI::dbConnect(
              RSQLite::SQLite(),
              SQLITE_PATH,
              flags = RSQLite::SQLITE_RO
            )
            on.exit(DBI::dbDisconnect(db_con), add = TRUE)

            raw <- DBI::dbGetQuery(
              db_con,
              sprintf(
                "
          SELECT
            permit_id                    AS permit_id,
            perm_feature_nmbr,
            perm_feature_type_code,
            monitoring_period_end_date,
            parameter_code,
            parameter_desc,
            dmr_value_nmbr,
            dmr_unit_code                AS unit_code,
            dmr_unit_desc,
            nodi_code,
            statistical_base_type_code,
            statistical_base_code,
            statistical_base_short_desc,
            limit_value_nmbr,
            limit_unit_code,
            limit_unit_desc,
            limit_set_designator
          FROM dmr_data
          WHERE permit_id = '%s'
          AND monitoring_period_end_date >= '%s'
          AND monitoring_period_end_date <= '%s'
        ",
                input$permit_id,
                format(input$date_start, "%Y-%m-%d"),
                format(input$date_end, "%Y-%m-%d")
              )
            ) %>%
              # Standardise types
              dplyr::mutate(
                parameter_code = stringr::str_pad(
                  as.character(parameter_code),
                  5,
                  "left",
                  "0"
                ),
                dmr_value_nmbr = as.numeric(dmr_value_nmbr),
                dmr_unit_desc = as.character(dmr_unit_desc),
                monitoring_period_end_date = as.Date(monitoring_period_end_date)
              ) %>%
              # Match ECHO column names the rest of the app expects
              dplyr::rename(
                perm_feature_nmbr = perm_feature_nmbr
              ) %>%
              # Statistic tagging replaces the old MAX-only filter. Which
              # statistics survive is now driven by the crosswalk's `statistic`
              # column, so extraction and comparison cannot disagree — DO keeps
              # its MIN records because a crosswalk row asks for them, and
              # Enterococci keeps its geometric means.
              #
              # Do NOT filter on crosswalk_full_filt$parameter_code here —
              # parameters with no WQS match (e.g. PCBs, code 39516) must still
              # reach rv$dmr so flag_unmatched_params can surface them in the
              # Needs Attention tab. The crosswalk filter only gates the RP calc,
              # not the data fetch.
              tag_statistic(lookup = statistic_lookup) %>%
              # Drop rows with no outfall identifier — these arise when the
              # ICIS-NPDES join produces a NULL perm_feature_nmbr (e.g. data
              # submitted against a limit set that has no valid perm_feature
              # record). They carry no DMR observations and would otherwise
              # appear as an "NA" entry in the outfall dropdown.
              dplyr::filter(!is.na(perm_feature_nmbr))

            # Retain only statistics the crosswalk actually asks for. Rows for
            # parameters with NO criterion at all are kept regardless, so
            # Needs Attention can still surface them (this is why BOD5 used to
            # vanish entirely rather than being flagged).
            req_stats <- crosswalk_full_filt %>%
              dplyr::filter(!is.na(statistic), nzchar(statistic)) %>%
              dplyr::distinct(parameter_code, statistic) %>%
              dplyr::mutate(.required = TRUE)

            raw <- raw %>%
              dplyr::left_join(
                req_stats,
                by = c("parameter_code", "statistic")
              ) %>%
              dplyr::filter(
                !is.na(.required) |
                  !parameter_code %in% req_stats$parameter_code
              ) %>%
              dplyr::select(-.required)

            # (1) Dominant unit per parameter
            dom_units <- raw %>%
              dplyr::filter(!is.na(dmr_unit_desc), dmr_unit_desc != "") %>%
              dplyr::group_by(parameter_code, dmr_unit_desc) %>%
              dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
              dplyr::slice_max(n, with_ties = FALSE) %>%
              dplyr::ungroup() %>%
              dplyr::select(parameter_code, dominant_unit = dmr_unit_desc)

            # (2) Flag parameters where ALL rows have missing units
            all_missing_tbl <- raw %>%
              dplyr::group_by(parameter_code) %>%
              dplyr::summarise(
                all_units_missing = all(
                  is.na(dmr_unit_desc) | dmr_unit_desc == ""
                ),
                .groups = "drop"
              )

            # (3) WQS unit hint for fallback
            unit_hint <- crosswalk_full_filt %>%
              dplyr::distinct(parameter_code, UNIT_NAME)

            raw %>%
              dplyr::left_join(dom_units, by = "parameter_code") %>%
              dplyr::left_join(all_missing_tbl, by = "parameter_code") %>%
              dplyr::left_join(unit_hint, by = "parameter_code") %>%
              dplyr::mutate(
                dmr_value_nmbr = ifelse(
                  nodi_code %in% c("B", "Q"),
                  0,
                  dmr_value_nmbr
                ),
                dmr_unit_desc = dplyr::case_when(
                  all_units_missing &
                    (is.na(dmr_unit_desc) | dmr_unit_desc == "") ~ UNIT_NAME,
                  nodi_code %in%
                    c("B", "Q") &
                    (is.na(dmr_unit_desc) | dmr_unit_desc == "") &
                    !is.na(dominant_unit) ~ dominant_unit,
                  TRUE ~ dmr_unit_desc
                )
              ) %>%
              dplyr::select(-dominant_unit, -all_units_missing, -UNIT_NAME) %>%
              tidyr::drop_na(dmr_value_nmbr)
          },
          error = function(e) {
            message("SQLite DMR query error: ", e$message)
            NULL
          }
        )

        cat("DMR load successful!!!")

        if (!is.null(dmr_raw) && nrow(dmr_raw) > 0) {
          rv$dmr <- dmr_raw %>%
            # Standardize codes first
            mutate(parameter_code = str_pad(parameter_code, 5, "left", "0")) %>%
            # Join with crosswalk to see what the WQS unit should be
            left_join(
              crosswalk %>% select(parameter_code, UNIT_NAME) %>% distinct(),
              by = "parameter_code"
            ) %>%
            rowwise() %>%
            mutate(
              conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
              Conv_Flag = conv_data$flag,
              # Apply conversion. See standalone path above for the full
              # comment; behaviour here is identical. Rows that aren’t NA
              # or PASS are kept in rv$dmr for the exported DMR table and
              # filtered out of the RP math at ~line 2354.
              dmr_value_nmbr = case_when(
                Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
                Conv_Flag %in% c("EXCLUDED", "NO_WQS", "FAIL") ~ dmr_value_nmbr,
                !is.na(conv_data$mult) ~ dmr_value_nmbr * conv_data$mult,
                TRUE ~ dmr_value_nmbr
              )
            ) %>%
            ungroup() %>%
            select(-conv_data, -UNIT_NAME) # Clean up helper columns

          # ── Coalesce 82230 (Ammonia & ammonium total) into 00610 (TAN) ──────
          # 82230 and 00610 capture the same measurement. Prefer 00610 when both
          # are present for the same outfall/period; recode 82230 to 00610 when
          # it is the only ammonia code so it matches the crosswalk (criterion
          # 79613). Original code preserved in orig_parameter_code.
          if (any(AMMONIA_ALIAS_CODES %in% rv$dmr$parameter_code)) {
            has_00610 <- rv$dmr %>%
              dplyr::filter(parameter_code == "00610") %>%
              dplyr::distinct(perm_feature_nmbr, monitoring_period_end_date) %>%
              dplyr::mutate(has_canonical = TRUE)

            rv$dmr <- rv$dmr %>%
              dplyr::mutate(orig_parameter_code = parameter_code) %>%
              dplyr::left_join(
                has_00610,
                by = c("perm_feature_nmbr", "monitoring_period_end_date")
              ) %>%
              dplyr::filter(
                !parameter_code %in% AMMONIA_ALIAS_CODES |
                  (parameter_code %in%
                    AMMONIA_ALIAS_CODES &
                    is.na(has_canonical))
              ) %>%
              dplyr::mutate(
                parameter_code = dplyr::if_else(
                  parameter_code %in% AMMONIA_ALIAS_CODES,
                  "00610",
                  parameter_code
                ),
                parameter_desc = dplyr::if_else(
                  orig_parameter_code %in% AMMONIA_ALIAS_CODES,
                  "Nitrogen, ammonia total [as N]",
                  parameter_desc
                )
              ) %>%
              dplyr::select(-has_canonical)
          } else {
            rv$dmr <- rv$dmr %>%
              dplyr::mutate(orig_parameter_code = parameter_code)
          }

          # ── Coalesce 00011 (temp °F) into 00010 (temp °C) ───────────────────
          # Some permittees report temperature in both °F (00011) and °C (00010)
          # for the same outfall and period. When both are present, prefer °F —
          # convert it to °C and drop the °C record to avoid duplication. When
          # only °F is present, convert and recode to 00010 so it reaches the
          # temperature findings logic. Original code preserved in
          # orig_parameter_code (set above; update only for 00011 rows here).
          if ("00011" %in% rv$dmr$parameter_code) {
            has_00010 <- rv$dmr %>%
              dplyr::filter(parameter_code == "00010") %>%
              dplyr::distinct(perm_feature_nmbr, monitoring_period_end_date) %>%
              dplyr::mutate(has_celsius = TRUE)

            rv$dmr <- rv$dmr %>%
              dplyr::left_join(
                has_00010,
                by = c("perm_feature_nmbr", "monitoring_period_end_date")
              ) %>%
              dplyr::filter(
                # Drop °C rows where °F exists for the same outfall/period
                !(parameter_code == "00010" &
                  !is.na(has_celsius) &
                  perm_feature_nmbr %in%
                    (rv$dmr %>%
                      dplyr::filter(parameter_code == "00011") %>%
                      dplyr::pull(perm_feature_nmbr))) |
                  parameter_code == "00011" |
                  (!parameter_code %in% c("00010", "00011"))
              ) %>%
              dplyr::mutate(
                orig_parameter_code = dplyr::if_else(
                  parameter_code == "00011",
                  "00011",
                  orig_parameter_code
                ),
                dmr_value_nmbr = dplyr::if_else(
                  parameter_code == "00011",
                  (dmr_value_nmbr - 32) * 5 / 9,
                  dmr_value_nmbr
                ),
                dmr_unit_desc = dplyr::if_else(
                  parameter_code == "00011",
                  "deg c",
                  dmr_unit_desc
                ),
                # Rewrite the description too. Leaving the Fahrenheit desc in
                # place gives 00010 two parameter_desc values, which fans out
                # joins keyed on parameter_code (see flag_unmatched_params).
                parameter_desc = dplyr::if_else(
                  parameter_code == "00011",
                  "Temperature, water deg. centigrade",
                  parameter_desc
                ),
                Conv_Flag = dplyr::if_else(
                  # NEW: F->C handled here, clear stale FAIL
                  parameter_code == "00011",
                  "PASS",
                  Conv_Flag
                ),
                parameter_code = dplyr::if_else(
                  # keep LAST
                  parameter_code == "00011",
                  "00010",
                  parameter_code
                )
              ) %>%
              dplyr::select(-has_celsius)
          }

          # Re-apply the required-statistic filter AFTER the 82230->00610 and
          # 00011->00010 recodes. Those source codes have no crosswalk row, so
          # the fetch-time filter kept ALL their statistics; once recoded onto
          # a parameter that DOES declare a required statistic, the surplus
          # records (e.g. monthly-average temperature) would otherwise leak
          # into the RP calculation.
          if (
            !is.null(rv$dmr) &&
              nrow(rv$dmr) > 0 &&
              "statistic" %in% names(rv$dmr)
          ) {
            req_stats_post <- rv$crosswalk_full %>%
              dplyr::filter(!is.na(statistic), nzchar(statistic)) %>%
              dplyr::distinct(parameter_code, statistic) %>%
              dplyr::mutate(.required = TRUE)

            rv$dmr <- rv$dmr %>%
              dplyr::left_join(
                req_stats_post,
                by = c("parameter_code", "statistic")
              ) %>%
              dplyr::filter(
                !is.na(.required) |
                  !parameter_code %in% req_stats_post$parameter_code
              ) %>%
              dplyr::select(-.required)
          }

          cat(paste0(
            "\nDMR has ",
            nrow(rv$dmr),
            " samples across ",
            length(unique(rv$dmr$parameter_code)),
            " parameters"
          ))

          # Per-parameter unit-conversion status. See the standalone-path
          # version above for the full comment; logic is identical.
          rv$parameter_status <- rv$dmr %>%
            group_by(parameter_code) %>%
            summarize(
              total_samples = n(),
              excluded = sum(Conv_Flag == "EXCLUDED", na.rm = TRUE),
              no_wqs = sum(Conv_Flag == "NO_WQS", na.rm = TRUE),
              fails = sum(Conv_Flag == "FAIL", na.rm = TRUE),
              passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              wqs_samples = total_samples - excluded,
              Unit_Status = case_when(
                excluded == total_samples ~ "EXCLUDED",
                no_wqs == wqs_samples ~ "NO WQS",
                fails == wqs_samples ~ "FAIL",
                fails > 0 ~ "FAIL (Partial)",
                passes > 0 ~ "PASS",
                TRUE ~ "MATCH"
              ),
              # Colors are referenced by the Coverage Summary renderer to
              # style the Unit_Status cell. NO WQS uses a neutral blue-grey
              # so it reads as informational, not error.
              status_color = case_when(
                Unit_Status == "EXCLUDED" ~ "grey",
                Unit_Status == "NO WQS" ~ "lightblue",
                Unit_Status == "FAIL" ~ "red",
                Unit_Status == "FAIL (Partial)" ~ "orange",
                Unit_Status == "PASS" ~ "yellow",
                Unit_Status == "MATCH" ~ "green"
              )
            )

          # ── Flag parameters with DMR data but no usable WQS for selected class ──
          rv$flagged_params <- flag_unmatched_params(
            dmr = rv$dmr,
            crosswalk_full = rv$crosswalk_full,
            crosswalk_all = crosswalk,
            dmr_parameters = dmr_parameters_lookup,
            statistic_lookup = statistic_lookup
          )

          current_page("summary")
        } else {
          showNotification(
            "No DMR records found for the selected date range.",
            type = "warning"
          )
        }
      }
    )
  })

  output$downloadDmr <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(rv$dmr)
      write.csv(rv$dmr, file)
    }
  )

  output$download_user_guide <- downloadHandler(
    filename = function() "RP_Calculator_User_Guide.pdf",
    content = function(file) {
      file.copy("www/RPA_User_Guide.pdf", file)
    },
    contentType = "application/pdf"
  )

  output$download_dmr_template <- downloadHandler(
    filename = function() {
      id <- input$permit_id %||% "permit"
      paste0(
        "dmr_append_template_",
        id,
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      req(rv$crosswalk)
      tmpl <- rv$crosswalk %>%
        dplyr::distinct(
          parameter_code,
          parameter_desc = POLLUTANT_NAME,
          Expected_Unit = UNIT_NAME
        ) %>%
        dplyr::mutate(
          dmr_value_nmbr = NA_real_,
          dmr_unit_desc = Expected_Unit,
          monitoring_period_end_date = "MM-DD-YYYY" # user fills (YYYY-MM-DD)
        ) %>%
        dplyr::select(
          parameter_code,
          parameter_desc,
          dmr_value_nmbr,
          dmr_unit_desc,
          monitoring_period_end_date,
          Expected_Unit
        )

      readr::write_csv(tmpl, file)
    }
  )

  observeEvent(input$append_dmr_csv, {
    req(input$append_dmr_csv)
    add_df <- readr::read_csv(
      input$append_dmr_csv$datapath,
      show_col_types = FALSE
    )

    # Required columns
    needed <- c(
      "parameter_code",
      "dmr_value_nmbr",
      "dmr_unit_desc",
      "monitoring_period_end_date"
    )
    missing <- setdiff(needed, names(add_df))
    if (length(missing) > 0) {
      showNotification(
        paste("Missing required columns:", paste(missing, collapse = ", ")),
        type = "error"
      )
      return()
    }

    add_df <- add_df %>%
      dplyr::mutate(
        parameter_code = stringr::str_pad(
          as.character(parameter_code),
          5,
          pad = "0"
        ),
        # keep units as given; users are responsible for correctness
        dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
        # try ISO parse; if it fails, keep as character
        monitoring_period_end_date = suppressWarnings(lubridate::mdy(
          monitoring_period_end_date
        ))
      )

    # Optional: warn if units don’t match expected UNIT_NAME
    if (!is.null(rv$crosswalk)) {
      exp_units <- rv$crosswalk %>%
        dplyr::distinct(parameter_code, Expected_Unit = UNIT_NAME)
      chk <- add_df %>%
        dplyr::left_join(exp_units, by = "parameter_code") %>%
        dplyr::filter(
          !is.na(Expected_Unit) &
            !is.na(dmr_unit_desc) &
            dmr_unit_desc != Expected_Unit
        )
      if (nrow(chk) > 0) {
        warn_codes <- paste(unique(chk$parameter_code), collapse = ", ")
        showNotification(
          paste0(
            "Unit mismatch for parameter_code(s): ",
            warn_codes,
            ". Expected vs provided units differ. Proceeding to append as-is."
          ),
          type = "warning",
          duration = 8
        )
      }
    }

    # Minimal columns your app expects elsewhere (fill if missing)
    add_df <- add_df %>%
      dplyr::mutate(
        # Source tag: lets coverage_tbl_data() and the report distinguish
        # user-appended rows from ECHO/SQLite base data ("Basis for
        # Inclusion" column + \u2021 footnote in the Included Pollutants table).
        dataset_source = "Manually Added",
        npdes_id = input$permit_id %||% NA_character_,
        perm_feature_nmbr = perm_feature_nmbr %||% NA_character_,
        perm_feature_type_code = perm_feature_type_code %||% "EXO",
        statistical_base_type_code = statistical_base_type_code %||% "MAX",
        value_type_desc = value_type_desc %||% "Concentration3",
        parameter_desc = if (!"parameter_desc" %in% names(add_df)) {
          NA_character_
        } else {
          parameter_desc
        }
      )

    # Append
    rv$dmr <- dplyr::bind_rows(
      rv$dmr,
      tag_statistic(add_df, statistic_lookup)
    )
    showNotification(
      paste0("Appended ", nrow(add_df), " rows to DMR."),
      type = "message"
    )
  })

  # Manual upload
  observeEvent(input$standalone_upload, {
    showModal(modalDialog(
      fileInput("manual_file", "Choose CSV File"),
      footer = modalButton("Cancel")
    ))
  })

  observeEvent(input$manual_file, {
    req(input$manual_file)
    uploaded_file <- read_csv(
      input$manual_file$datapath,
      show_col_types = FALSE
    ) %>%
      mutate(
        monitoring_period_end_date = mdy(monitoring_period_end_date),
        dataset_source = "Manual Upload"
      )

    rv$dmr <- bind_rows(rv$dmr, tag_statistic(uploaded_file, statistic_lookup))
    removeModal()
    current_page("summary")
  })

  # --- OUTPUTS: Summary page
  output$map <- renderLeaflet({
    req(rv$selected_facility)
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addMarkers(
        lng = rv$selected_facility$FacLong,
        lat = rv$selected_facility$FacLat
      ) %>%
      # 2. Add the control to toggle between them
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        options = layersControlOptions(collapsed = FALSE) # Keeps the menu open by default
      )
  })

  # Add more robust facility details

  output$facility_details <- renderUI({
    req(rv$selected_facility)
    f <- rv$selected_facility

    # Map permit status codes to human-readable labels
    status_labels <- c(
      "EFF" = "Effective",
      "ADC" = "Administratively Continued",
      "PND" = "Pending",
      "TRM" = "Terminated",
      "RET" = "Retired",
      "NON" = "Non-Permitted",
      "UNK" = "Unknown"
    )

    # Column names are lowercased by refresh_db.R before writing to SQLite;
    # four are renamed on load (permit_id -> SourceID, etc.) — the rest stay lowercase.
    status_code <- f$permit_status_code %||% "—"
    status_text <- status_labels[status_code] %||% status_code

    # permit_status_date was replaced with effective_date after checking the
    # actual ICIS_PERMIT schema — guard against older databases that may not
    # have the column yet.
    status_date_text <- tryCatch(
      {
        raw <- if ("effective_date" %in% names(f)) f$effective_date else NA
        d <- suppressWarnings(as.Date(raw))
        if (!is.na(d)) format(d, "%B %d, %Y") else "—"
      },
      error = function(e) "—"
    )

    permit_id <- f$SourceID %||% "—"
    fac_name <- f$CWPName %||% "—"
    city <- f$city %||% "—"
    state <- f$state_code %||% "—"
    echo_url <- paste0(
      "https://echo.epa.gov/detailed-facility-report?fid=",
      permit_id,
      "&sys=NPDES"
    )

    tagList(
      h4("Facility Details"),
      tags$table(
        style = "line-height:1.8; font-size:14px;",
        tags$tr(tags$td(tags$b("Permit #:")), tags$td(permit_id)),
        tags$tr(tags$td(tags$b("Facility Name:")), tags$td(fac_name)),
        tags$tr(tags$td(tags$b("City:")), tags$td(city)),
        tags$tr(tags$td(tags$b("State:")), tags$td(state)),
        tags$tr(tags$td(tags$b("Last Status:")), tags$td(status_text)),
        tags$tr(tags$td(tags$b("Effective Date:")), tags$td(status_date_text))
      ),
      br(),
      tags$a(
        href = echo_url,
        target = "_blank",
        "Click here to open ECHO page"
      )
    )
  })

  # Coverage summary header — static description + last monitoring period date
  output$coverage_summary_header <- renderUI({
    base_text <- "All parameters for which DMR data was retrieved. Parameters not associated
                  with the selected NPDES forms are included if DMR data exists; the Form
                  column indicates their association."

    # Query the full database for this permit's latest monitoring period —
    # independent of the user's selected date range — so the date reflects
    # when data was last available in ICIS, not just the queried window.
    last_date_text <- tryCatch(
      {
        req(input$permit_id)
        db_con <- DBI::dbConnect(
          RSQLite::SQLite(),
          SQLITE_PATH,
          flags = RSQLite::SQLITE_RO
        )
        on.exit(DBI::dbDisconnect(db_con), add = TRUE)
        row <- DBI::dbGetQuery(
          db_con,
          sprintf(
            "SELECT MAX(monitoring_period_end_date) AS max_date
           FROM dmr_data
          WHERE permit_id = '%s'",
            input$permit_id
          )
        )
        max_dt <- suppressWarnings(as.Date(row$max_date[1]))
        if (!is.na(max_dt)) {
          sprintf(
            " Most recent monitoring period available in database: %s.",
            format(max_dt, "%B %d, %Y")
          )
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )

    tagList(
      p(base_text, if (!is.null(last_date_text)) tags$b(last_date_text))
    )
  })

  # Define the coverage table as a reactive expression
  coverage_tbl_data <- reactive({
    req(rv$dmr, rv$crosswalk, input$npdes_forms)

    # ── 1. Build pollutant list from the FULL water-class-filtered crosswalk
    #       (not just form-filtered) so non-form parameters appear too.
    #       Form association comes from rv$form_lookup.
    all_params_in_dmr <- rv$dmr %>%
      dplyr::distinct(parameter_code)

    crosswalk_base <- rv$crosswalk_full %||% rv$crosswalk

    # Manual WQS entries (decision == "include") aren't in the base crosswalk —
    # they exist precisely because no WQS row matched the selected class. Bind
    # them in here so they appear in the coverage table (and get the report's
    # dagger flag), mirroring the bind into crosswalk_effective at Run RP time.
    manual_rows_cov <- build_manual_crosswalk_rows(
      rv$wqs_overrides,
      # Unfiltered crosswalk: a manual entry exists because the parameter has
      # no criterion in the selected class, so its method profile (e.g.
      # temperature = direct) is only found outside the class filter.
      base_crosswalk = crosswalk
    )
    if (!is.null(manual_rows_cov) && nrow(manual_rows_cov) > 0) {
      crosswalk_base <- dplyr::bind_rows(
        crosswalk_base,
        manual_rows_cov %>%
          dplyr::mutate(
            NPDES_Pollutant = dplyr::if_else(
              parameter_code == "00010",
              "Temperature",
              NPDES_Pollutant
            )
          ) %>%
          dplyr::distinct(NPDES_Pollutant, parameter_code)
      )
    }

    param_pollutant <- crosswalk_base %>%
      dplyr::distinct(NPDES_Pollutant, parameter_code) %>%
      dplyr::filter(
        parameter_code %in%
          all_params_in_dmr$parameter_code |
          parameter_code %in% rv$crosswalk$parameter_code
      ) %>%
      # Fall back to dmr_parameters_lookup name when NPDES_Pollutant is blank.
      # This covers parameters that have a WQS match but were never assigned an
      # NPDES pollutant name because they aren't in the forms crosswalk.
      dplyr::left_join(dmr_parameters_lookup, by = "parameter_code") %>%
      dplyr::mutate(
        NPDES_Pollutant = dplyr::coalesce(NPDES_Pollutant, parameter_desc)
      ) %>%
      dplyr::select(-parameter_desc)

    # Attach form association via form_lookup
    form_lkp <- rv$form_lookup
    param_pollutant <- param_pollutant %>%
      dplyr::left_join(form_lkp, by = "parameter_code") %>%
      dplyr::mutate(
        Form = dplyr::coalesce(associated_forms, "Not in selected forms")
      ) %>%
      dplyr::select(-associated_forms) %>%
      dplyr::mutate(
        param_status = dplyr::if_else(
          is.na(parameter_code),
          "No Ref",
          parameter_code
        )
      ) %>%
      dplyr::select(
        Pollutant = NPDES_Pollutant,
        Form,
        `Ref Param` = param_status
      ) %>%
      dplyr::distinct()

    # ── 2. Sample counts — split pH by stat type to avoid double-counting ──
    # pH (minimum) and pH (maximum) are separate rows in ph_dmr keyed by
    # NPDES_Pollutant. All other parameters count rows from rv$dmr.
    params_by_code <- rv$dmr %>%
      dplyr::filter(parameter_code != "00400") %>%
      dplyr::group_by(parameter_code) %>%
      dplyr::summarise(`# Samples` = dplyr::n(), .groups = "drop")

    # pH counts from direct_dmr (built at Run RP time; NULL before first run)
    ph_counts <- if (!is.null(rv$ph_dmr) && nrow(rv$ph_dmr) > 0) {
      rv$ph_dmr %>%
        dplyr::group_by(NPDES_Pollutant) %>%
        dplyr::summarise(`# Samples` = dplyr::n(), .groups = "drop") %>%
        dplyr::rename(Pollutant = NPDES_Pollutant)
    } else {
      # Before Run RP, split raw counts evenly between min/max
      ph_raw_n <- rv$dmr %>%
        dplyr::filter(parameter_code == "00400") %>%
        dplyr::group_by(statistical_base_type_code) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      dplyr::tibble(
        Pollutant = c("pH (maximum)", "pH (minimum)"),
        `# Samples` = c(
          dplyr::coalesce(
            ph_raw_n$n[ph_raw_n$statistical_base_type_code == "MAX"],
            0L
          ),
          dplyr::coalesce(
            ph_raw_n$n[ph_raw_n$statistical_base_type_code == "MIN"],
            0L
          )
        )
      )
    }

    # ── 3. Join counts to pollutant list ──────────────────────────────────────
    out <- param_pollutant %>%
      dplyr::left_join(
        params_by_code,
        by = c("Ref Param" = "parameter_code")
      ) %>%
      # Overwrite pH counts with correctly split values
      dplyr::left_join(ph_counts, by = "Pollutant") %>%
      dplyr::mutate(
        `# Samples` = dplyr::coalesce(`# Samples.y`, `# Samples.x`),
        `# Samples` = tidyr::replace_na(`# Samples`, 0L)
      ) %>%
      dplyr::select(-dplyr::any_of(c("# Samples.x", "# Samples.y")))

    # ── 4. Manually-added-data flag per parameter ────────────────────────────
    # User-supplied rows carry a dataset_source tag ("Manually Added" from the
    # Append DMR upload; "Manual Upload" from the standalone whole-dataset
    # upload); ECHO/SQLite base rows carry NA. Values written to `User Data`:
    #   ""        no user-supplied rows
    #   "partial" some rows user-supplied, some from the base record
    #   "all"     every row for this parameter is user-supplied
    # The report maps "all" (for non-form parameters) to a "Manually Added"
    # basis and flags "partial"/form-associated cases with a \u2021 footnote.
    # Suppressed entirely when EVERY row in rv$dmr is user-supplied (the
    # standalone workflow), where the marker would flag every single row.
    user_by_code <- NULL
    if ("dataset_source" %in% names(rv$dmr)) {
      src_tab <- rv$dmr %>%
        dplyr::group_by(parameter_code) %>%
        dplyr::summarise(
          n_user = sum(!is.na(dataset_source) & nzchar(dataset_source)),
          n_tot = dplyr::n(),
          .groups = "drop"
        )
      if (sum(src_tab$n_user) < sum(src_tab$n_tot)) {
        user_by_code <- src_tab
      }
    }

    if (!is.null(user_by_code)) {
      out <- out %>%
        dplyr::left_join(
          user_by_code %>%
            dplyr::transmute(
              parameter_code,
              `User Data` = dplyr::case_when(
                n_user == 0L ~ "",
                n_user == n_tot ~ "all",
                TRUE ~ "partial"
              )
            ),
          by = c("Ref Param" = "parameter_code")
        ) %>%
        dplyr::mutate(`User Data` = tidyr::replace_na(`User Data`, ""))
    } else {
      out <- out %>% dplyr::mutate(`User Data` = "")
    }

    out
  })

  output$coverage_summary <- renderDT({
    req(rv$parameter_status, coverage_tbl_data())

    # Use the reactive data frame we just defined
    df_display <- coverage_tbl_data() %>%
      left_join(
        rv$parameter_status %>% select(parameter_code, Unit_Status),
        by = c("Ref Param" = "parameter_code")
      ) %>%
      mutate(Unit_Status = coalesce(Unit_Status, "NO DATA")) %>%
      arrange(desc(`# Samples`)) %>%
      # Form and User Data feed the report's Basis-for-Inclusion column and
      # footnotes; neither belongs in the on-screen Data Overview.
      select(!dplyr::any_of(c("Form", "User Data"))) %>%
      distinct()

    # Coverage Summary renderer. Color-code the Unit_Status cell so the user
    # can scan the table quickly:
    #   MATCH          — green     (units identical, no conversion)
    #   PASS           — yellow    (converted successfully)
    #   FAIL (Partial) — orange    (some convertible, some unrecognized)
    #   FAIL           — red       (no convertible records; check source)
    #   EXCLUDED       — grey      (mass-load / flow; not comparable)
    #   NO WQS         — blue-grey (no criterion in selected class — see
    #                               Needs Attention tab for the specific reason)
    #   NO DATA        — light grey (in crosswalk but no DMR records)
    datatable(df_display, options = list(dom = 't', pageLength = -1)) %>%
      formatStyle(
        'Unit_Status',
        backgroundColor = styleEqual(
          c(
            "MATCH",
            "PASS",
            "FAIL (Partial)",
            "FAIL",
            "EXCLUDED",
            "NO WQS",
            "NO DATA"
          ),
          c(
            "#ccffcc",
            "#ffffcc",
            "#ffe5cc",
            "#ffcccc",
            "#e0e0e0",
            "#cce6ff",
            "#f5f5f5"
          )
        ),
        color = styleEqual(
          c(
            "MATCH",
            "PASS",
            "FAIL (Partial)",
            "FAIL",
            "EXCLUDED",
            "NO WQS",
            "NO DATA"
          ),
          c(
            "#006600",
            "#999900",
            "#994c00",
            "#990000",
            "#555555",
            "#003366",
            "#888888"
          )
        ),
        fontWeight = 'bold'
      )
  })

  # ── WQS Reference table (static, full crosswalk) ─────────────────────────
  output$wqs_reference_table <- DT::renderDT({
    ref <- crosswalk %>%
      dplyr::left_join(
        dmr_parameters_lookup,
        by = "parameter_code"
      ) %>%
      dplyr::mutate(
        Pollutant = dplyr::coalesce(
          NPDES_Pollutant,
          parameter_desc,
          parameter_code
        )
      ) %>%
      dplyr::mutate(
        # Combine base class + sub_class so the "Water Class" cell reads
        # "class SD waters — streams" where the sub-class applies and falls
        # back to the base class otherwise. Sub_class is a future-proofed
        # column; today only PR SD nitrogen/phosphorus/selenium populate it.
        `Water Class` = format_water_class(
          USE_CLASS_NAME_LOCATION_ETC,
          if ("sub_class" %in% names(.)) sub_class else NA_character_
        )
      ) %>%
      dplyr::select(
        `Parameter Code` = parameter_code,
        `Pollutant` = Pollutant,
        `Water Class`,
        `Criterion ID` = CRITERION_ID,
        `Criterion Value` = CRITERION_VALUE,
        `Unit` = UNIT_NAME,
        `Criterion Type` = dplyr::any_of("CRITERIATYPEAQUAHUMHLTH")
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(`Parameter Code`)
    DT::datatable(
      ref,
      filter = "top",
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ── Run RP button UI (enabled/disabled based on review panel state) ────────
  output$run_rp_buttons_ui <- renderUI({
    fp <- rv$flagged_params
    ov <- rv$wqs_overrides
    n_flagged <- if (!is.null(fp)) nrow(fp) else 0L
    # Only count overrides for parameters in the current flagged set
    n_resolved <- if (!is.null(ov) && nrow(ov) > 0 && n_flagged > 0) {
      sum(fp$parameter_code %in% ov$parameter_code)
    } else {
      0L
    }
    all_resolved <- (n_flagged == 0L) || (n_resolved >= n_flagged)

    tagList(
      if (all_resolved) {
        actionButton(
          "run_rp",
          "Run RP Analysis",
          class = "btn-success btn-lg",
          style = "width:100%; margin-bottom:6px;"
        )
      } else {
        div(
          actionButton(
            "run_rp",
            "Run RP Analysis",
            class = "btn-success btn-lg",
            style = "width:100%; margin-bottom:6px;",
            disabled = "disabled"
          ),
          tags$small(
            class = "text-warning",
            sprintf(
              "⚠ %d parameter(s) need review before running.",
              n_flagged - n_resolved
            )
          )
        )
      },
      # Quick Run only appears when there are unresolved flagged parameters
      if (n_flagged > 0 && !all_resolved) {
        tagList(
          tags$br(),
          actionButton(
            "quick_run_rp",
            "⚡ Quick Run (Drop Unresolved)",
            class = "btn-danger",
            style = "width:100%; margin-top:4px;"
          )
        )
      }
    )
  })

  # ── Needs Attention panel ──────────────────────────────────────────────────
  # IMPORTANT: This renderUI must NOT read rv$wqs_overrides directly, because
  # doing so would cause the entire panel (and all its inputs) to be destroyed
  # and recreated every time a save occurs, wiping unsaved user input.
  # Saved state is restored only on initial render via isolate().
  output$needs_attention_panel <- renderUI({
    fp <- rv$flagged_params
    if (is.null(fp) || nrow(fp) == 0) {
      return(div(
        class = "alert alert-success",
        "✓ No parameters require attention. All DMR parameters have matching WQS criteria."
      ))
    }

    # Allowed units: intersection of recognized units and what the param reported
    recognized <- recognized_concentration_units()

    # Selected water classes (for the manual entry selector)
    wt_sel <- isolate(input$water_type_filter) %||% "all"
    avail_classes <- if ("all" %in% wt_sel) {
      unique(crosswalk$USE_CLASS_NAME_LOCATION_ETC)
    } else {
      map_one_wt <- function(x) {
        switch(
          x,
          "SD" = c("class SD waters", "class SD waters- drinking water"),
          "SG" = c("class SG waters", "class SG waters- drinking water"),
          "class SB waters" = "class SB waters",
          "surface waters" = "surface waters",
          character(0)
        )
      }
      # surface waters is selectable; include it only when selected (see the
      # reactive helper above).
      unname(unique(unlist(lapply(wt_sel, map_one_wt))))
    }

    case_labels <- c(
      "No crosswalk entry" = "Case 2 — No WQS entry exists for this parameter code in any water class.",
      "Wrong water class" = "Case 1 — WQS exists for this parameter but not for the selected water class(es).",
      "Missing criterion value" = "Case 3 — WQS entry found but criterion value is missing.",
      "Narrative criterion only" = "Case 4 — A narrative WQS applies (no numeric criterion). Enter a value only if operationalising the narrative standard.",
      "Reported statistic does not match criterion" = "Case 5 — A WQS exists, but the reported statistical basis does not match the one the criterion requires (e.g. a geometric-mean criterion with only daily-maximum data)."
    )

    # Known code aliases: DMR parameter codes whose WQS lives in the crosswalk
    # under a DIFFERENT code, usually a dissolved/total or measurement-basis
    # distinction the permit writer must resolve. Surfaced as a hint so the
    # writer knows a criterion exists rather than assuming none does. These are
    # intentionally NOT auto-joined — the fraction/basis choice is theirs.
    alias_hints <- c(
      "01040" = "Copper criterion exists under code 01042 (as total recoverable); dissolved-vs-total translation is the writer's call.",
      "01065" = "Nickel criterion exists under code 01067; dissolved-vs-total translation is the writer's call.",
      "01105" = "Aluminum criterion exists under code 01251 (SD 87 ug/L).",
      "01252" = "Arsenic criterion exists under code 01002 (SB 36, SD/SG 10 ug/L).",
      "50064" = "Free chlorine criterion exists under code 50060 (SB 7.5, SD 11 ug/L).",
      "00719" = "Cyanide criterion exists under code 00720 (4 ug/L SD/SG) or 51173 (1 ug/L SB, free).",
      "01220" = "Chromium criterion exists under code 01034 (total, SG-DW 100 ug/L); note DMR reports hexavalent.",
      "00951" = "Fluoride criterion exists under code NA (drinking-water classes only, 4000 ug/L).",
      "51445" = "Total nitrogen criterion exists under code 00600 (stream/reservoir split); note differing measurement basis.",
      "51489" = "Total nitrogen criterion exists under code 00600 (stream/reservoir split); note differing measurement basis."
    )

    # Read saved state once via isolate so the panel only re-renders when
    # rv$flagged_params changes (new fetch), not on every save.
    saved_ov <- isolate(rv$wqs_overrides)

    param_cards <- lapply(seq_len(nrow(fp)), function(i) {
      p <- fp[i, ]
      pc <- p$parameter_code
      desc <- p$parameter_desc
      reason <- p$case_reason
      n_s <- p$n_samples
      units <- p$unique_units

      # Units available for this parameter (recognized only)
      unit_choices <- intersect(
        recognized,
        trimws(strsplit(units, ",")[[1]])
      )
      if (length(unit_choices) == 0) {
        unit_choices <- recognized
      }

      radio_id <- paste0("flag_decision_", pc)
      crit_id <- paste0("flag_crit_val_", pc)
      unit_id <- paste0("flag_unit_", pc)
      class_id <- paste0("flag_wclass_", pc)
      ctype_id <- paste0("flag_ctype_", pc)
      dir_id <- paste0("flag_dir_", pc)
      notes_id <- paste0("flag_notes_", pc)
      excl_notes_id <- paste0("flag_excl_notes_", pc)

      # Restore previously saved values for this parameter
      saved_row <- if (!is.null(saved_ov) && pc %in% saved_ov$parameter_code) {
        saved_ov[saved_ov$parameter_code == pc, ]
      } else {
        NULL
      }

      saved_decision <- saved_row$decision %||% character(0)
      saved_crit_val <- if (!is.null(saved_row)) {
        saved_row$criterion_value
      } else {
        NA
      }
      saved_unit <- if (!is.null(saved_row)) saved_row$unit else unit_choices[1]
      saved_water_cls <- if (
        !is.null(saved_row) && !is.na(saved_row$water_classes)
      ) {
        trimws(strsplit(saved_row$water_classes, ",")[[1]])
      } else {
        NULL
      }
      saved_ctype <- if (!is.null(saved_row)) saved_row$criterion_type else ""
      # "ceiling" (shall not exceed) is the default; "floor" (shall not fall
      # below, e.g. dissolved oxygen) is the exception. Stored as rp_operator.
      saved_dir <- if (
        !is.null(saved_row) &&
          "rp_operator" %in% names(saved_row) &&
          !is.na(saved_row$rp_operator) &&
          saved_row$rp_operator == "<"
      ) {
        "floor"
      } else {
        "ceiling"
      }
      saved_notes <- if (
        !is.null(saved_row) && saved_row$decision == "include"
      ) {
        saved_row$notes
      } else {
        ""
      }
      saved_excl_notes <- if (
        !is.null(saved_row) && saved_row$decision == "exclude"
      ) {
        saved_row$notes
      } else {
        ""
      }

      wellPanel(
        style = "border-left: 4px solid #e8a000; margin-bottom: 12px;",
        tags$b(desc),
        tags$code(paste0(" (", pc, ")")),
        tags$br(),
        tags$span(class = "text-muted", case_labels[reason]),
        if (pc %in% names(alias_hints)) {
          tagList(
            tags$br(),
            tags$span(
              style = "color:#1f6f43; font-size:90%;",
              tags$b("Hint: "),
              alias_hints[[pc]]
            )
          )
        },
        tags$br(),
        tags$small(sprintf(
          "DMR samples: %d  |  Units reported: %s",
          n_s,
          units
        )),
        tags$hr(style = "margin: 8px 0;"),
        radioButtons(
          radio_id,
          label = NULL,
          choices = c(
            "Include with manual WQS" = "include",
            "Exclude from analysis" = "exclude"
          ),
          selected = saved_decision,
          inline = TRUE
        ),
        # Include branch
        conditionalPanel(
          condition = sprintf("input['%s'] == 'include'", radio_id),
          fluidRow(
            column(
              4,
              numericInput(
                crit_id,
                "Criterion Value",
                value = saved_crit_val,
                min = 0
              )
            ),
            column(
              4,
              selectInput(
                unit_id,
                "Unit",
                choices = unit_choices,
                selected = saved_unit
              )
            ),
            column(
              4,
              textInput(
                ctype_id,
                "Criterion Type",
                value = saved_ctype %||% "",
                placeholder = "e.g. Aquatic Life - Acute"
              )
            )
          ),
          fluidRow(
            column(
              6,
              selectInput(
                dir_id,
                "Criterion Direction",
                choices = c(
                  "Ceiling — shall not exceed (>)" = "ceiling",
                  "Floor — shall not fall below (<)" = "floor"
                ),
                selected = saved_dir
              )
            ),
            column(
              6,
              selectInput(
                class_id,
                "Apply to Water Class(es)",
                choices = avail_classes,
                selected = saved_water_cls,
                multiple = TRUE
              )
            )
          ),
          textAreaInput(
            notes_id,
            "Basis / Notes (required)",
            value = saved_notes %||% "",
            placeholder = "Describe the source and basis for this criterion value.",
            rows = 2
          )
        ),
        # Exclude branch
        conditionalPanel(
          condition = sprintf("input['%s'] == 'exclude'", radio_id),
          textAreaInput(
            excl_notes_id,
            "Reason for Exclusion (required)",
            value = saved_excl_notes %||% "",
            placeholder = "Explain why this parameter is excluded from analysis.",
            rows = 2
          )
        )
      )
    })

    tagList(
      div(
        class = "alert alert-warning",
        tags$b(sprintf(
          "⚠ %d parameter(s) require a decision before running RP.",
          nrow(fp)
        )),
        " Use the WQS Reference tab to look up applicable criteria."
      ),
      actionButton(
        "save_overrides",
        "Save All Decisions",
        class = "btn-primary",
        style = "margin-bottom: 12px;"
      ),
      uiOutput("override_save_status"),
      tagList(param_cards)
    )
  })

  # Save override decisions from the review panel.
  # Saves each valid row independently and merges into rv$wqs_overrides,
  # so partial saves accumulate rather than replacing the full set.
  observeEvent(input$save_overrides, {
    fp <- rv$flagged_params
    req(!is.null(fp), nrow(fp) > 0)

    any_incomplete <- FALSE

    for (i in seq_len(nrow(fp))) {
      pc <- fp$parameter_code[i]
      desc <- fp$parameter_desc[i]
      reason <- fp$case_reason[i]
      decision <- input[[paste0("flag_decision_", pc)]]

      # No radio selection yet — skip silently (not an error, just not filled)
      if (is.null(decision) || decision == "") {
        next
      }

      if (decision == "include") {
        crit_val <- input[[paste0("flag_crit_val_", pc)]]
        unit <- input[[paste0("flag_unit_", pc)]]
        water_cls <- input[[paste0("flag_wclass_", pc)]]
        ctype <- input[[paste0("flag_ctype_", pc)]]
        direction <- input[[paste0("flag_dir_", pc)]] %||% "ceiling"
        notes <- input[[paste0("flag_notes_", pc)]]
        if (
          is.null(crit_val) ||
            is.na(crit_val) ||
            is.null(unit) ||
            is.null(water_cls) ||
            length(water_cls) == 0 ||
            is.null(notes) ||
            nchar(trimws(notes)) < 5
        ) {
          showNotification(
            paste0(
              "Incomplete entry for ",
              desc,
              ". Criterion value, unit, water class, and notes are all required."
            ),
            type = "warning",
            duration = 6
          )
          any_incomplete <- TRUE
          next # skip this row but keep processing others
        }
        new_row <- dplyr::tibble(
          parameter_code = pc,
          parameter_desc = desc,
          case_reason = reason,
          decision = "include",
          criterion_value = as.numeric(crit_val),
          unit = unit,
          water_classes = paste(water_cls, collapse = ", "),
          criterion_type = ctype %||% "",
          rp_operator = if (identical(direction, "floor")) "<" else ">",
          notes = notes
        )
      } else {
        # exclude
        excl_notes <- input[[paste0("flag_excl_notes_", pc)]]
        if (is.null(excl_notes) || nchar(trimws(excl_notes)) < 5) {
          showNotification(
            paste0("Exclusion reason required for ", desc, "."),
            type = "warning",
            duration = 6
          )
          any_incomplete <- TRUE
          next
        }
        new_row <- dplyr::tibble(
          parameter_code = pc,
          parameter_desc = desc,
          case_reason = reason,
          decision = "exclude",
          criterion_value = NA_real_,
          unit = NA_character_,
          water_classes = NA_character_,
          criterion_type = NA_character_,
          rp_operator = NA_character_,
          notes = excl_notes
        )
      }

      # Merge: replace existing row for this parameter_code, or append
      existing <- rv$wqs_overrides
      if (!is.null(existing) && pc %in% existing$parameter_code) {
        rv$wqs_overrides <- dplyr::bind_rows(
          existing[existing$parameter_code != pc, ],
          new_row
        )
      } else {
        rv$wqs_overrides <- dplyr::bind_rows(existing, new_row)
      }
    }

    fp_codes <- fp$parameter_code
    saved_codes <- if (!is.null(rv$wqs_overrides)) {
      rv$wqs_overrides$parameter_code
    } else {
      character(0)
    }
    n_saved <- sum(fp_codes %in% saved_codes)

    if (any_incomplete) {
      showNotification(
        sprintf(
          "%d of %d parameter(s) saved. Fix the incomplete entries above and save again.",
          n_saved,
          nrow(fp)
        ),
        type = "warning",
        duration = 8
      )
    } else if (n_saved > 0) {
      showNotification(
        sprintf("✓ All %d parameter(s) saved successfully.", n_saved),
        type = "message"
      )
    }
  })

  output$override_save_status <- renderUI({
    fp <- rv$flagged_params
    ov <- rv$wqs_overrides
    n_fp <- if (!is.null(fp)) nrow(fp) else 0L
    n_ov <- if (!is.null(ov) && nrow(ov) > 0 && n_fp > 0) {
      sum(fp$parameter_code %in% ov$parameter_code)
    } else {
      0L
    }
    if (n_ov == 0) {
      return(NULL)
    }
    if (n_ov >= n_fp) {
      tags$small(
        class = "text-success",
        sprintf(
          "✓ All %d parameter(s) resolved. Run RP is now available.",
          n_fp
        )
      )
    } else {
      tags$small(
        class = "text-warning",
        sprintf("%d of %d parameter(s) saved.", n_ov, n_fp)
      )
    }
  })

  # ── Quick Run: auto-exclude all unresolved flagged params ─────────────────
  observeEvent(input$quick_run_rp, {
    fp <- rv$flagged_params
    if (!is.null(fp) && nrow(fp) > 0) {
      already_resolved <- if (!is.null(rv$wqs_overrides)) {
        rv$wqs_overrides$parameter_code
      } else {
        character(0)
      }
      unresolved <- fp %>% dplyr::filter(!parameter_code %in% already_resolved)

      if (nrow(unresolved) > 0) {
        auto_rows <- unresolved %>%
          dplyr::mutate(
            decision = "exclude",
            criterion_value = NA_real_,
            unit = NA_character_,
            water_classes = NA_character_,
            criterion_type = NA_character_,
            notes = "Auto-excluded: parameter not reviewed prior to Quick Run"
          ) %>%
          dplyr::select(
            parameter_code,
            parameter_desc,
            case_reason,
            decision,
            criterion_value,
            unit,
            water_classes,
            criterion_type,
            notes
          )

        rv$wqs_overrides <- dplyr::bind_rows(rv$wqs_overrides, auto_rows)
        rv$quick_run_flag <- TRUE
        rv$quick_run_params <- unresolved$parameter_desc
      }
    }
    # Trigger the actual RP run
    shinyjs::click("run_rp")
  })

  # --- RUN RP: compute canonicalization/MF/RWC, convert WQS and limits, build comparisons
  observeEvent(input$run_rp, {
    req(rv$dmr)

    # Hard gate: even if the button somehow fires, abort if flagged params
    # have not all been resolved. This guards against stale button state.
    fp <- rv$flagged_params
    ov <- rv$wqs_overrides
    if (!is.null(fp) && nrow(fp) > 0) {
      n_resolved <- if (!is.null(ov) && nrow(ov) > 0) {
        sum(fp$parameter_code %in% ov$parameter_code)
      } else {
        0L
      }
      if (n_resolved < nrow(fp)) {
        showNotification(
          sprintf(
            "Cannot run: %d parameter(s) in the Needs Attention tab still require a decision.",
            nrow(fp) - n_resolved
          ),
          type = "error",
          duration = 8
        )
        return()
      }
    }

    # ── Build crosswalk_effective: the single source of truth for RP calc ──────
    # = crosswalk_full (water-class-matched, all parameters)
    # + manual WQS rows (from include decisions in wqs_overrides)
    # - excluded parameters (from exclude decisions in wqs_overrides)
    #
    # This replaces the old approach of mutating rv$crosswalk, which was
    # form-filtered and would silently drop non-form parameters like Chlorine.

    excl_codes <- if (
      !is.null(rv$wqs_overrides) && nrow(rv$wqs_overrides) > 0
    ) {
      rv$wqs_overrides$parameter_code[rv$wqs_overrides$decision == "exclude"]
    } else {
      character(0)
    }

    manual_rows <- build_manual_crosswalk_rows(
      rv$wqs_overrides,
      # Unfiltered crosswalk: a manual entry exists because the parameter has
      # no criterion in the selected class, so its method profile (e.g.
      # temperature = direct) is only found outside the class filter.
      base_crosswalk = crosswalk
    )

    rv$crosswalk_effective <- rv$crosswalk_full %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      dplyr::filter(!parameter_code %in% excl_codes) %>%
      {
        if (!is.null(manual_rows) && nrow(manual_rows) > 0) {
          dplyr::bind_rows(., manual_rows)
        } else {
          .
        }
      }

    # Re-run unit conversion for manually-entered parameters using their
    # user-selected unit as the WQS target
    if (!is.null(manual_rows) && nrow(manual_rows) > 0) {
      manual_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code %in% manual_rows$parameter_code) %>%
        dplyr::left_join(
          manual_rows %>% dplyr::distinct(parameter_code, UNIT_NAME),
          by = "parameter_code"
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
          Conv_Flag = conv_data$flag,
          # Same branches as the standalone / SQLite conversion mutates: any
          # non-convertible flag (EXCLUDED, NO_WQS, FAIL) retains the value
          # as-is for later filtering by the RP stage.
          dmr_value_nmbr = dplyr::case_when(
            Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
            Conv_Flag %in% c("EXCLUDED", "NO_WQS", "FAIL") ~ dmr_value_nmbr,
            !is.na(conv_data$mult) ~ dmr_value_nmbr * conv_data$mult,
            TRUE ~ dmr_value_nmbr
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-conv_data, -UNIT_NAME)

      rv$dmr <- rv$dmr %>%
        dplyr::filter(!parameter_code %in% manual_rows$parameter_code) %>%
        dplyr::bind_rows(manual_dmr)
    }

    cat("\nTrying to get metal IDs")

    # Filter rv$dmr to selected water classes

    # Get Criterion IDs for metals (match to crosswalk_effective)
    wqs_metals <- rv$crosswalk_effective %>%
      dplyr::select(CRITERION_ID, parameter_code) %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      dplyr::filter(
        CRITERION_ID %in%
          c("79228", "79238", "79236", "79253", "79248", "79251", "79264")
      ) %>%
      dplyr::distinct()

    cat("\nRetrieved metals crosswalk")

    # Parameters evaluated by direct comparison (no MF, no dilution) are
    # handled in the direct-findings block below. Which those are is declared
    # by the crosswalk's rp_method column rather than a hardcoded list — this
    # is what lets color (00080) and turbidity (00070) reach a result, having
    # previously been excluded here with no handler downstream.
    direct_codes <- rv$crosswalk_effective %>%
      dplyr::filter(rp_method == "direct") %>%
      dplyr::pull(parameter_code) %>%
      unique()

    # Also exclude any parameter whose effective row is non-projected, as a
    # guard against a manual override that slipped through with the wrong
    # rp_method. compute_rwc() must only ever see projected concentrations.
    projected_codes <- rv$crosswalk_effective %>%
      dplyr::filter(is.na(rp_method) | rp_method == "projected") %>%
      dplyr::pull(parameter_code) %>%
      unique()

    cb <- rv$dmr %>%
      dplyr::filter(!parameter_code %in% direct_codes) %>%
      dplyr::filter(parameter_code %in% projected_codes) %>%
      # Keep only records whose units are WQS-compatible. The is.na |
      # Conv_Flag == "PASS" predicate already excludes every other state:
      #   NA (MATCH) or PASS -> concentration is in WQS units (kept)
      #   EXCLUDED           -> mass-load / flow; not comparable to a
      #                         concentration criterion (dropped)
      #   NO_WQS             -> no criterion exists for the selected class;
      #                         surfaced separately on Needs Attention (dropped)
      #   FAIL               -> genuine unit problem; retained in rv$dmr for
      #                         the exported DMR table but dropped here so a
      #                         bad RWC value isn't computed
      dplyr::filter(is.na(Conv_Flag) | Conv_Flag == "PASS")

    cat("\nfiltered cb")

    # Calculate RWC for concentration-based parameters
    rwc <- compute_rwc(
      cb,
      dilution_ratio = input$rp_dr,
      confidence_level = input$confidence_level,
      target_percentile = input$target_percentile
    )

    cat("\nrwc calculated")

    # Metals limits (always using numeric hardness for RP)
    metals_filt <- wqs_metals %>%
      dplyr::filter(parameter_code %in% cb$parameter_code)

    if (nrow(metals_filt) > 0) {
      all_metals_limits <- metal_limits_cache # cache loaded at startup

      hardness_setting <- suppressWarnings(as.numeric(input$hardness_value))
      if (!is.finite(hardness_setting)) {
        showNotification(
          "Invalid hardness value; using default 100 mg/L as CaCO3.",
          type = "warning"
        )
        hardness_setting <- 100
      }

      limits_filt <- all_metals_limits %>%
        dplyr::filter(CRITERION_ID %in% metals_filt$CRITERION_ID) %>%
        dplyr::mutate(
          hardness_set = hardness_setting,
          diff = abs(hardness_set - hardness)
        ) %>%
        dplyr::group_by(CRITERION_ID) %>%
        dplyr::slice_min(diff, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::select(CRITERION_ID, limit)

      rv$hardness <- hardness_setting

      # Write the hardness-derived metals limits back into crosswalk_effective
      # so they flow through to downstream consumers — particularly the
      # wqs_info.csv that ships in the report ZIP, which is built from
      # crosswalk_effective verbatim. Without this, hardness-metal rows
      # would still carry CRITERION_VALUE = NA in the export even though
      # the RP calculation used the calculated value. Mirrors the same
      # write-back pattern used by the TAN block below.
      rv$crosswalk_effective <- rv$crosswalk_effective %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::left_join(
          limits_filt %>%
            dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)),
          by = "CRITERION_ID"
        ) %>%
        dplyr::mutate(
          CRITERION_VALUE = dplyr::if_else(
            !is.na(limit),
            limit,
            CRITERION_VALUE
          )
        ) %>%
        dplyr::select(-limit)
    }

    cat("\n Made it past metals limits")

    # Calculate TAN Limits
    if (
      any(
        as.character(rv$crosswalk_effective$CRITERION_ID) == "79613",
        na.rm = TRUE
      )
    ) {
      pH <- suppressWarnings(as.numeric(input$ph_value))
      tempC <- suppressWarnings(as.numeric(input$temp_value))

      if (!is.finite(pH) || !is.finite(tempC)) {
        showNotification(
          "TAN limit not updated: provide numeric pH and Temperature.",
          type = "warning",
          duration = 8
        )
      } else {
        # PRWQS Rule 1303.2(C)(2)(l). The leading 0.8876 coefficient is
        # REQUIRED — omitting it inflates every TAN criterion by ~12.7% and
        # makes the standard artificially lenient. Kept identical to
        # calc_tan_limits() in functions.R, which is the authoritative copy.
        term1 <- 0.0278 / (1 + 10^(7.688 - pH))
        term2 <- 1.1994 / (1 + 10^(pH - 7.688))
        temp_factor <- 2.126 * 10^(0.028 * (20 - tempC))
        tan_mgN_L <- 0.8876 * (term1 + term2) * temp_factor

        # Overwrite TAN criterion value in crosswalk_effective
        rv$crosswalk_effective <- rv$crosswalk_effective %>%
          dplyr::mutate(
            CRITERION_ID = as.character(CRITERION_ID),
            CRITERION_VALUE = dplyr::if_else(
              CRITERION_ID == "79613",
              tan_mgN_L,
              CRITERION_VALUE
            )
          )
      }
    }

    cat("\n Joining rwc to criterion IDS")

    # Build base rwc_criteria from crosswalk_effective × rwc
    # crosswalk_effective contains all water-class-matched parameters plus manual
    # entries, with excluded parameters already removed — so every parameter that
    # reaches this point has a valid WQS criterion regardless of form association.
    rwc_criteria <- dplyr::select(
      rv$crosswalk_effective,
      NPDES_Pollutant,
      CRITERION_ID,
      CRITERION_VALUE,
      UNIT_NAME,
      parameter_code,
      USE_CLASS_NAME_LOCATION_ETC,
      # sub_class differentiates same-class criteria that diverge by receiving
      # water type (e.g., PR class SD waters — streams vs reservoir/lake for
      # total nitrogen, total phosphorus, selenium). any_of() so older
      # crosswalks without the column don't break.
      dplyr::any_of("sub_class"),
      dplyr::any_of(c(
        "rp_method",
        "rp_operator",
        "statistic",
        "criterion_form",
        "criterion_label",
        "no_exceed_rp",
        "rp_note",
        # Legacy column. Still load-bearing: the report's pH section keys its
        # Min/Max criterion lines on it. Cannot be dropped from the crosswalk
        # until that section is migrated to rp_operator.
        "Method"
      ))
    ) %>%
      dplyr::filter(rp_method == "projected") %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      # Fall back to dmr_parameters_lookup name when NPDES_Pollutant is blank,
      # mirroring the same coalesce applied in coverage_tbl_data()
      dplyr::left_join(dmr_parameters_lookup, by = "parameter_code") %>%
      dplyr::mutate(
        NPDES_Pollutant = dplyr::coalesce(NPDES_Pollutant, parameter_desc)
      ) %>%
      dplyr::select(-parameter_desc) %>%
      # Join on statistic as well as parameter_code: Enterococci has two
      # criteria under one parameter_code and each must pick up the RWC
      # computed from its own series.
      dplyr::left_join(rwc, by = c("parameter_code", "statistic")) %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))

    # ── Direct findings: observed statistic vs criterion ──────────────────
    # Replaces the former pH-only and temperature-only blocks. Serves every
    # crosswalk row with rp_method == "direct" (pH, temperature, dissolved
    # oxygen, color, turbidity). No multiplying factor and no dilution ratio
    # is applied — the observed extreme is compared to the criterion directly.
    #
    # Which extreme is compared is decided by rp_operator:
    #   ">"  ceiling -> observed maximum
    #   "<"  floor   -> observed minimum
    direct_findings <- NULL

    direct_cw <- rv$crosswalk_effective %>%
      dplyr::filter(rp_method == "direct") %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))

    if (nrow(direct_cw) > 0 && !is.null(rv$dmr) && nrow(rv$dmr) > 0) {
      direct_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code %in% direct_cw$parameter_code) %>%
        dplyr::select(
          parameter_code,
          statistic,
          perm_feature_nmbr,
          dplyr::any_of("perm_feature_type_code"),
          dmr_value_nmbr,
          dmr_unit_desc,
          limit_value_nmbr,
          statistical_base_type_code,
          monitoring_period_end_date,
          dplyr::any_of(c("limit_begin_date", "limit_end_date"))
        ) %>%
        tidyr::drop_na(dmr_value_nmbr)

      # Consumed by the Findings plot and the Data Overview sample counts.
      rv$direct_dmr <- direct_dmr
      # Retained for backward compatibility with any remaining ph_dmr readers.
      rv$ph_dmr <- direct_dmr %>%
        dplyr::filter(parameter_code == "00400") %>%
        dplyr::left_join(
          direct_cw %>%
            dplyr::distinct(
              parameter_code,
              statistic,
              criterion_label,
              NPDES_Pollutant
            ),
          by = c("parameter_code", "statistic")
        ) %>%
        dplyr::mutate(
          NPDES_Pollutant = resolve_display_name(
            criterion_label,
            NPDES_Pollutant
          )
        )

      direct_findings <- direct_cw %>%
        dplyr::select(
          NPDES_Pollutant,
          CRITERION_ID,
          CRITERION_VALUE,
          UNIT_NAME,
          parameter_code,
          USE_CLASS_NAME_LOCATION_ETC,
          dplyr::any_of("sub_class"),
          rp_method,
          rp_operator,
          statistic,
          criterion_form,
          criterion_label,
          dplyr::any_of(c("no_exceed_rp", "rp_note", "Method"))
        ) %>%
        dplyr::left_join(
          direct_dmr,
          by = c("parameter_code", "statistic")
        ) %>%
        tidyr::drop_na(dmr_value_nmbr) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(
          "NPDES_Pollutant",
          "CRITERION_ID",
          "CRITERION_VALUE",
          "UNIT_NAME",
          "parameter_code",
          "USE_CLASS_NAME_LOCATION_ETC",
          "rp_method",
          "rp_operator",
          "statistic",
          "criterion_form",
          "criterion_label",
          if ("no_exceed_rp" %in% names(.)) "no_exceed_rp",
          if ("rp_note" %in% names(.)) "rp_note",
          if ("Method" %in% names(.)) "Method",
          "perm_feature_nmbr",
          if ("perm_feature_type_code" %in% names(.)) "perm_feature_type_code",
          if ("sub_class" %in% names(.)) "sub_class"
        )))) %>%
        dplyr::summarise(
          max_value = max(dmr_value_nmbr, na.rm = TRUE),
          min_value = min(dmr_value_nmbr, na.rm = TRUE),
          mean_value = mean(dmr_value_nmbr, na.rm = TRUE),
          n_used = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          # The observed value the criterion is tested against.
          observed_value = dplyr::if_else(
            rp_operator == "<",
            min_value,
            max_value
          ),
          RP = rp_label(
            observed_value,
            CRITERION_VALUE,
            rp_operator,
            no_exceed = if ("no_exceed_rp" %in% names(.)) no_exceed_rp else "NO"
          ),
          # RWC_rs is kept for schema compatibility with the projected path,
          # but on these rows it holds an OBSERVED value — no MF, no dilution
          # ratio. value_basis is what the UI and report should key on.
          RWC_rs = observed_value,
          value_basis = "observed",
          MF = NA_real_,
          dilution_ratio = NA_real_,
          NPDES_Pollutant = resolve_display_name(
            criterion_label,
            NPDES_Pollutant
          )
        )

      rv$temp_dmr <- direct_dmr %>%
        dplyr::filter(parameter_code == "00010")
    }

    cat("\n joining metals")

    # Join numeric hardness-based limits into rwc_criteria (no range placeholders)
    if (nrow(metals_filt) > 0) {
      rwc_criteria <- rwc_criteria %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::left_join(
          limits_filt %>%
            dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)),
          by = "CRITERION_ID"
        ) %>%
        dplyr::mutate(
          CRITERION_VALUE = dplyr::if_else(
            !is.na(limit),
            limit,
            CRITERION_VALUE
          )
        ) %>%
        dplyr::select(-limit)
    }

    cat("\n Finding maximums\n")

    # Max value per parameter × outfall
    # max_dmr_vals <- rv$dmr %>%
    #   dplyr::group_by(parameter_code, perm_feature_nmbr) %>%
    #   dplyr::summarise(max_value = max(dmr_value_nmbr, na.rm = TRUE), .groups = "drop")

    #cat(paste0("columns in max_dmr_vals: ",paste(colnames(max_dmr_vals), collapse = ", ")))

    cat("\n Determining RP")

    cat(paste0(
      "\n rwc_criteria columns: ",
      paste(colnames(rwc_criteria), collapse = ", "),
      "\n"
    ))
    # Base RP (YES/NO)
    rp_concentration_findings <- rwc_criteria %>%
      dplyr::mutate(
        RP = rp_label(
          RWC_rs,
          CRITERION_VALUE,
          rp_operator,
          no_exceed = if ("no_exceed_rp" %in% names(.)) no_exceed_rp else "NO"
        ),
        # Distinguishes a calculated RWC from an observed value in the UI,
        # the report, and rp_concentration_*.csv.
        value_basis = "calculated",
        NPDES_Pollutant = resolve_display_name(
          criterion_label,
          NPDES_Pollutant
        )
      )

    cat(paste0(
      "\ncolumns in joined data: ",
      paste(colnames(rp_concentration_findings), collapse = ", ")
    ))

    # Combine all findings (bind_rows drops NULL automatically)
    all_findings <- dplyr::bind_rows(
      rp_concentration_findings,
      direct_findings
    )

    rv$rp_concentration <- all_findings

    # Evaluable-outfall check for the empty-result warning below. (The old
    # outfall dropdown this used to populate is gone — the Findings RP table
    # now shows every outfall directly — but the guard remains valid.)
    # Criterion rows that never matched a DMR series return from the
    # left_join with perm_feature_nmbr = NA — these are WQS criteria for
    # pollutants this facility does not monitor, and they are legitimate
    # rows to keep in rv$rp_concentration; they are not evaluable outfalls.
    outfalls <- rv$rp_concentration %>%
      dplyr::filter(!is.na(perm_feature_nmbr), nzchar(perm_feature_nmbr)) %>%
      dplyr::distinct(perm_feature_nmbr) %>%
      dplyr::pull(perm_feature_nmbr)

    if (length(outfalls) == 0) {
      showNotification(
        paste(
          "RP ran but no outfall produced an evaluable result.",
          "Check unit conversion and statistical-basis matching",
          "on the Needs Attention tab."
        ),
        type = "warning",
        duration = NULL
      )
    }

    current_page("rp")
    cat("\n All RP functions succesful")
  })

  # Navigate back to summary page
  observeEvent(input$back_to_summary, {
    # Strip any auto-excluded rows that Quick Run injected into wqs_overrides.
    # Without this, those rows keep n_resolved >= n_flagged, which makes
    # run_rp_buttons_ui render the green Run RP button as enabled and hides
    # the Quick Run button — giving the false impression that all parameters
    # have been genuinely resolved. Returning to Settings should restore the
    # full pre-run state so the user is reminded that attention is still needed.
    if (isTRUE(rv$quick_run_flag) && !is.null(rv$wqs_overrides)) {
      rv$wqs_overrides <- rv$wqs_overrides %>%
        dplyr::filter(
          is.na(notes) |
            notes != "Auto-excluded: parameter not reviewed prior to Quick Run"
        )
      if (nrow(rv$wqs_overrides) == 0) rv$wqs_overrides <- NULL
    }
    rv$quick_run_flag <- FALSE
    rv$quick_run_params <- character(0)
    current_page("summary")
  })

  # --- OUTPUTS: Final dashboard
  output$download_rp_conc <- downloadHandler(
    filename = function() {
      id <- input$permit_id %||% "permit"
      paste0("rp_concentration_", id, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$rp_concentration)
      readr::write_csv(rv$rp_concentration, file)
    },
    contentType = "text/csv"
  )

  # Shared reactive for the Findings RP table. Now UNFILTERED by outfall: one
  # row per outfall x pollutant x WQS criterion x water class across the whole
  # facility, so the user finally sees everything ranked in one place.
  #
  # Sort order: RP is an ordered factor (NO < DEPENDS < YES) so arrange(desc(RP))
  # places YES rows first; pollutant then outfall break ties. Row 1 is therefore
  # always the worst-case finding — which is what the plot opens on.
  #
  # Display columns (6): Outfall (short E/I label), Pollutant, WQS (value+unit
  # merged), Water Class (shortened), RWC, RP.
  # Hidden key columns ride along for the selection reactive and styling:
  # perm_feature_nmbr, perm_feature_type_code, CRITERION_ID, rp_note. They are
  # hidden via columnDefs, exactly the mechanism already used for
  # perm_feature_type_code, so DT's column filters never show them.
  rp_table_data <- reactive({
    req(rv$rp_concentration)
    df <- rv$rp_concentration %>%
      dplyr::filter(!is.na(perm_feature_nmbr), nzchar(perm_feature_nmbr)) %>%
      dplyr::mutate(
        RP = factor(RP, levels = c("NO", "DEPENDS", "YES"), ordered = TRUE),
        RWC_rs = round(RWC_rs, 2),
        # Binary (detection-based) criteria must not print as "0" — see
        # format_criterion(). Rounding first keeps numeric rows unchanged.
        CRITERION_VALUE = format_criterion(
          round(suppressWarnings(as.numeric(CRITERION_VALUE)), 2),
          if ("criterion_form" %in% names(.)) criterion_form else NA_character_
        )
      )
    sc <- if ("sub_class" %in% names(df)) df$sub_class else NA_character_
    df %>%
      dplyr::mutate(
        # WQS value + unit merged into one column ("7.5 ug/L"). RWC is always
        # in WQS units, so the unit reads for both columns.
        WQS = paste0(CRITERION_VALUE, " ", UNIT_NAME),
        Water_Class = shorten_water_class(
          format_water_class(USE_CLASS_NAME_LOCATION_ETC, sc)
        ),
        # Internal Outfall flag: mislabeled INO permits stay visibly distinct.
        # Short label carries the E/I prefix; red+bold styling is applied in
        # the renderer via the hidden perm_feature_type_code column.
        Outfall = format_outfall_label_short(
          perm_feature_nmbr,
          if ("perm_feature_type_code" %in% names(df)) {
            perm_feature_type_code
          } else {
            NA_character_
          }
        ),
        rp_note = if ("rp_note" %in% names(df)) rp_note else NA_character_
      ) %>%
      dplyr::select(
        Outfall,
        NPDES_Pollutant,
        WQS,
        Water_Class,
        RWC_rs,
        RP,
        # hidden keys (order matters only for the colnames vector below)
        perm_feature_nmbr,
        dplyr::any_of("perm_feature_type_code"),
        CRITERION_ID,
        rp_note
      ) %>%
      dplyr::arrange(dplyr::desc(RP), NPDES_Pollutant, perm_feature_nmbr) %>%
      tidyr::drop_na(RWC_rs) %>%
      distinct()
  })

  # Render the table with single-row selection driving the plot.
  #
  # filter = "top" is retained deliberately: with the outfall/pollutant
  # dropdowns gone, the per-column filters ARE the navigation. Typing "E001"
  # in Outfall or "chlor" in Pollutant reproduces the old dropdown behavior;
  # leaving them empty gives the never-before-available all-outfalls view.
  #
  # Row-selection stability: DT's _rows_selected returns the DATA-FRAME index,
  # not the display index, so user-driven sorting and filtering never desync
  # the selected row from the plot. (Same property the old math panel relied
  # on.) If the user filters the selected row out of view, the plot keeps
  # showing it — intentional: filtering narrows the browse set, it does not
  # deselect.
  #
  # scrollY + paging=FALSE replaces pagination so the table scrolls within a
  # fixed height that visually matches the 620 px plot.
  output$rp_table <- DT::renderDT({
    req(rp_table_data())
    df <- rp_table_data()
    has_type_col <- "perm_feature_type_code" %in% names(df)
    hidden_names <- intersect(
      c(
        "perm_feature_nmbr",
        "perm_feature_type_code",
        "CRITERION_ID",
        "rp_note"
      ),
      names(df)
    )
    hidden_idx <- which(names(df) %in% hidden_names) - 1

    dt <- DT::datatable(
      df,
      colnames = c(
        "Outfall",
        "Pollutant",
        "WQS",
        "Water Class",
        "RWC",
        "RP",
        # hidden columns still need header names to keep DT's bookkeeping
        # aligned; they are never displayed.
        "perm_feature_nmbr",
        if (has_type_col) "Feature Type" else NULL,
        "WQS ID",
        "rp_note"
      ),
      selection = list(mode = "single", selected = 1),
      rownames = FALSE,
      filter = "top", # per-column filters in the header — primary navigation
      class = "compact stripe hover",
      options = list(
        scrollY = "440px",
        paging = FALSE,
        scrollX = TRUE,
        dom = "t", # table only: filter row comes from filter="top"
        order = list(), # respect the YES > DEPENDS > NO pre-sort
        columnDefs = list(list(visible = FALSE, targets = hidden_idx))
      )
    ) %>%
      DT::formatStyle(
        "RP",
        backgroundColor = DT::styleEqual(
          c("YES", "DEPENDS", "NO"),
          c("#ffcccc", "#ffe5cc", "#ccffcc")
        ),
        color = DT::styleEqual(
          c("YES", "DEPENDS", "NO"),
          c("#990000", "#994c00", "#006600")
        ),
        fontWeight = "bold"
      )

    # Internal Outfalls: red + bold, driven by the hidden type code — the same
    # mechanism as before the redesign, so the flag survives the short label.
    if (has_type_col) {
      dt <- dt %>%
        DT::formatStyle(
          "Outfall",
          valueColumns = "perm_feature_type_code",
          color = DT::styleEqual("INO", "#cc0000"),
          fontWeight = DT::styleEqual("INO", "bold")
        )
    }
    dt
  })

  output$rp_table_summary <- renderDT({
    req(rv$rp_comparisons)
    datatable(
      rv$rp_comparisons,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$flags_table <- renderDT({
    req(rv$data_flags_all)
    datatable(
      rv$data_flags_all,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$finding_header <- renderUI({
    req(rv$rp_comparisons)
    if (
      any(
        rv$rp_comparisons$compare_group == "WQS" &
          rv$rp_comparisons$status_upper == "Exceedance",
        na.rm = TRUE
      ) ||
        any(
          rv$rp_comparisons$compare_group == "Permit" &
            rv$rp_comparisons$permit_status_upper == "Exceedance",
          na.rm = TRUE
        )
    ) {
      div(
        class = "alert alert-danger",
        style = "text-align:center;",
        h4("Exceedances Detected", style = "margin:0;")
      )
    } else {
      div(
        class = "alert alert-success",
        style = "text-align:center;",
        h4("No Exceedances Found", style = "margin:0;")
      )
    }
  })

  # --- INTERACTIVE INSPECTOR LOGIC ---

  # Sticky row selection. Two failure modes of raw input$rp_table_rows_selected
  # are absorbed here:
  #   * clicking the selected row deselects it (input -> NULL), which would
  #     blank the plot — ignoreNULL keeps the last selection alive;
  #   * on first render there is no click yet — default 1L opens the page on
  #     the worst-case finding (row 1 of the YES-first sort).
  sel_row <- reactiveVal(1L)

  observeEvent(
    input$rp_table_rows_selected,
    {
      sel_row(input$rp_table_rows_selected)
    },
    ignoreNULL = TRUE
  )

  # A re-run rebuilds the table; reset to the (possibly new) worst-case row so
  # a stale index from the previous run can't point at the wrong finding.
  observeEvent(rv$rp_concentration, {
    sel_row(1L)
  })

  # The single source of truth replacing input$selected_outfall and
  # input$selected_pollutant. min() guards the edge where a re-run shrinks the
  # table below the remembered index between the reset firing and the table
  # rendering.
  selected_finding <- reactive({
    df <- rp_table_data()
    req(nrow(df) > 0)
    i <- min(sel_row(), nrow(df))
    df[i, , drop = FALSE]
  })

  sel_outfall <- reactive(selected_finding()$perm_feature_nmbr)
  sel_pollutant <- reactive(selected_finding()$NPDES_Pollutant)

  # Full display label for plot titles ("External Outfall 001" /
  # "Internal Outfall 001" in red). The type code now rides along in
  # selected_finding(), so no second lookup into rv$rp_concentration is needed.
  selected_outfall_label <- reactive({
    f <- selected_finding()
    format_outfall_label(
      f$perm_feature_nmbr,
      if ("perm_feature_type_code" %in% names(f)) {
        f$perm_feature_type_code
      } else {
        NA_character_
      }
    )
  })

  output$pollutant_plotly <- renderPlotly({
    req(rv$dmr, rv$rp_concentration)
    f_sel <- selected_finding() # [CHG] selection comes from the table row
    req(nrow(f_sel) == 1)

    # Colors from Report.qmd (unchanged)
    unit_colors <- c(
      "deg C" = "#1b9e77",
      "mg/L" = "#d95f02",
      "SU" = "#7570b3",
      "mL/L" = "#e7298a",
      "ug/L" = "#66a61e"
    )
    class_colors <- c(
      "class SB waters" = "#1f78b4",
      "class SD waters- drinking water" = "#33a02c",
      "class SG waters- drinking water" = "#e31a1c",
      "class SD waters" = "#ff7f00",
      "class SG waters" = "#6a3d9a",
      "surface waters" = "#b2df8a",
      "class SD waters \u2014 stream" = "#cc6600",
      "class SD waters \u2014 reservoir/lake" = "#ffaa55"
    )

    # Subset findings to pollutant + outfall (ALL classes — the plot keeps
    # showing every criterion line, as before; the selected row's line is
    # emphasized below).
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == sel_pollutant(), # [CHG]
        perm_feature_nmbr == sel_outfall() # [CHG]
      )
    req(nrow(sub_rp) > 0)

    # A display label should map to exactly one parameter_code. If a
    # criterion_label is ever reused across codes, sub_rp spans them all and
    # picking [1] silently plots the wrong series against the wrong criterion.
    # Prefer the code that actually has observations for this outfall.
    codes_here <- unique(sub_rp$parameter_code)
    param_code <- if (length(codes_here) > 1) {
      with_data <- rv$dmr %>%
        dplyr::filter(
          parameter_code %in% codes_here,
          perm_feature_nmbr == sel_outfall() # [CHG]
        ) %>%
        dplyr::count(parameter_code, sort = TRUE)
      if (nrow(with_data) > 0) with_data$parameter_code[1] else codes_here[1]
    } else {
      codes_here[1]
    }
    sub_rp <- sub_rp %>% dplyr::filter(parameter_code == param_code)

    # Observations for that parameter + outfall
    obs <- rv$dmr %>%
      dplyr::filter(
        parameter_code == param_code,
        perm_feature_nmbr == sel_outfall() # [CHG]
      ) %>%
      dplyr::mutate(
        monitoring_period_end_date = if (
          inherits(monitoring_period_end_date, "Date")
        ) {
          monitoring_period_end_date
        } else {
          suppressWarnings(lubridate::mdy(monitoring_period_end_date))
        }
      ) %>%
      dplyr::arrange(monitoring_period_end_date)

    # pH / Temperature special sourcing — keyed on the pollutant NAME, which
    # appears verbatim in the table's Pollutant column, so this logic survives
    # the dropdown removal untouched.
    if (sel_pollutant() == "pH (maximum)") {
      # [CHG]
      obs <- rv$direct_dmr %>%
        filter(
          parameter_code == "00400" &
            statistic == "DAILY_MAX" &
            perm_feature_nmbr == sel_outfall() # [CHG]
        ) %>%
        dplyr::mutate(
          monitoring_period_end_date = if (
            inherits(monitoring_period_end_date, "Date")
          ) {
            monitoring_period_end_date
          } else {
            suppressWarnings(lubridate::mdy(monitoring_period_end_date))
          }
        )
    }

    if (sel_pollutant() == "pH (minimum)") {
      # [CHG]
      obs <- rv$direct_dmr %>%
        filter(
          parameter_code == "00400" &
            statistic == "DAILY_MIN" &
            perm_feature_nmbr == sel_outfall() # [CHG]
        ) %>%
        dplyr::mutate(
          monitoring_period_end_date = if (
            inherits(monitoring_period_end_date, "Date")
          ) {
            monitoring_period_end_date
          } else {
            suppressWarnings(lubridate::mdy(monitoring_period_end_date))
          }
        )
    }

    if (sel_pollutant() == "Temperature") {
      # [CHG]
      obs <- rv$temp_dmr %>%
        filter(perm_feature_nmbr == sel_outfall()) %>% # [CHG]
        dplyr::mutate(
          monitoring_period_end_date = if (
            inherits(monitoring_period_end_date, "Date")
          ) {
            monitoring_period_end_date
          } else {
            suppressWarnings(lubridate::mdy(monitoring_period_end_date))
          }
        )
    }

    req(nrow(obs) > 0)

    # Build Line Data (WQS, RWC, and Permit Limits)
    rwc_val <- sub_rp$RWC_rs[1]
    sc_lines <- if ("sub_class" %in% names(sub_rp)) {
      sub_rp$sub_class
    } else {
      NA_character_
    }
    # [NEW] is_sel flags the criterion belonging to the clicked table row so
    # its dashed line draws thicker — visible feedback that clicking the
    # "Zinc / SD" row vs the "Zinc / SB" row did something, even though both
    # show the same observations. Grouping by (class, value) rather than
    # carrying CRITERION_ID into distinct() avoids duplicate overlapping
    # lines / legend entries when two criteria share a class and value.
    sel_crit <- f_sel$CRITERION_ID
    wqs_lines <- sub_rp %>%
      dplyr::mutate(
        USE_CLASS_NAME_LOCATION_ETC = format_water_class(
          USE_CLASS_NAME_LOCATION_ETC,
          sc_lines
        )
      ) %>%
      dplyr::group_by(USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE) %>%
      dplyr::summarise(
        is_sel = any(as.character(CRITERION_ID) == as.character(sel_crit)),
        .groups = "drop"
      )

    # Compute "hardness where limit ~ RWC" if user is in hardness range mode
    # AND this pollutant is a metal under Class SD (unchanged)
    hardness_note <- NULL
    if (isTRUE(input$hardness_show_range) && is.finite(rwc_val)) {
      metals_ids <- c(
        "79228",
        "79238",
        "79236",
        "79253",
        "79248",
        "79251",
        "79264"
      )
      sub_sd_metal <- sub_rp %>%
        dplyr::filter(
          CRITERION_ID %in% metals_ids,
          USE_CLASS_NAME_LOCATION_ETC == "class SD waters"
        )
      if (
        nrow(sub_sd_metal) > 0 &&
          exists("metal_limits_cache", inherits = TRUE) &&
          !is.null(metal_limits_cache) &&
          nrow(metal_limits_cache) > 0
      ) {
        crit <- sub_sd_metal$CRITERION_ID[1]
        limits_df <- metal_limits_cache %>%
          dplyr::filter(CRITERION_ID == crit)
        if (nrow(limits_df) > 0) {
          h_row <- limits_df %>%
            dplyr::mutate(diff = abs(limit - rwc_val)) %>%
            dplyr::slice_min(diff, n = 1, with_ties = FALSE)
          if (nrow(h_row) == 1 && is.finite(h_row$hardness)) {
            hardness_note <- paste0(
              "RWC would exceed WQS at Hardness values < ",
              round(h_row$hardness, 0)
            )
          }
        }
      }
    }

    # TAN overlay (unchanged, input substituted)
    tan_overlay <- NULL
    if (
      !is.null(rv$tan_limits) &&
        nrow(sub_rp) > 0 &&
        "79613" %in% sub_rp$CRITERION_ID
    ) {
      tan_overlay <- rv$tan_limits %>%
        dplyr::filter(perm_feature_nmbr == sel_outfall()) %>% # [CHG]
        dplyr::mutate(
          monitoring_period_end_date = lubridate::as_date(
            monitoring_period_end_date
          )
        ) %>%
        dplyr::arrange(monitoring_period_end_date)
    }

    # [NEW] Prior permit limit, converted to WQS units. Replaces the Quick
    # Stats "Prior Limit" field with a reference line. No limit / failed
    # conversion -> pl$y is NA and no line is drawn (the old "None").
    pl <- prior_limit_info(obs, sub_rp$UNIT_NAME[1])

    ## Find lowest value for axis — [CHG] prior limit included so the line is
    ## never clipped off the bottom of the linear axis.
    vals <- c(obs$dmr_value_nmbr, rwc_val, wqs_lines$CRITERION_VALUE, pl$y)
    min_val <- min(vals, na.rm = TRUE) - 1

    # Order-of-magnitude log-axis switch (unchanged — see original comments:
    # this is NOT a units check; large ratios are real exceedances or
    # naturally wide-range parameters)
    pos_obs <- obs$dmr_value_nmbr[
      is.finite(obs$dmr_value_nmbr) &
        obs$dmr_value_nmbr > 0
    ]
    crit_min <- suppressWarnings(min(
      wqs_lines$CRITERION_VALUE[
        is.finite(wqs_lines$CRITERION_VALUE) &
          wqs_lines$CRITERION_VALUE > 0
      ]
    ))
    oom_ratio <- if (
      length(pos_obs) > 0 && is.finite(crit_min) && crit_min > 0
    ) {
      max(pos_obs) / crit_min
    } else {
      NA_real_
    }
    use_log_axis <- is.finite(oom_ratio) && oom_ratio >= 100

    # Non-detect floor handling (unchanged)
    obs$is_nondetect <- is.finite(obs$dmr_value_nmbr) & obs$dmr_value_nmbr <= 0
    nd_floor <- if (length(pos_obs) > 0) min(pos_obs) else NA_real_
    plot_nd_at_floor <- use_log_axis &&
      any(obs$is_nondetect) &&
      is.finite(nd_floor)
    obs$y_plot <- ifelse(
      obs$is_nondetect & plot_nd_at_floor,
      nd_floor,
      obs$dmr_value_nmbr
    )

    nd_suffix <- if ("nodi_code" %in% names(obs)) {
      paste0(" (NODI ", obs$nodi_code, ")")
    } else {
      ""
    }
    nd_floor_txt <- if (plot_nd_at_floor) {
      paste0("<br>Plotted at floor: ", signif(nd_floor, 3))
    } else {
      ""
    }
    obs$nd_label <- paste0(
      "Date: ",
      obs$monitoring_period_end_date,
      "<br>Non-detect",
      nd_suffix,
      nd_floor_txt
    )

    # Direct-comparison parameters have no calculated RWC (unchanged)
    is_direct_plot <- "rp_method" %in%
      names(sub_rp) &&
      isTRUE(sub_rp$rp_method[1] == "direct")

    p <- ggplot(obs, aes(x = monitoring_period_end_date, y = y_plot)) +
      geom_line(color = "lightgrey", linetype = "dotted", alpha = 0.5)

    if (!is_direct_plot) {
      p <- p +
        geom_hline(
          aes(yintercept = rwc_val, color = "Calculated RWC"),
          linetype = "solid",
          linewidth = 1
        )
    }

    # [NEW] Prior permit limit line (dot-dash, brown — a hue unused by the
    # class/unit palettes so it reads as "reference", not "criterion").
    if (is.finite(pl$y)) {
      p <- p +
        geom_hline(
          aes(yintercept = pl$y, color = !!pl$label),
          linetype = "dotdash",
          linewidth = 0.8
        )
    }

    # [CHG] WQS criterion lines split into two layers: the selected row's
    # criterion draws thicker than the rest. Colors still map by class, so the
    # legend is identical to before.
    wqs_oth <- wqs_lines[!wqs_lines$is_sel, , drop = FALSE]
    wqs_sel <- wqs_lines[wqs_lines$is_sel, , drop = FALSE]
    if (nrow(wqs_oth) > 0) {
      p <- p +
        geom_hline(
          data = wqs_oth,
          aes(
            yintercept = CRITERION_VALUE,
            color = USE_CLASS_NAME_LOCATION_ETC
          ),
          linetype = "dashed",
          linewidth = 0.6
        )
    }
    if (nrow(wqs_sel) > 0) {
      p <- p +
        geom_hline(
          data = wqs_sel,
          aes(
            yintercept = CRITERION_VALUE,
            color = USE_CLASS_NAME_LOCATION_ETC
          ),
          linetype = "dashed",
          linewidth = 1.3
        )
    }

    p <- p +
      geom_point(
        data = obs[!obs$is_nondetect, , drop = FALSE],
        aes(
          color = dmr_unit_desc,
          text = paste0(
            "Date: ",
            monitoring_period_end_date,
            "<br>Value: ",
            round(dmr_value_nmbr, 4),
            " ",
            dmr_unit_desc,
            "<br>Limit: ",
            limit_value_nmbr
          )
        ),
        size = 2
      ) +
      geom_point(
        data = obs[obs$is_nondetect, , drop = FALSE],
        aes(shape = "Non-detect", text = nd_label),
        color = "grey30",
        size = 2.6,
        stroke = 0.9
      ) +
      scale_shape_manual(name = NULL, values = c("Non-detect" = 6)) +
      {
        if (use_log_axis) {
          ggplot2::scale_y_log10()
        } else {
          ggplot2::scale_y_continuous(limits = c(min_val, NA))
        }
      } +
      scale_color_manual(
        name = "Legend",
        values = c(
          class_colors,
          "Calculated RWC" = "#37493b",
          unit_colors,
          # [NEW] prior-limit legend entry keyed on its dynamic label
          if (is.finite(pl$y)) stats::setNames("#b15928", pl$label) else NULL
        )
      ) +
      labs(
        x = "Date",
        y = paste0(
          sub_rp$UNIT_NAME[1],
          if (use_log_axis) " (log scale)" else "",
          if (plot_nd_at_floor) {
            paste0(" \u2014 non-detects at ", signif(nd_floor, 3))
          } else {
            ""
          }
        )
      ) +
      theme_minimal()

    # Hardness annotation (unchanged)
    if (!is.null(hardness_note)) {
      date_range <- range(obs$monitoring_period_end_date, na.rm = TRUE)
      x_pos <- date_range[1] + diff(date_range) / 2
      y_pos <- if (is.finite(rwc_val)) rwc_val * 1.03 else NA_real_
      if (is.finite(y_pos)) {
        p <- p +
          annotate(
            "text",
            x = x_pos,
            y = y_pos,
            label = hardness_note,
            hjust = 0.5,
            vjust = 0,
            size = 3.5,
            color = "#2c7fb8"
          )
      }
    }

    # ── [NEW] Title + subtitle: absorbs the Quick Stats panel ────────────────
    # Line 1: "Analysis for <pollutant> – <full outfall label>" (Internal in
    #         red, as before).
    # Line 2 (smaller, grey): n samples · max/min value · RWC or observed
    #         extreme · worst-case RP (colored).
    html_title <- if (grepl("^Internal Outfall", selected_outfall_label())) {
      paste0(
        "Analysis for ",
        sel_pollutant(), # [CHG]
        " \u2013 ",
        "<span style='color:#cc0000;font-weight:bold;'>",
        selected_outfall_label(),
        "</span>"
      )
    } else {
      paste0(
        "Analysis for ",
        sel_pollutant(), # [CHG]
        " \u2013 ",
        selected_outfall_label()
      )
    }

    # Worst-case RP across all classes for this pollutant x outfall — moved
    # verbatim from the deleted Quick Stats card. DEPENDS is a real
    # determination, not a placeholder (see DO / Streeter-Phelps note there).
    rp_vals <- unique(as.character(sub_rp$RP))
    worst <- if (any(rp_vals == "YES", na.rm = TRUE)) {
      "YES"
    } else if (any(rp_vals == "DEPENDS", na.rm = TRUE)) {
      "DEPENDS"
    } else {
      "NO"
    }
    worst_color <- switch(worst, "YES" = "red", "DEPENDS" = "#b8860b", "green")

    # Floor criteria (rp_operator "<") are assessed on the observed minimum —
    # moved from Quick Stats.
    f1 <- sub_rp %>% dplyr::slice(1)
    is_min_param <- if ("rp_operator" %in% names(f1)) {
      isTRUE(f1$rp_operator[1] == "<")
    } else {
      sel_pollutant() == "pH (minimum)"
    }
    val_label <- if (is_min_param) "min " else "max "
    val_num <- if (
      is_min_param && "min_value" %in% names(f1) && is.finite(f1$min_value)
    ) {
      round(f1$min_value, 3)
    } else {
      round(f1$max_value, 3)
    }

    # RWC_rs holds an OBSERVED extreme on direct-comparison rows — labelling
    # both "RWC" would let an observed DO minimum read as dilution-adjusted.
    rwc_txt <- if (is_direct_plot) {
      paste0(
        "Observed ",
        if (is_min_param) "minimum" else "maximum",
        ": ",
        round(f1$RWC_rs, 3),
        " (no RWC calculated)"
      )
    } else {
      paste0("RWC: ", round(f1$RWC_rs, 3), " ", f1$UNIT_NAME)
    }

    sub_txt <- paste0(
      "n = ",
      nrow(obs),
      " samples \u00b7 ",
      val_label,
      val_num,
      " ",
      f1$UNIT_NAME,
      " \u00b7 ",
      rwc_txt,
      " \u00b7 RP: <span style='color:",
      worst_color,
      ";font-weight:bold;'>",
      worst,
      "</span>"
    )

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        title = list(
          text = paste0(
            html_title,
            "<br><span style='font-size:12px;color:#666;'>",
            sub_txt,
            "</span>"
          ),
          x = 0,
          xanchor = "left",
          font = list(size = 14),
          pad = list(t = 10, b = 10)
        ),
        margin = list(t = 85) # [CHG] room for the subtitle line
      )
  })

  # The old nested tabset is replaced by a one-line radio toggle that only
  # renders for SD metals in hardness-range mode — the DEPENDS case, which is
  # exactly when the user needs the hardness view. Everywhere else the plot
  # renders bare.
  #
  # Note: because this renderUI re-executes when the selected row changes, the
  # radio resets to the observations view on each new selection. That is the
  # desired behavior — the hardness view is a per-parameter side trip, not a
  # sticky mode.
  output$inspector_plot_container <- renderUI({
    f <- selected_finding()
    req(nrow(f) == 1, rv$rp_concentration)

    if (!isTRUE(input$hardness_show_range)) {
      return(plotlyOutput("pollutant_plotly", height = "550px"))
    }

    metals_ids <- c(
      "79228",
      "79238",
      "79236",
      "79253",
      "79248",
      "79251",
      "79264"
    )
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == sel_pollutant(), # [CHG]
        perm_feature_nmbr == sel_outfall() # [CHG]
      )
    has_metal <- any(sub_rp$CRITERION_ID %in% metals_ids, na.rm = TRUE)
    has_sd <- any(
      sub_rp$USE_CLASS_NAME_LOCATION_ETC == "class SD waters",
      na.rm = TRUE
    )

    if (isTRUE(has_metal) && isTRUE(has_sd)) {
      tagList(
        radioButtons(
          "inspector_view",
          label = NULL,
          inline = TRUE,
          choices = c(
            "Observed vs WQS/RWC" = "obs",
            "Hardness vs Limit" = "hardness"
          ),
          selected = "obs"
        ),
        uiOutput("inspector_plot_switch")
      )
    } else {
      plotlyOutput("pollutant_plotly", height = "550px")
    }
  })

  output$inspector_plot_switch <- renderUI({
    if (identical(input$inspector_view, "hardness")) {
      plotlyOutput("metal_hardness_plotly", height = "510px")
    } else {
      plotlyOutput("pollutant_plotly", height = "510px")
    }
  })

  # rp_note callout under the plot — the cream/goldenrod styling carried over
  # from Quick Stats (DO Streeter-Phelps note, Enterococci interval note, ...).
  # Renders nothing (zero height) when the crosswalk carries no note.
  output$criterion_note <- renderUI({
    f <- selected_finding()
    req(nrow(f) == 1)
    note <- f$rp_note
    if (is.null(note) || is.na(note) || !nzchar(note)) {
      return(NULL)
    }
    div(
      style = paste0(
        "font-size:88%; color:#444; background:#fdf6e3; ",
        "border-left:3px solid #b8860b; padding:6px 8px; margin-top:8px;"
      ),
      note
    )
  })

  output$metal_hardness_plotly <- renderPlotly({
    req(selected_finding(), rv$rp_concentration) # [CHG]
    req(identical(input$hardness_mode, "range"))
    req(!is.null(metal_limits_cache) && nrow(metal_limits_cache) > 0)

    metals_ids <- c(
      "79228",
      "79238",
      "79236",
      "79253",
      "79248",
      "79251",
      "79264"
    )

    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == sel_pollutant(), # [CHG]
        perm_feature_nmbr == sel_outfall(), # [CHG]
        USE_CLASS_NAME_LOCATION_ETC == "class SD waters",
        CRITERION_ID %in% metals_ids
      )
    req(nrow(sub_rp) > 0)

    crit <- sub_rp$CRITERION_ID[1]
    ylab <- sub_rp$UNIT_NAME[1] %||% "Limit (units)"

    limits_df <- metal_limits_cache %>%
      dplyr::filter(CRITERION_ID == crit) %>%
      dplyr::arrange(hardness)
    req(nrow(limits_df) > 0)

    p <- ggplot(limits_df, aes(x = hardness, y = limit)) +
      geom_line(color = "#d61309", linewidth = 1) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = paste(
          "Hardness vs Limit \u2013",
          sel_pollutant(), # [CHG]
          "(Class SD)"
        ),
        x = "Hardness (mg/L as CaCO3)",
        y = ylab
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$download_report <- downloadHandler(
    filename = function() {
      id <- input$permit_id %||% "permit"
      stamp <- format(Sys.Date(), "%Y%m%d")
      if (isTRUE(input$include_data)) {
        paste0("RP_Report_", id, "_", stamp, ".zip")
      } else {
        paste0("RP_Report_", id, "_", stamp, ".pdf")
      }
    },
    content = function(file) {
      req(rv$rp_concentration)
      req(rv$crosswalk)

      # Use native Shiny progress (shows a bar at the top/bottom of the screen)
      withProgress(message = 'Generating Report...', value = 0.5, {
        # 1. Create Temp Workspace
        tmp_dir <- tempfile("rp_report_")
        dir.create(tmp_dir, recursive = TRUE)

        # 2. Build Coverage Table
        coverage_tbl <- tryCatch(
          {
            if (exists("coverage_tbl_data") && is.function(coverage_tbl_data)) {
              coverage_tbl_data()
            } else {
              npdes_status <- rv$crosswalk %>%
                left_join(
                  npdes_forms,
                  by = c("NPDES_Pollutant" = "Pollutant")
                ) %>%
                select(NPDES_Pollutant, Form, parameter_code) %>%
                filter(Form %in% input$npdes_forms) %>%
                mutate(
                  param_status = ifelse(
                    is.na(parameter_code),
                    "No Ref",
                    parameter_code
                  )
                ) %>%
                select(
                  Pollutant = NPDES_Pollutant,
                  Form,
                  `Ref Param` = param_status
                ) %>%
                distinct()

              params_by_code <- rv$dmr %>%
                group_by(parameter_code) %>%
                summarise(`# Samples` = n(), .groups = "drop")

              npdes_status %>%
                left_join(
                  params_by_code,
                  by = c("Ref Param" = "parameter_code")
                ) %>%
                mutate(`# Samples` = tidyr::replace_na(`# Samples`, 0))
            }
          },
          error = function(e) NULL
        )

        # 3. Write Data Files to Temp Dir
        report_src <- normalizePath("report.qmd", mustWork = TRUE)
        file.copy(
          report_src,
          file.path(tmp_dir, "report.qmd"),
          overwrite = TRUE
        )

        readr::write_csv(
          rv$rp_concentration,
          file.path(tmp_dir, "rp_concentration.csv")
        )
        readr::write_csv(rv$dmr, file.path(tmp_dir, "dmr.csv"))

        ph_path <- ""
        if (exists("rv") && is.data.frame(rv$ph_dmr) && nrow(rv$ph_dmr) > 0) {
          ph_path <- "ph_dmr.csv"
          readr::write_csv(rv$ph_dmr, file.path(tmp_dir, ph_path))
        }

        # Every rp_method == "direct" parameter, so the report can render
        # observations for DO / color / turbidity / Enterococci / oil & grease
        # without needing a bespoke block per parameter.
        direct_path <- ""
        if (
          exists("rv") &&
            is.data.frame(rv$direct_dmr) &&
            nrow(rv$direct_dmr) > 0
        ) {
          direct_path <- "direct_dmr.csv"
          readr::write_csv(rv$direct_dmr, file.path(tmp_dir, direct_path))
        }

        temp_path <- ""
        if (
          exists("rv") && is.data.frame(rv$temp_dmr) && nrow(rv$temp_dmr) > 0
        ) {
          temp_path <- "temp_dmr.csv"
          readr::write_csv(rv$temp_dmr, file.path(tmp_dir, temp_path))
        }

        if (is.data.frame(coverage_tbl) && nrow(coverage_tbl) > 0) {
          readr::write_csv(coverage_tbl, file.path(tmp_dir, "coverage.csv"))
        }

        overrides_path <- ""
        if (!is.null(rv$wqs_overrides) && nrow(rv$wqs_overrides) > 0) {
          overrides_path <- "wqs_overrides.csv"
          readr::write_csv(rv$wqs_overrides, file.path(tmp_dir, overrides_path))
        }

        # Write full crosswalk_effective as wqs_info — the report selects
        # what it needs from it locally. This replaces the old slimmed-down
        # wqs_info that only carried criterion type columns.
        wqs_info_df <- rv$crosswalk_effective
        readr::write_csv(wqs_info_df, file.path(tmp_dir, "wqs_info.csv"))

        setProgress(value = 0.8, message = "Rendering PDF...")

        # 4. Render the PDF
        out_name <- "report.pdf"
        quarto::quarto_render(
          input = file.path(tmp_dir, "report.qmd"),
          output_format = "pdf",
          output_file = out_name,
          execute_params = list(
            permit_id = input$permit_id %||% "",
            facility_name = rv$selected_facility$CWPName %||% "",
            date_start = as.character(input$date_start),
            date_end = as.character(input$date_end),
            forms = paste(input$npdes_forms, collapse = ", "),
            dilution_ratio = input$rp_dr %||% 1,
            confidence_level = input$confidence_level %||% 0.95,
            target_percentile = input$target_percentile %||% 0.95,
            hardness_mode = input$hardness_mode %||% "range",
            hardness_value = input$hardness_value %||% NA_real_,
            rp_path = "rp_concentration.csv",
            coverage_path = if (
              file.exists(file.path(tmp_dir, "coverage.csv"))
            ) {
              "coverage.csv"
            } else {
              ""
            },
            dmr_path = "dmr.csv",
            ph_dmr_path = ph_path,
            temp_dmr_path = temp_path,
            direct_dmr_path = direct_path,
            wqs_info_path = "wqs_info.csv",
            overrides_path = overrides_path,
            quick_run = rv$quick_run_flag %||% FALSE,
            quick_run_params = paste(
              rv$quick_run_params %||% character(0),
              collapse = ", "
            )
          ),
          quiet = FALSE
        )

        # 5. Delivery Logic
        if (!isTRUE(input$include_data)) {
          file.copy(file.path(tmp_dir, out_name), file, overwrite = TRUE)
        } else {
          setProgress(value = 0.9, message = "Packaging Data...")

          # Create summary findings table
          sf_tbl <- rv$rp_concentration %>%
            select(
              perm_feature_nmbr,
              dplyr::any_of("perm_feature_type_code"),
              NPDES_Pollutant,
              UNIT_NAME,
              n_used,
              min_value,
              mean_value,
              max_value,
              CRITERION_VALUE,
              RWC_rs,
              RP,
              n_used
            ) %>%
            # Keep any parameter with data (see inspector gate). A non-detect
            # NO finding is still a finding worth exporting.
            filter((!is.na(n_used) & n_used > 0) | is.finite(RWC_rs))

          # Build Excel
          xlsx_name <- "data.xlsx"
          if (requireNamespace("writexl", quietly = TRUE)) {
            sheets <- list(
              Summary_Findings = sf_tbl,
              RP_Concentration = as.data.frame(rv$rp_concentration),
              DMR = as.data.frame(rv$dmr),
              Coverage = if (!is.null(coverage_tbl)) {
                as.data.frame(coverage_tbl)
              } else {
                NULL
              },
              WQS_Info = as.data.frame(wqs_info_df)
            )
            sheets <- sheets[!vapply(sheets, is.null, logical(1))]
            writexl::write_xlsx(sheets, file.path(tmp_dir, xlsx_name))
          }

          # Build README
          readme_name <- "README.txt"
          cat(
            paste0(
              "RP Report Package\n-----------------\nGenerated:",
              format(Sys.time()),
              "\n",
              "NPDES Permit ID: ",
              input$permit_id,
              "\n",
              "----------------------------------\n",
              "REPORT SETTINGS:\n ",
              "Dates Queried: ",
              input$date_start,
              " to ",
              input$date_end,
              "\n",
              "Hardness: ",
              rv$hardness
            ),
            file = file.path(tmp_dir, readme_name)
          )

          # ZIP (Strict pathing)
          owd <- setwd(tmp_dir)
          on.exit(setwd(owd), add = TRUE)

          files_to_zip <- c(out_name, readme_name)
          if (file.exists(xlsx_name)) {
            files_to_zip <- c(files_to_zip, xlsx_name)
          }

          if (requireNamespace("zip", quietly = TRUE)) {
            zip::zipr(zipfile = file, files = files_to_zip)
          } else {
            utils::zip(zipfile = file, files = files_to_zip)
          }
        }
      }) # End withProgress
    },
    contentType = "application/octet-stream"
  )
}

shinyApp(ui, server)
