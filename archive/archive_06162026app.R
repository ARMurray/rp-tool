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

# Load crosswalk
crosswalk <- read_csv(
  "data/crosswalk.csv",
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
  distinct()

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
    rp_comparisons = NULL
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

  # POPULATE SELECTIZE — only fires when permit_data_rv has data
  observe({
    req(current_page() == "select")
    pd <- permit_data_rv()
    req(!is.null(pd) && nrow(pd) > 0)
    choice_list <- setNames(pd$SourceID, pd$display_label)
    session$onFlushed(function() {
      updateSelectizeInput(
        session,
        "permit_id",
        choices = choice_list,
        selected = "PR0001031",
        server = TRUE
      )
    })
  })

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
            dateInput("date_start", "Start Date", value = Sys.Date() - years(5))
          ),
          column(6, dateInput("date_end", "End Date", value = Sys.Date()))
        ),
        hr(),
        actionButton("back_home", "Return Home")
      ),
      mainPanel(
        fluidRow(
          column(6, leafletOutput("map", height = "400px")),
          column(6, h4("Facility Details"), DTOutput("facility_table"))
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
                    selected = "Class SB",
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
      uiOutput("finding_header"),
      sidebarLayout(
        sidebarPanel(
          # ADD THIS BUTTON
          actionButton(
            "back_to_summary",
            "← Back to Settings",
            class = "btn-outline-secondary",
            style = "width:100%; margin-bottom: 15px;"
          ),
          hr(),
          selectInput("selected_outfall", "Select Outfall", choices = NULL),
          hr(),
          selectInput(
            "selected_pollutant",
            "Select Parameter/Pollutant",
            choices = NULL
          ),
          hr(),
          uiOutput("pollutant_stats_card"), # Summary metadata
          hr(),
          #actionButton("prepare_report", "Download Report", icon = icon("file-pdf")),
          # This hidden div holds the real (invisible) download button
          #div(style = "display:none;", downloadButton("real_download", "hidden")),
          downloadButton("download_report", "Download Report"),
          br(),
          checkboxInput(
            "include_data",
            "Include Data in Download",
            value = FALSE
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "findings_tab",
            tabPanel(
              "Interactive Inspector",
              uiOutput("inspector_plot_container"),
              hr(),
              uiOutput("math_inspector") # Separate output
            ),
            tabPanel(
              "RP Summary Table",
              DTOutput("rp_table"),
              hr(),
              uiOutput("math_table") # Separate output
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
    unname(unique(unlist(lapply(sel, map_one))))
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
      dplyr::filter(
        Form %in% input$npdes_forms | parameter_code %in% c("00010", "00400")
      ) %>%
      dplyr::filter(
        USE_CLASS_NAME_LOCATION_ETC %in%
          target_water_types() |
          parameter_code %in% c("00010", "00400")
      )
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
        # Apply conversion:
        #   EXCLUDED        -> retain value as-is (will be dropped from WQS analysis)
        #   CONVERT_F_TO_C  -> apply (F-32)*5/9 offset
        #   PASS / NA       -> multiply by conv_data$mult
        #   FAIL            -> retain value at face value (flagged)
        dmr_value_nmbr = dplyr::case_when(
          Conv_Flag == "EXCLUDED" ~ dmr_value_nmbr,
          Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
          !is.na(conv_data$mult) ~ dmr_value_nmbr * conv_data$mult,
          TRUE ~ dmr_value_nmbr
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-conv_data, -UNIT_NAME)

    # ── Coalesce 82230 into 00610 (same logic as SQLite path) ───────────────
    if ("82230" %in% df_conv$parameter_code) {
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
          parameter_code != "82230" |
            (parameter_code == "82230" & is.na(has_canonical))
        ) %>%
        dplyr::mutate(
          parameter_code = dplyr::if_else(
            parameter_code == "82230",
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
          Conv_Flag = dplyr::if_else(        # NEW: F->C handled here, clear stale FAIL
            parameter_code == "00011",
            "PASS",
            Conv_Flag
          ),
          parameter_code = dplyr::if_else(   # keep LAST
            parameter_code == "00011",
            "00010",
            parameter_code
          )
        ) %>%
        dplyr::select(-has_celsius)
    }

    rv$dmr <- df_conv
    rv$dataset_source <- "Standalone"

    # Unit conversion status — EXCLUDED is tracked separately from FAIL
    rv$parameter_status <- rv$dmr %>%
      dplyr::group_by(parameter_code) %>%
      dplyr::summarize(
        total_samples = dplyr::n(),
        excluded = sum(Conv_Flag == "EXCLUDED", na.rm = TRUE),
        fails = sum(Conv_Flag == "FAIL", na.rm = TRUE),
        passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Samples available for WQS analysis (excludes mass-load records)
        wqs_samples = total_samples - excluded,
        Unit_Status = dplyr::case_when(
          excluded == total_samples ~ "EXCLUDED", # all records are mass/flow loads
          fails == wqs_samples ~ "FAIL", # all WQS-eligible records failed
          fails > 0 ~ "FAIL (Partial)", # some WQS-eligible records failed
          passes > 0 ~ "PASS", # converted successfully
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
      unname(unique(unlist(lapply(wt_sel, map_one_wt))))
    }

    # ── crosswalk_full: water class only, no form filter ─────────────────────
    # Drives the ECHO API parameter scope AND the flagging logic.
    crosswalk_full_filt <- crosswalk %>%
      dplyr::filter(
        USE_CLASS_NAME_LOCATION_ETC %in% selected_water_classes |
          parameter_code %in% c("00010", "00400")
      )
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
              # Keep all MAX stat base records (pH keeps MIN too).
              # Do NOT filter on crosswalk_full_filt$parameter_code here —
              # parameters with no WQS match (e.g. PCBs, code 39516) must still
              # reach rv$dmr so flag_unmatched_params can surface them in the
              # Needs Attention tab. The crosswalk filter only gates the RP calc,
              # not the data fetch.
              dplyr::filter(
                statistical_base_type_code == "MAX" |
                  (parameter_code == "00400" &
                    statistical_base_type_code %in% c("MAX", "MIN"))
              )

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
              # Apply conversion:
              #   EXCLUDED        -> retain value as-is (dropped from WQS analysis)
              #   CONVERT_F_TO_C  -> apply (F-32)*5/9 offset
              #   PASS / NA       -> multiply by conv_data$mult
              #   FAIL            -> retain at face value (flagged)
              dmr_value_nmbr = case_when(
                Conv_Flag == "EXCLUDED" ~ dmr_value_nmbr,
                Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
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
          if ("82230" %in% rv$dmr$parameter_code) {
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
                parameter_code != "82230" |
                  (parameter_code == "82230" & is.na(has_canonical))
              ) %>%
              dplyr::mutate(
                parameter_code = dplyr::if_else(
                  parameter_code == "82230",
                  "00610",
                  parameter_code
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
                  parameter_code == "00011", "deg c", dmr_unit_desc
                ),
                Conv_Flag = dplyr::if_else(        # NEW: F->C handled here, clear stale FAIL
                  parameter_code == "00011", "PASS", Conv_Flag
                ),
                parameter_code = dplyr::if_else(   # keep LAST
                  parameter_code == "00011", "00010", parameter_code
                )
              ) %>%
              dplyr::select(-has_celsius)
          }

          cat(paste0(
            "\nDMR has ",
            nrow(rv$dmr),
            " samples across ",
            length(unique(rv$dmr$parameter_code)),
            " parameters"
          ))

          # Unit conversion status — EXCLUDED tracked separately from FAIL
          rv$parameter_status <- rv$dmr %>%
            group_by(parameter_code) %>%
            summarize(
              total_samples = n(),
              excluded = sum(Conv_Flag == "EXCLUDED", na.rm = TRUE),
              fails = sum(Conv_Flag == "FAIL", na.rm = TRUE),
              passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              wqs_samples = total_samples - excluded,
              Unit_Status = case_when(
                excluded == total_samples ~ "EXCLUDED",
                fails == wqs_samples ~ "FAIL",
                fails > 0 ~ "FAIL (Partial)",
                passes > 0 ~ "PASS",
                TRUE ~ "MATCH"
              ),
              status_color = case_when(
                Unit_Status == "EXCLUDED" ~ "grey",
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
            dmr_parameters = dmr_parameters_lookup
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
    rv$dmr <- dplyr::bind_rows(rv$dmr, add_df)
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

    rv$dmr <- bind_rows(rv$dmr, uploaded_file)
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

  output$facility_table <- renderDT({
    req(rv$selected_facility)
    datatable(
      rv$selected_facility %>%
        dplyr::select(
          `Permit ID` = SourceID,
          `Facility` = CWPName,
          dplyr::any_of(c(
            `City` = "city",
            `State` = "state_code",
            `Status` = "permit_status_code"
          ))
        ),
      options = list(dom = 't')
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

    # pH counts from ph_dmr (built at Run RP time; may be NULL before first run)
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
    param_pollutant %>%
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
      select(!Form) %>%
      distinct()

    datatable(df_display, options = list(dom = 't', pageLength = -1)) %>%
      formatStyle(
        'Unit_Status',
        backgroundColor = styleEqual(
          c("FAIL", "FAIL (Partial)", "PASS", "MATCH"),
          c("#ffcccc", "#ffe5cc", "#ffffcc", "#ccffcc")
        ),
        color = styleEqual(
          c("FAIL", "FAIL (Partial)", "PASS", "MATCH"),
          c("#990000", "#994c00", "#999900", "#006600")
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
      dplyr::select(
        `Parameter Code` = parameter_code,
        `Pollutant` = Pollutant,
        `Water Class` = USE_CLASS_NAME_LOCATION_ETC,
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
      unname(unique(unlist(lapply(wt_sel, map_one_wt))))
    }

    case_labels <- c(
      "No crosswalk entry" = "Case 2 — No WQS entry exists for this parameter code in any water class.",
      "Wrong water class" = "Case 1 — WQS exists for this parameter but not for the selected water class(es).",
      "Missing criterion value" = "Case 3 — WQS entry found but criterion value is missing."
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
          selectInput(
            class_id,
            "Apply to Water Class(es)",
            choices = avail_classes,
            selected = saved_water_cls,
            multiple = TRUE
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

    manual_rows <- build_manual_crosswalk_rows(rv$wqs_overrides)

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
          dmr_value_nmbr = dplyr::case_when(
            Conv_Flag == "EXCLUDED" ~ dmr_value_nmbr,
            Conv_Flag == "CONVERT_F_TO_C" ~ (dmr_value_nmbr - 32) * 5 / 9,
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

    cb <- rv$dmr %>%
      dplyr::filter(
        !parameter_code %in% c("00070", "00010", "00080", "00400")
      ) %>%
      # Keep only records whose units are WQS-compatible:
      #   NA (MATCH) or PASS -> concentration successfully in WQS units
      #   EXCLUDED           -> mass/flow loads, not comparable to WQS criteria
      #   FAIL               -> genuine unit problem, excluded to avoid bad RWC values
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
        term1 <- 0.0278 / (1 + 10^(7.688 - pH))
        term2 <- 1.1994 / (1 + 10^(pH - 7.688))
        temp_factor <- 2.126 * 10^(0.028 * (20 - tempC))
        tan_mgN_L <- (term1 + term2) * temp_factor

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
      USE_CLASS_NAME_LOCATION_ETC
    ) %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      # Fall back to dmr_parameters_lookup name when NPDES_Pollutant is blank,
      # mirroring the same coalesce applied in coverage_tbl_data()
      dplyr::left_join(dmr_parameters_lookup, by = "parameter_code") %>%
      dplyr::mutate(
        NPDES_Pollutant = dplyr::coalesce(NPDES_Pollutant, parameter_desc)
      ) %>%
      dplyr::select(-parameter_desc) %>%
      dplyr::left_join(rwc, by = "parameter_code") %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))

    # pH findings
    pH_findings <- NULL
    if ("00400" %in% rv$dmr$parameter_code) {
      ph_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code == "00400") %>%
        dplyr::select(
          parameter_code,
          perm_feature_nmbr,
          dmr_value_nmbr,
          dmr_unit_desc,
          limit_value_nmbr,
          statistical_base_type_code,
          monitoring_period_end_date,
          dplyr::any_of(c("limit_begin_date", "limit_end_date"))
        ) %>%
        mutate(
          NPDES_Pollutant = ifelse(
            statistical_base_type_code == "MAX",
            "pH (maximum)",
            ifelse(statistical_base_type_code == "MIN", "pH (minimum)", NA)
          )
        ) %>%
        drop_na()

      rv$ph_dmr <- ph_dmr

      pH_findings <- rv$crosswalk_effective %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::filter(
          CRITERION_ID %in% c("79595", "79606") & parameter_code == "00400"
        ) %>%
        dplyr::select(
          NPDES_Pollutant,
          CRITERION_ID,
          Method,
          CRITERION_VALUE,
          UNIT_NAME,
          parameter_code,
          USE_CLASS_NAME_LOCATION_ETC
        ) %>%
        dplyr::left_join(
          ph_dmr,
          by = c("parameter_code", "NPDES_Pollutant")
        ) %>%
        drop_na() %>%
        group_by(
          NPDES_Pollutant,
          CRITERION_ID,
          Method,
          CRITERION_VALUE,
          UNIT_NAME,
          parameter_code,
          USE_CLASS_NAME_LOCATION_ETC,
          perm_feature_nmbr
        ) %>%
        summarise(
          max_value = max(dmr_value_nmbr, na.rm = TRUE),
          min_value = min(dmr_value_nmbr, na.rm = TRUE),
          mean_value = mean(dmr_value_nmbr, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          RP = ifelse(
            Method == "Max" & max_value >= CRITERION_VALUE,
            "YES",
            ifelse(Method == "Min" & min_value <= CRITERION_VALUE, "YES", "NO")
          ),
          RWC_rs = ifelse(Method == "Max", max_value, min_value),
          RP = as.character(RP)
        )
    }

    # Temperature findings
    tempFindings <- NULL
    if ("00010" %in% rv$dmr$parameter_code) {
      temp_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code == "00010") %>%
        mutate(NPDES_Pollutant = "Temperature") %>%
        dplyr::select(
          NPDES_Pollutant,
          parameter_code,
          perm_feature_nmbr,
          dmr_value_nmbr,
          dmr_unit_desc,
          limit_value_nmbr,
          statistical_base_type_code,
          monitoring_period_end_date,
          dplyr::any_of(c("limit_begin_date", "limit_end_date"))
        ) %>%
        drop_na(dmr_value_nmbr)

      tempFindings <- rv$crosswalk_effective %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::filter(CRITERION_ID == "79218") %>%
        dplyr::select(
          CRITERION_ID,
          Method,
          CRITERION_VALUE,
          UNIT_NAME,
          parameter_code,
          USE_CLASS_NAME_LOCATION_ETC
        ) %>%
        dplyr::left_join(temp_dmr, by = "parameter_code") %>%
        group_by(
          NPDES_Pollutant,
          CRITERION_ID,
          Method,
          CRITERION_VALUE,
          UNIT_NAME,
          parameter_code,
          USE_CLASS_NAME_LOCATION_ETC,
          perm_feature_nmbr
        ) %>%
        summarise(
          min_value = min(dmr_value_nmbr, na.rm = TRUE),
          mean_value = mean(dmr_value_nmbr, na.rm = TRUE),
          max_value = max(dmr_value_nmbr, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          RP = ifelse(max_value >= CRITERION_VALUE, "YES", "NO"),
          RWC_rs = max_value,
          RP = as.character(RP)
        )

      rv$temp_dmr <- temp_dmr
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
        RP = as.character(ifelse(RWC_rs >= CRITERION_VALUE, "YES", "NO"))
      )

    cat(paste0(
      "\ncolumns in joined data: ",
      paste(colnames(rp_concentration_findings), collapse = ", ")
    ))

    # Combine all findings (bind_rows drops NULL automatically)
    all_findings <- dplyr::bind_rows(
      rp_concentration_findings,
      pH_findings,
      tempFindings
    )

    rv$rp_concentration <- all_findings

    # Update outfall choices for the Findings UI
    outfalls <- rv$rp_concentration %>%
      dplyr::distinct(perm_feature_nmbr) %>%
      dplyr::arrange(perm_feature_nmbr) %>%
      dplyr::pull()

    updateSelectInput(
      session,
      "selected_outfall",
      choices = outfalls,
      selected = outfalls[[1]]
    )

    current_page("rp")
    cat("\n All RP functions succesful")
  })

  # Navigate back to summary page
  observeEvent(input$back_to_summary, {
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

  # Shared reactive for the table (used by both the table and the math panel)
  rp_table_data <- reactive({
    req(rv$rp_concentration, input$selected_outfall)
    rv$rp_concentration %>%
      dplyr::filter(perm_feature_nmbr == input$selected_outfall) %>%
      dplyr::mutate(
        RP = factor(RP, levels = c("YES", "NO"), ordered = TRUE),
        RWC_rs = round(RWC_rs, 2),
        CRITERION_VALUE = round(CRITERION_VALUE, 2)
      ) %>%
      dplyr::select(
        perm_feature_nmbr,
        CRITERION_ID,
        NPDES_Pollutant,
        CRITERION_VALUE,
        UNIT_NAME,
        USE_CLASS_NAME_LOCATION_ETC,
        RWC_rs,
        RP
      ) %>%
      dplyr::arrange(desc(RP)) %>%
      tidyr::drop_na(RWC_rs) %>%
      distinct()
  })

  # Render the table with row selection enabled
  output$rp_table <- DT::renderDT({
    req(rp_table_data())
    DT::datatable(
      rp_table_data(),
      colnames = c(
        "Outfall",
        "WQS ID",
        "Pollutant",
        "WQS",
        "Unit",
        "Water Class",
        "RWC",
        "RP"
      ),
      selection = "single", # enable row click selection
      rownames = FALSE,
      options = list(
        ordering = FALSE, # keep row indices aligned with rp_table_data()
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })

  # MATH
  # Tab 1: Plot Math
  output$math_inspector <- renderUI({
    req(input$selected_pollutant, input$selected_outfall, rv$rp_concentration)

    f <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == input$selected_pollutant,
        perm_feature_nmbr == input$selected_outfall
      ) %>%
      dplyr::slice(1)

    req(nrow(f) > 0, is.finite(f$MF))
    render_math_formula(f, input$rp_dr %||% 1, input$confidence_level)
  })

  # Tab 2: Table Math
  output$math_table <- renderUI({
    req(input$rp_table_rows_selected, rp_table_data())

    # Get pollutant from the selected row
    sel <- input$rp_table_rows_selected
    tbl <- rp_table_data()
    pollutant <- tbl$NPDES_Pollutant[sel]

    f <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == pollutant,
        perm_feature_nmbr == input$selected_outfall
      ) %>%
      dplyr::slice(1)

    req(nrow(f) > 0, is.finite(f$MF))
    render_math_formula(f, input$rp_dr %||% 1, input$confidence_level)
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
      div(class = "alert alert-danger", h2("Exceedances Detected"))
    } else {
      div(class = "alert alert-success", h2("No Exceedances Found"))
    }
  })

  # --- INTERACTIVE INSPECTOR LOGIC ---

  # 1. Update Pollutant Selection
  # Update Pollutant Selection based on selected outfall
  observe({
    req(rv$rp_concentration)
    data <- rv$rp_concentration
    if (!is.null(input$selected_outfall) && nzchar(input$selected_outfall)) {
      data <- data %>%
        dplyr::filter(perm_feature_nmbr == input$selected_outfall)
    }
    available <- data %>%
      dplyr::filter(RWC_rs > 0 | parameter_code %in% c("00400", "00010")) %>%
      dplyr::pull(NPDES_Pollutant) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "selected_pollutant", choices = available)
  })

  # 2. Replicate Report ggplot logic in Plotly
  output$pollutant_plotly <- renderPlotly({
    req(input$selected_pollutant, rv$dmr, rv$rp_concentration)
    req(input$selected_outfall)

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
      "surface waters" = "#b2df8a"
    )

    # Subset findings to pollutant + outfall
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == input$selected_pollutant,
        perm_feature_nmbr == input$selected_outfall
      )
    req(nrow(sub_rp) > 0)
    param_code <- sub_rp$parameter_code[1]

    # Observations for that parameter + outfall
    obs <- rv$dmr %>%
      dplyr::filter(
        parameter_code == param_code,
        perm_feature_nmbr == input$selected_outfall
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
    req(nrow(obs) > 0)

    # If plotting pH, subset to max or min
    if (input$selected_pollutant == "pH (maximum)") {
      obs <- rv$ph_dmr %>%
        filter(
          statistical_base_type_code == "MAX" &
            perm_feature_nmbr == input$selected_outfall
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
      cat("\nSelected ph Maximum, found: ", nrow(obs), "rows\n")
    }

    if (input$selected_pollutant == "pH (minimum)") {
      obs <- rv$ph_dmr %>%
        filter(
          statistical_base_type_code == "MIN" &
            perm_feature_nmbr == input$selected_outfall
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

      cat("\nSelected ph Minimum, found: ", nrow(obs), "rows\n")
    }

    # If plotting temperature, pull from temp_dmr
    if (input$selected_pollutant == "Temperature") {
      obs <- rv$temp_dmr %>%
        filter(perm_feature_nmbr == input$selected_outfall) %>%
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

    # Build Line Data (WQS, RWC, and Permit Limits)
    rwc_val <- sub_rp$RWC_rs[1]
    wqs_lines <- sub_rp %>%
      dplyr::select(USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE) %>%
      dplyr::distinct()

    # Compute “hardness where limit ≈ RWC” if user is in hardness range mode AND this pollutant is a metal under Class SD
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

    # TAN overlay: if this pollutant has CRITERION_ID "79613", add time-varying TAN limit line
    tan_overlay <- NULL
    if (
      !is.null(rv$tan_limits) &&
        nrow(sub_rp) > 0 &&
        "79613" %in% sub_rp$CRITERION_ID
    ) {
      tan_overlay <- rv$tan_limits %>%
        dplyr::filter(perm_feature_nmbr == input$selected_outfall) %>%
        dplyr::mutate(
          monitoring_period_end_date = lubridate::as_date(
            monitoring_period_end_date
          )
        ) %>%
        dplyr::arrange(monitoring_period_end_date)
    }

    # Inside output$pollutant_plotly
    ## Find lowest value for axis
    vals <- c(obs$dmr_value_nmbr, rwc_val, wqs_lines$CRITERION_VALUE)
    min_val <- min(vals) - 1

    p <- ggplot(obs, aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
      geom_line(color = "lightgrey", linetype = "dotted", alpha = 0.5) +
      geom_hline(
        aes(yintercept = rwc_val, color = "Calculated RWC"),
        linetype = "solid",
        linewidth = 1
      ) +
      geom_hline(
        data = wqs_lines,
        aes(yintercept = CRITERION_VALUE, color = USE_CLASS_NAME_LOCATION_ETC),
        linetype = "dashed",
        linewidth = 0.7
      ) +
      geom_point(
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
      scale_y_continuous(limits = c(min_val, NA)) +
      scale_color_manual(
        name = "Legend",
        values = c(class_colors, "Calculated RWC" = "#37493b", unit_colors)
      ) +
      labs(
        title = paste(
          "Analysis for",
          input$selected_pollutant,
          "– Outfall",
          input$selected_outfall
        ),
        x = "Date",
        y = sub_rp$UNIT_NAME[1]
      ) +
      theme_minimal()

    # Add the hardness note above the RWC line (if available)
    # Replace your annotation block in output$pollutant_plotly with this centered version
    if (!is.null(hardness_note)) {
      # Center the annotation between the min and max dates
      date_range <- range(obs$monitoring_period_end_date, na.rm = TRUE)
      x_pos <- date_range[1] + diff(date_range) / 2

      # Slightly above the RWC line
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

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$inspector_plot_container <- renderUI({
    req(input$selected_pollutant, rv$rp_concentration, input$selected_outfall)

    if (!isTRUE(input$hardness_show_range)) {
      return(plotlyOutput("pollutant_plotly", height = "600px"))
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
        NPDES_Pollutant == input$selected_pollutant,
        perm_feature_nmbr == input$selected_outfall
      )
    has_metal <- any(sub_rp$CRITERION_ID %in% metals_ids, na.rm = TRUE)
    has_sd <- any(
      sub_rp$USE_CLASS_NAME_LOCATION_ETC == "class SD waters",
      na.rm = TRUE
    )

    if (isTRUE(has_metal) && isTRUE(has_sd)) {
      tabsetPanel(
        tabPanel(
          "Observed vs WQS/RWC",
          plotlyOutput("pollutant_plotly", height = "600px")
        ),
        tabPanel(
          "Hardness vs Limit",
          plotlyOutput("metal_hardness_plotly", height = "600px")
        )
      )
    } else {
      plotlyOutput("pollutant_plotly", height = "600px")
    }
  })

  output$metal_hardness_plotly <- renderPlotly({
    req(input$selected_pollutant, input$selected_outfall, rv$rp_concentration)
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
        NPDES_Pollutant == input$selected_pollutant,
        perm_feature_nmbr == input$selected_outfall,
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
          "Hardness vs Limit –",
          input$selected_pollutant,
          "(Class SD)"
        ),
        x = "Hardness (mg/L as CaCO3)",
        y = ylab
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # 3. Dynamic Sidebar Stats Card
  # Replace your current output$pollutant_stats_card with this version to include "Prior Limit"

  output$pollutant_stats_card <- renderUI({
    req(
      input$selected_pollutant,
      input$selected_outfall,
      rv$rp_concentration,
      rv$dmr
    )

    # All findings for this pollutant × outfall (may include multiple water classes)

    sub <- rv$rp_concentration %>%
      dplyr::filter(
        NPDES_Pollutant == input$selected_pollutant,
        perm_feature_nmbr == input$selected_outfall
      )
    req(nrow(sub) > 0)

    # Use a representative row for parameter_code/units/etc. (MF/RWC are the same across classes)
    f <- sub %>% dplyr::slice(1)

    ## Define DMR based on concentration or other
    if (input$selected_pollutant == "pH (minimum)") {
      dmr_sel <- rv$ph_dmr %>%
        filter(
          NPDES_Pollutant == "pH (minimum)" &
            perm_feature_nmbr == input$selected_outfall
        )
    } else if (input$selected_pollutant == "pH (maximum)") {
      dmr_sel <- rv$ph_dmr %>%
        filter(
          NPDES_Pollutant == "pH (maximum)" &
            perm_feature_nmbr == input$selected_outfall
        )
    } else if (input$selected_pollutant == "Temperature") {
      dmr_sel <- rv$temp_dmr %>%
        filter(
          NPDES_Pollutant == "Temperature" &
            perm_feature_nmbr == input$selected_outfall
        )
    } else {
      dmr_sel <- rv$dmr
    }

    # Sample count for selected parameter + outfall
    n_samps <- dmr_sel %>%
      dplyr::filter(
        parameter_code == f$parameter_code,
        perm_feature_nmbr == input$selected_outfall
      ) %>%
      nrow()

    # Prior limit from DMR (same field used in tooltips)
    lim_row <- dmr_sel %>%
      dplyr::filter(
        parameter_code == f$parameter_code,
        perm_feature_nmbr == input$selected_outfall
      ) %>%
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

    prior_limit_txt <- if (nrow(lim_row) == 1) {
      paste0(
        round(lim_row$lim_val_num, 3),
        " ",
        lim_row$limit_unit_desc %||% ""
      )
    } else {
      "None"
    }

    # rp_by_class distinct per class
    rp_by_class <- sub %>%
      dplyr::select(USE_CLASS_NAME_LOCATION_ETC, RP) %>%
      dplyr::distinct()

    # Worst-case RP: YES > NO
    worst <- if (any(rp_by_class$RP == "YES", na.rm = TRUE)) "YES" else "NO"
    worst_color <- if (worst == "YES") "red" else "green"

    # Per-class list items (YES/NO)
    items <- lapply(seq_len(nrow(rp_by_class)), function(i) {
      cls <- rp_by_class$USE_CLASS_NAME_LOCATION_ETC[i]
      val <- rp_by_class$RP[i]
      col <- if (val == "YES") "red" else "green"
      tags$li(span(
        paste0(cls, ": ", val),
        style = paste0("color:", col, "; font-weight:bold;")
      ))
    })

    wellPanel(
      h5("Quick Stats"),
      tags$b("Outfall: "),
      input$selected_outfall,
      br(),
      tags$b("Samples: "),
      n_samps,
      br(),
      # Label and value depend on whether this is a minimum criterion parameter
      {
        is_min_param <- input$selected_pollutant == "pH (minimum)"
        val_label <- if (is_min_param) "Min Value: " else "Max Value: "
        val_num <- if (
          is_min_param && "min_value" %in% names(f) && is.finite(f$min_value)
        ) {
          round(f$min_value, 3)
        } else {
          round(f$max_value, 3)
        }
        tagList(tags$b(val_label), val_num, " ", f$UNIT_NAME, br())
      },
      tags$b("Prior Limit: "),
      prior_limit_txt,
      br(),
      tags$b("RWC: "),
      round(f$RWC_rs, 3),
      br(),
      tags$b("Worst-case RP: "),
      span(worst, style = paste0("color:", worst_color, "; font-weight:bold;")),
      if (nrow(rp_by_class) > 1) {
        tagList(
          tags$hr(),
          tags$b("RP by Water Class:"),
          tags$ul(style = "margin: 4px 0 0 18px; padding: 0;", items),
          if (any(rp_by_class$RP == "DEPENDS", na.rm = TRUE)) {
            div(
              style = "margin-top:6px; font-size: 90%;",
              "Hardness range selected for metals; see 'Hardness vs Limit' tab for thresholds."
            )
          }
        )
      }
    )
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
              NPDES_Pollutant,
              UNIT_NAME,
              n_used,
              min_value,
              mean_value,
              max_value,
              CRITERION_VALUE,
              RWC_rs,
              RP
            ) %>%
            filter(RWC_rs > 0)

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
