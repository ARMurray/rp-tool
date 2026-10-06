# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(echor)
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
crosswalk <- read_csv("data/full_join.csv",
                      col_types = cols(parameter_code = col_character()))%>%
  mutate(parameter_code = str_pad(parameter_code,5,"left",pad = "0"))%>%
  mutate(NPDES_Pollutant = ifelse(NPDES_Pollutant %in% c("Temperature (summer)","Temperature (winter)"),"Temperature",NPDES_Pollutant))%>%
  distinct()

# Load NPDES forms
npdes_forms <- read_csv("data/NPDES_Forms_Pollutants_1.csv")%>%
  mutate(Pollutant = ifelse(Pollutant %in% c("Temperature (summer)","Temperature (winter)"),"Temperature",Pollutant))%>%
  distinct()

# Load metals limits
metal_limits_cache <- read_csv("www/metal_limits.csv", show_col_types = FALSE)


message("Pre-loading PR/VI Facility Cache from ECHO...")
global_permit_data <- tryCatch({
  echoWaterGetFacilityInfo(output = "df", p_st = "PR", p_ptype = "NPD")%>%
    mutate(
      FacLat = as.numeric(FacLat),
      FacLong = as.numeric(FacLong),
      display_label = paste0(SourceID, " - ", CWPName)
    )
}, error = function(e) {
  message("Global Fetch Error: ", e)
  NULL
})

# ---------------- UI ----------------
ui <- fluidPage(
  useShinyjs(),
  #theme = bslib::bs_theme(),
  theme = bslib::bs_theme(bg = "#ffffff", primary = "#07648d", success = "#4d8055",fg = "#000",info = "#07648d"),
  
  # Updated Title Panel with a Help Button
  div(class = "d-flex justify-content-between align-items-center",
      titlePanel(
        title = span(
          img(src = "epa_logo.png", height = 75, style = "margin-right: 15px;"), 
          "Puerto Rico Reasonable Potential (RP) Calculator"
        )
      ),
      actionButton("show_guide", "User Guide", icon = icon("question-circle"), 
                   class = "btn-info", style = "margin-top: 20px; margin-right: 20px;")
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
  
  metals_ids <- c("79228","79238","79236","79253","79248","79251","79264")
  
  
  # Reactive Values
  current_page <- reactiveVal("landing")
  rv <- reactiveValues(
    crosswalk = NULL,
    dmr = NULL,
    selected_facility = NULL,
    dataset_source = "DMR Only",
    canonical_df_all = NULL,
    mf_by_outfall_all = NULL,
    rwc_sum_all = NULL,
    data_flags_all = NULL,
    wqs_df_canon = NULL,
    permit_limits_df = NULL,
    rp_comparisons = NULL
  )
  
  # POPULATE SELECTIZE
  observe({
    req(current_page() == "select")
    req(global_permit_data)
    choice_list <- setNames(global_permit_data$SourceID, global_permit_data$display_label)
    session$onFlushed(function() {
      updateSelectizeInput(session, "permit_id",
                           choices = choice_list,
                           selected = "PR0001031",
                           server = TRUE)
    })
  })
  
  # --- PAGE ROUTING ---
  output$page_content <- renderUI({
    switch(current_page(),
           "landing" = landing_ui(),
           "select"  = select_ui(),
           "summary" = summary_ui(),
           "rp"      = findings_ui(),
           "standalone" = standalone_ui())
  })
  
  # --- UI PAGE DEFINITIONS ---
  landing_ui <- function() {
    div(class = "container", style = "margin-top: 50px; text-align: center;",
        br(),
        p("This application is designed to aid permit writers in determining if reasonable potential exists for effluent discharging from NPDES permitted facilities to exceed published water quality standards. For detailed information on how to use this application, refer to the User Guide which can be accessed in the top right corner."),
        br(),br(),
        h2("Select Analysis Workflow"),
        br(),
        fluidRow(
          column(6, wellPanel(
            icon("search", "fa-3x"), h4("Calculate RWC Using NPDES Data"),
            actionButton("go_npdes", "Connect to ICIS-NPDES", class = "btn-primary"),
            br(),br(),
            p("Choose this if you have the NPDES ID. You will have the option to add local data if needed.")
          )),
          column(6, wellPanel(
            icon("file-upload", "fa-3x"), h4("Calculate RWC Using Local Data"),
            actionButton("go_calc", "Standalone Calculator", class = "btn-primary"),
            br(),br(),
            p("Choose this if no NPDES ID exists but you have data to use.")
          ))
        )
    )
  }
  
  standalone_ui <- function() {
    sidebarLayout(
      sidebarPanel(
        h4("Standalone Analysis Settings"),
        
        # Use the same IDs as summary_ui so downstream code (coverage, RP) just works
        selectInput(
          "npdes_forms", "NPDES Forms",
          choices = sort(unique(npdes_forms$Form)),
          multiple = TRUE
        ),
        h5("Water Classifications"),
        selectInput(
          inputId = "water_type_filter",
          label   = "Water Type",
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
        fileInput("standalone_csv", "Upload Completed DMR Template", accept = ".csv"),
        hr(),
        numericInput("rp_dr", "Dilution Ratio", value = 1, min = 1),
        numericInput("confidence_level", "Confidence Level", value = 0.95, min = 0.5, max = 0.999, step = 0.01),
        numericInput("target_percentile", "Target Percentile (upper bound)", value = 0.95, min = 0.8, max = 0.999, step = 0.01),
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
            label   = "Hardness (mg/L as CaCO3)",
            value   = 100,
            min     = 1,
            max     = 1000,
            step    = 1
          ),
          checkboxInput(
            inputId = "hardness_show_range",
            label   = "Show hardness range analysis (plots only)",
            value   = FALSE
          ),
          # pH and Temperature (numeric only; receiving water for TAN)
          numericInput(
            inputId = "ph_value",
            label   = "Receiving Water pH (SU)",
            value   = 6,
            min     = 0,
            max     = 14,
            step    = 0.1
          ),
          numericInput(
            inputId = "temp_value",
            label   = "Receiving Water Temperature (°C)",
            value   = 30,
            min     = 0,
            max     = 50,
            step    = 0.1
          )
        ),
        hr(),
        actionButton("run_rp", "Run RP Analysis", class = "btn-success", style = "width:100%;"),
        hr(),
        actionButton("back_home_from_standalone", "← Back to Home", class = "btn-outline-secondary", style = "width:100%;"),
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
        selectizeInput("permit_id", "Search Permit ID or Facility Name",
                       choices = "", options = list(placeholder = 'Type to search...')),
        fluidRow(
          column(6, dateInput("date_start", "Start Date", value = Sys.Date() - years(5))),
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
          div(id = "filters_block",
              fluidRow(
                column(2, div(h4("Settings"))),
                column(10,
                       fluidRow(
                         column(6, selectInput("npdes_forms", "NPDES Forms",
                                               choices = sort(unique(npdes_forms$Form)),
                                               multiple = TRUE)),
                         h5("Water Classifications"),
                         selectInput(
                           inputId = "water_type_filter",
                           label   = "Water Type",
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
          actionButton("fetch_dmr", "Fetch DMR Data", class = "btn-success", style="width:100%;")
        )
      )
    )
  }
  
  summary_ui <- function() {
    fluidPage(
      fluidRow(
        column(4, wellPanel(
          h4("Analysis Settings"),
          p(tags$b("NPDES Forms:"), paste(input$npdes_forms, collapse = ", ")),
          numericInput("rp_dr", "Dilution Ratio", value = 1, min = 1),
          numericInput("confidence_level", "Confidence Level", value = 0.95, min = 0.5, max = 0.999, step = 0.01),
          numericInput("target_percentile", "Target Percentile (upper bound)", value = 0.95, min = 0.8, max = 0.999, step = 0.01),
          
          # Hardness Setting
          # In summary_ui(), replace the Hardness Setting + pH/temp blocks with:
          
          tags$hr(),
          conditionalPanel(
            # Show only if Class SD is selected in Water Type multi-select
            condition = "Array.isArray(input.water_type_filter) && input.water_type_filter.indexOf('SD') >= 0",
            h5("Receiving Water Inputs (Class SD)"),
            # Hardness (numeric only; used for RP calculation)
            numericInput(
              inputId = "hardness_value",
              label   = "Hardness (mg/L as CaCO3)",
              value   = 100,
              min     = 1,
              max     = 1000,
              step    = 1
            ),
            checkboxInput(
              inputId = "hardness_show_range",
              label   = "Show hardness range analysis (plots only)",
              value   = FALSE
            ),
            # pH and Temperature (numeric only; receiving water for TAN)
            numericInput(
              inputId = "ph_value",
              label   = "Receiving Water pH (SU)",
              value   = 6,
              min     = 0,
              max     = 14,
              step    = 0.1
            ),
            numericInput(
              inputId = "temp_value",
              label   = "Receiving Water Temperature (°C)",
              value   = 30,
              min     = 0,
              max     = 50,
              step    = 0.1
            )
          ),
          
          # TEMPORARY DOWNLOAD CHECK
          downloadLink("downloadDmr", "Download"),
          
          actionButton("run_rp", "Run RP Analysis", class = "btn-success btn-lg", style="width:100%;"),
          hr(),
          h5("Append DMR Data"),
          div(
            style = "margin-bottom: 8px;",
            downloadButton("download_dmr_template", "Download DMR Template")
          ),
          fileInput("append_dmr_csv", "Upload CSV to Append", accept = ".csv"),
          actionButton("back_to_select", "← Back to Search", class = "btn-outline-secondary", style="width:100%;")
        )),
        column(8,
               h4("Data Overview"),
               dataTableOutput("coverage_summary"))
      )
    )
  }
  
  findings_ui <- function() {
    fluidPage(
      uiOutput("finding_header"),
      sidebarLayout(
        sidebarPanel(
          # ADD THIS BUTTON
          actionButton("back_to_summary", "← Back to Settings", 
                       class = "btn-outline-secondary", style="width:100%; margin-bottom: 15px;"),
          hr(),
          selectInput("selected_outfall", "Select Outfall", choices = NULL),
          hr(),
          selectInput("selected_pollutant", "Select Parameter/Pollutant", choices = NULL),
          hr(),
          uiOutput("pollutant_stats_card"), # Summary metadata
          hr(),
          #actionButton("prepare_report", "Download Report", icon = icon("file-pdf")),
          # This hidden div holds the real (invisible) download button
          #div(style = "display:none;", downloadButton("real_download", "hidden")),
          downloadButton("download_report", "Download Report"),
          br(),
          checkboxInput("include_data", "Include Data in Download", value = FALSE)
          
        ),
        mainPanel(
          tabsetPanel(
            id = "findings_tab",
            tabPanel("Interactive Inspector", 
                     uiOutput("inspector_plot_container"),
                     hr(),
                     uiOutput("math_inspector") # Separate output
            ),
            tabPanel("RP Summary Table", 
                     DTOutput("rp_table"),
                     hr(),
                     uiOutput("math_table")     # Separate output
            )
          )
        )
      )
    )
  }
  
  
  # --- LOGIC OBSERVERS ---
  
  observeEvent(input$go_npdes, { current_page("select") })
  observeEvent(input$back_home, { current_page("landing") })
  observeEvent(input$back_to_select, { current_page("select") })
  observeEvent(input$go_calc, { current_page("standalone") })
  observeEvent(input$back_home_from_standalone, {
    current_page("landing")
  })
  
  # Reveal filters and fetch button after loading facility
  observeEvent(input$permit_id, {
    req(input$permit_id)
    facility_match <- global_permit_data %>%
      filter(SourceID == input$permit_id) %>%
      slice(1)
    if (nrow(facility_match) == 0) {
      showNotification("Error: Facility not found in local cache.", type = "error")
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
    if ("all" %in% sel) return(unique(crosswalk$USE_CLASS_NAME_LOCATION_ETC))
    
    base <- character(0)
    map_one <- function(x) {
      switch(x,
             "SD"               = c("class SD waters", "class SD waters- drinking water"),
             "SG"               = c("class SG waters", "class SG waters- drinking water"),
             "class SB waters"  = "class SB waters",
             "surface waters"   = "surface waters",
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
      dplyr::left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant")) %>%
      dplyr::filter(Form %in% input$npdes_forms | parameter_code %in% c("00010","00400")) %>%
      dplyr::filter(USE_CLASS_NAME_LOCATION_ETC %in% target_water_types() | parameter_code %in% c("00010","00400"))
    rv$crosswalk <- crosswalk_filt
  })
  
  observeEvent(input$standalone_csv, {
    req(input$standalone_csv)
    req(rv$crosswalk)
    
    # Read
    df <- readr::read_csv(input$standalone_csv$datapath, show_col_types = FALSE)
    
    # Required columns
    needed <- c("parameter_code","dmr_value_nmbr","dmr_unit_desc","monitoring_period_end_date","perm_feature_nmbr")
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) {
      showNotification(paste("Missing required columns:", paste(missing, collapse = ", ")), type = "error")
      return()
    }
    
    # Standardize and coerce
    df <- df %>%
      dplyr::mutate(
        parameter_code = stringr::str_pad(as.character(parameter_code), 5, pad = "0"),
        dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
        monitoring_period_end_date = suppressWarnings(lubridate::mdy(monitoring_period_end_date)),
        perm_feature_nmbr = as.character(perm_feature_nmbr)
      )
    
    # Join expected WQS units and convert
    df_conv <- df %>%
      dplyr::left_join(rv$crosswalk %>% dplyr::select(parameter_code, UNIT_NAME) %>% dplyr::distinct(),
                       by = "parameter_code") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
        Conv_Flag = conv_data$flag,
        dmr_value_nmbr = dmr_value_nmbr * conv_data$mult
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-conv_data, -UNIT_NAME)
    
    rv$dmr <- df_conv
    rv$dataset_source <- "Standalone"
    
    # Unit conversion status
    rv$parameter_status <- rv$dmr %>%
      dplyr::group_by(parameter_code) %>%
      dplyr::summarize(
        total_samples = dplyr::n(),
        fails  = sum(Conv_Flag == "FAIL", na.rm = TRUE),
        passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        Unit_Status = dplyr::case_when(
          fails == total_samples ~ "FAIL",
          fails > 0              ~ "FAIL (Partial)",
          passes > 0             ~ "PASS",
          TRUE                   ~ "MATCH"
        )
      )
    
    showNotification("Standalone data loaded. Review summary and click Run RP.", type = "message")
  })
  
  # Fetch DMR, apply basic cleaning, store for summary
  observeEvent(input$fetch_dmr, {
    req(rv$selected_facility, input$date_start, input$date_end, input$permit_id, input$npdes_forms)
    
    # Map the user selection to the full set of detailed strings
    target_water_types <- reactive({
      req(input$water_type_filter)
      sel <- input$water_type_filter
      # If "all" is chosen (alone or with others), return all classes
      if ("all" %in% sel) return(unique(crosswalk$USE_CLASS_NAME_LOCATION_ETC))
      
      base <- character(0)
      map_one <- function(x) {
        switch(x,
               "SD"               = c("class SD waters", "class SD waters- drinking water"),
               "SG"               = c("class SG waters", "class SG waters- drinking water"),
               "class SB waters"  = "class SB waters",
               "surface waters"   = "surface waters",
               character(0)
        )
      }
      unname(unique(unlist(lapply(sel, map_one))))
    })
    
    
    start_fmt <- format(input$date_start, "%m/%d/%Y")
    end_fmt   <- format(input$date_end, "%m/%d/%Y")
    
    # Filter crosswalk to parameters associated with selected forms
    crosswalk_filt <- crosswalk%>%
      left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant"))%>%
      filter(Form %in% input$npdes_forms)%>%
      filter(USE_CLASS_NAME_LOCATION_ETC %in% target_water_types())
    
    rv$crosswalk <- crosswalk_filt
    
    withProgress(message = 'Downloading Effluent Records...', value = 0.5, {
      dmr_raw <- tryCatch({
        raw <- echoGetEffluent(p_id = input$permit_id,
                               output = "df",
                               start_date = start_fmt,
                               end_date   = end_fmt) %>%
          filter(parameter_code %in% crosswalk_filt$parameter_code) %>%
          filter(
            perm_feature_type_code == "EXO",
            statistical_base_type_code == "MAX" |
              (parameter_code == "00400" & statistical_base_type_code %in% c("MAX","MIN"))
          ) %>%
          mutate(
            dmr_value_nmbr = as.numeric(dmr_value_nmbr),
            dmr_unit_desc  = as.character(dmr_unit_desc)
          )
        
        # (1) Determine dominant unit per parameter from rows that actually have a unit
        dom_units <- raw %>%
          filter(!is.na(dmr_unit_desc), dmr_unit_desc != "") %>%
          group_by(parameter_code, dmr_unit_desc) %>%
          summarise(n = n(), .groups = "drop_last") %>%
          slice_max(n, with_ties = FALSE) %>%
          ungroup() %>%
          select(parameter_code, dominant_unit = dmr_unit_desc)
        
        # (2) Flag parameters where ALL rows have missing/blank units
        all_missing_tbl <- raw %>%
          group_by(parameter_code) %>%
          summarise(all_units_missing = all(is.na(dmr_unit_desc) | dmr_unit_desc == ""), .groups = "drop")
        
        # (3) Bring in UNIT_NAME from crosswalk for fallback
        unit_hint <- crosswalk_filt %>%
          distinct(parameter_code, UNIT_NAME)
        
        raw %>%
          left_join(dom_units, by = "parameter_code") %>%
          left_join(all_missing_tbl, by = "parameter_code") %>%
          left_join(unit_hint, by = "parameter_code") %>%
          mutate(
            # Set value for NODI B/Q to 0 (your policy)
            dmr_value_nmbr = ifelse(nodi_code %in% c("B","Q"), 0, dmr_value_nmbr),
            # Fill units:
            # - If all rows for this parameter have missing units, use UNIT_NAME for all missing/blank units
            # - Else, for NODI rows with missing unit, use dominant_unit when available
            dmr_unit_desc = case_when(
              all_units_missing & (is.na(dmr_unit_desc) | dmr_unit_desc == "") ~ UNIT_NAME,
              nodi_code %in% c("B","Q") & (is.na(dmr_unit_desc) | dmr_unit_desc == "") & !is.na(dominant_unit) ~ dominant_unit,
              TRUE ~ dmr_unit_desc
            )
          ) %>%
          select(-dominant_unit, -all_units_missing, -UNIT_NAME) %>%
          drop_na(dmr_value_nmbr)
      }, error = function(e) {
        message("ECHO API Error: ", e)
        NULL
      })
      
      cat("DMR download succesful!!!")
      
      if (!is.null(dmr_raw) && nrow(dmr_raw) > 0) {
       
        
        rv$dmr <- dmr_raw %>%
          # Standardize codes first
          mutate(parameter_code = str_pad(parameter_code, 5, "left", "0")) %>%
          # Join with crosswalk to see what the WQS unit should be
          left_join(crosswalk %>% select(parameter_code, UNIT_NAME) %>% distinct(), 
                    by = "parameter_code") %>%
          rowwise() %>%
          mutate(
            conv_data = list(get_unit_conversion(dmr_unit_desc, UNIT_NAME)),
            Conv_Flag = conv_data$flag,
            # Perform the math
            dmr_value_nmbr = dmr_value_nmbr * conv_data$mult
          ) %>%
          ungroup() %>%
          select(-conv_data, -UNIT_NAME) # Clean up helper columns
        
        
        cat(paste0("\nDMR has ",nrow(rv$dmr)," samples across ",length(unique(rv$dmr$parameter_code))," parameters"))
        
        # After you have processed rv$dmr and added the 'Conv_Flag' column...
        
        rv$parameter_status <- rv$dmr %>%
          group_by(parameter_code) %>%
          summarize(
            total_samples = n(),
            fails = sum(Conv_Flag == "FAIL", na.rm = TRUE),
            passes = sum(Conv_Flag == "PASS", na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            Unit_Status = case_when(
              fails == total_samples ~ "FAIL",         # All samples failed
              fails > 0              ~ "FAIL (Partial)", # Some failed
              passes > 0             ~ "PASS",         # Some converted, none failed
              TRUE                   ~ "MATCH"         # Everything matched perfectly
            ),
            # Map the logic to colors for the UI
            status_color = case_when(
              Unit_Status == "FAIL"            ~ "red",
              Unit_Status == "FAIL (Partial)"  ~ "orange",
              Unit_Status == "PASS"            ~ "yellow",
              Unit_Status == "MATCH"           ~ "green"
            )
          )
        
        current_page("summary")
      } else {
        showNotification("No DMR records found for the selected date range.", type = "warning")
      }
    })
  })
  
  output$downloadDmr <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(rv$dmr)
      write.csv(rv$dmr, file)
    }
  )
  
  output$download_dmr_template <- downloadHandler(
    filename = function() {
      id <- input$permit_id %||% "permit"
      paste0("dmr_append_template_", id, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv$crosswalk)
      tmpl <- rv$crosswalk %>%
        dplyr::distinct(parameter_code, parameter_desc = POLLUTANT_NAME, Expected_Unit = UNIT_NAME) %>%
        dplyr::mutate(
          dmr_value_nmbr = NA_real_,
          dmr_unit_desc = Expected_Unit,
          monitoring_period_end_date = "MM-DD-YYYY" # user fills (YYYY-MM-DD)
        ) %>%
        dplyr::select(parameter_code, parameter_desc, dmr_value_nmbr, dmr_unit_desc, monitoring_period_end_date, Expected_Unit)
      
      readr::write_csv(tmpl, file)
    }
  )
  
  observeEvent(input$append_dmr_csv, {
    req(input$append_dmr_csv)
    add_df <- readr::read_csv(input$append_dmr_csv$datapath, show_col_types = FALSE)
    
    # Required columns
    needed <- c("parameter_code","dmr_value_nmbr","dmr_unit_desc","monitoring_period_end_date")
    missing <- setdiff(needed, names(add_df))
    if (length(missing) > 0) {
      showNotification(paste("Missing required columns:", paste(missing, collapse = ", ")), type = "error")
      return()
    }
    
    add_df <- add_df %>%
      dplyr::mutate(
        parameter_code = stringr::str_pad(as.character(parameter_code), 5, pad = "0"),
        # keep units as given; users are responsible for correctness
        dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
        # try ISO parse; if it fails, keep as character
        monitoring_period_end_date = suppressWarnings(lubridate::mdy(monitoring_period_end_date))
      )
    
    # Optional: warn if units don’t match expected UNIT_NAME
    if (!is.null(rv$crosswalk)) {
      exp_units <- rv$crosswalk %>%
        dplyr::distinct(parameter_code, Expected_Unit = UNIT_NAME)
      chk <- add_df %>%
        dplyr::left_join(exp_units, by = "parameter_code") %>%
        dplyr::filter(!is.na(Expected_Unit) & !is.na(dmr_unit_desc) & dmr_unit_desc != Expected_Unit)
      if (nrow(chk) > 0) {
        warn_codes <- paste(unique(chk$parameter_code), collapse = ", ")
        showNotification(
          paste0("Unit mismatch for parameter_code(s): ", warn_codes,
                 ". Expected vs provided units differ. Proceeding to append as-is."),
          type = "warning", duration = 8
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
        parameter_desc = if (!"parameter_desc" %in% names(add_df)) NA_character_ else parameter_desc
      )
    
    # Append
    rv$dmr <- dplyr::bind_rows(rv$dmr, add_df)
    showNotification(paste0("Appended ", nrow(add_df), " rows to DMR."), type = "message")
  })
  
  # Manual upload
  observeEvent(input$standalone_upload, {
    showModal(modalDialog(fileInput("manual_file", "Choose CSV File"), footer = modalButton("Cancel")))
  })
  
  observeEvent(input$manual_file, {
    req(input$manual_file)
    uploaded_file <- read_csv(input$manual_file$datapath, show_col_types = FALSE)%>%
      mutate(monitoring_period_end_date = mdy(monitoring_period_end_date),
             dataset_source = "Manual Upload")
    
    rv$dmr <- bind_rows(rv$dmr,uploaded_file)
    removeModal()
    current_page("summary")
  })
  
  # --- OUTPUTS: Summary page
  output$map <- renderLeaflet({
    req(rv$selected_facility)
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addMarkers(lng = rv$selected_facility$FacLong, lat = rv$selected_facility$FacLat)%>%
      # 2. Add the control to toggle between them
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        options = layersControlOptions(collapsed = FALSE) # Keeps the menu open by default
      )
      
  })
  
  
  # Add more robust facility details
  
  output$facility_table <- renderDT({
    req(rv$selected_facility)
    datatable(rv$selected_facility %>% select(SourceID, CWPName, CWPState),
              options = list(dom = 't'))
  })
  
  # Define the coverage table as a reactive expression
  coverage_tbl_data <- reactive({
    req(rv$dmr, rv$crosswalk, input$npdes_forms)
    
    # 1. Map expected pollutants from the selected forms
    npdes_status <- rv$crosswalk %>%
      select(NPDES_Pollutant, Form, parameter_code) %>%
      mutate(param_status = ifelse(is.na(parameter_code), "No Ref", parameter_code)) %>%
      select(Pollutant = NPDES_Pollutant, Form, `Ref Param` = param_status) %>%
      distinct()
    
    # 2. Count actual samples from the DMR data
    params_by_code <- rv$dmr %>%
      group_by(parameter_code) %>%
      summarise(`# Samples` = n(), .groups = "drop")
    
    # 3. Join them together
    npdes_status %>%
      left_join(params_by_code, by = c("Ref Param" = "parameter_code")) %>%
      mutate(`# Samples` = tidyr::replace_na(`# Samples`, 0))
  })
  
  output$coverage_summary <- renderDT({
    req(rv$parameter_status, coverage_tbl_data())
    
    # Use the reactive data frame we just defined
    df_display <- coverage_tbl_data() %>%
      left_join(rv$parameter_status %>% select(parameter_code, Unit_Status), 
                by = c("Ref Param" = "parameter_code")) %>%
      mutate(Unit_Status = coalesce(Unit_Status, "NO DATA"))%>%
      arrange(desc(`# Samples`))%>%
      select(!Form)%>%
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
  
  
  # --- RUN RP: compute canonicalization/MF/RWC, convert WQS and limits, build comparisons
  observeEvent(input$run_rp, {
    req(rv$dmr)
    cat("\nTrying to get metal IDs")
    
    # Filter rv$dmr to selected water classes
    
    # Get Criterion IDS for metals (match to current crosswalk)
    wqs_metals <- crosswalk %>%
      dplyr::select(CRITERION_ID, parameter_code) %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      dplyr::filter(
        CRITERION_ID %in% c("79228","79238","79236","79253","79248","79251","79264") &
          CRITERION_ID %in% rv$crosswalk$CRITERION_ID
      )
    
    cat("\nRetrieved metals crosswalk")
    
    cb <- rv$dmr %>%
      dplyr::filter(!parameter_code %in% c("00070","00010","00080","00400")) %>%
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
      all_metals_limits <- metal_limits_cache  # cache loaded at startup
      
      hardness_setting <- suppressWarnings(as.numeric(input$hardness_value))
      if (!is.finite(hardness_setting)) {
        showNotification("Invalid hardness value; using default 100 mg/L as CaCO3.", type = "warning")
        hardness_setting <- 100
      }
      
      limits_filt <- all_metals_limits %>%
        dplyr::filter(CRITERION_ID %in% metals_filt$CRITERION_ID) %>%
        dplyr::mutate(hardness_set = hardness_setting,
                      diff = abs(hardness_set - hardness)) %>%
        dplyr::group_by(CRITERION_ID) %>%
        dplyr::slice_min(diff, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::select(CRITERION_ID, limit)
      
      rv$hardness <- hardness_setting
    }
    
    cat("\n Made it past metals limits")
    
    # Calculate TAN Limits
    if (any(as.character(rv$crosswalk$CRITERION_ID) == "79613", na.rm = TRUE)) {
      pH    <- suppressWarnings(as.numeric(input$ph_value))
      tempC <- suppressWarnings(as.numeric(input$temp_value))
      
      if (!is.finite(pH) || !is.finite(tempC)) {
        showNotification("TAN limit not updated: provide numeric pH and Temperature.", type = "warning", duration = 8)
      } else {
        term1       <- 0.0278 / (1 + 10^(7.688 - pH))
        term2       <- 1.1994 / (1 + 10^(pH - 7.688))
        temp_factor <- 2.126 * 10^(0.028 * (20 - tempC))
        tan_mgN_L   <- (term1 + term2) * temp_factor
        
        # Overwrite in rv$crosswalk safely
        rv$crosswalk <- rv$crosswalk %>%
          dplyr::mutate(
            CRITERION_ID = as.character(CRITERION_ID),
            CRITERION_VALUE = dplyr::if_else(CRITERION_ID == "79613", tan_mgN_L, CRITERION_VALUE)
          )
      }
    }
    
    cat("\n Joining rwc to criterion IDS")
    
    # Build base rwc_criteria (crosswalk x rwc by parameter_code)
    rwc_criteria <- dplyr::select(
      rv$crosswalk,
      NPDES_Pollutant, CRITERION_ID, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC
    ) %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
      dplyr::left_join(rwc, by = "parameter_code") %>%
      dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID))
    
    # pH findings
    pH_findings <- NULL
    if ("00400" %in% rv$dmr$parameter_code) {
      ph_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code == "00400") %>%
        dplyr::select(parameter_code, perm_feature_nmbr,dmr_value_nmbr,dmr_unit_desc,limit_value_nmbr,limit_begin_date,limit_end_date,statistical_base_type_code, monitoring_period_end_date)%>%
        mutate(NPDES_Pollutant = ifelse(statistical_base_type_code == "MAX", "pH (maximum)",
                                        ifelse(statistical_base_type_code== "MIN","pH (minimum)",NA)))%>%
        drop_na()
      
      rv$ph_dmr <- ph_dmr
      
      pH_findings <- rv$crosswalk %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::filter(CRITERION_ID %in% c("79595","79606") & parameter_code == "00400")%>%
        dplyr::select(NPDES_Pollutant, CRITERION_ID, Method, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
        dplyr::left_join(ph_dmr, by = c("parameter_code","NPDES_Pollutant"))%>%
        drop_na()%>%
        group_by(NPDES_Pollutant,CRITERION_ID,Method,CRITERION_VALUE,UNIT_NAME,parameter_code,USE_CLASS_NAME_LOCATION_ETC,perm_feature_nmbr)%>%
        summarise(max_value = max(dmr_value_nmbr, na.rm = TRUE),
                  min_value = min(dmr_value_nmbr, na.rm = TRUE),
                  mean_value = mean(dmr_value_nmbr, na.rm = TRUE))%>%
        ungroup()%>%
        mutate(RP = ifelse(Method == "Max" & max_value > CRITERION_VALUE,"YES",
                           ifelse(Method == "Min" & min_value < CRITERION_VALUE,"YES","NO")),
               RWC_rs = ifelse(Method == "Max",max_value,min_value))
    }
    
    # Temperature findings
    tempFindings <- NULL
    if ("00010" %in% rv$dmr$parameter_code) {
      temp_dmr <- rv$dmr %>%
        dplyr::filter(parameter_code == "00010") %>%
        mutate(NPDES_Pollutant = "Temperature")%>%
        dplyr::select(NPDES_Pollutant, parameter_code, perm_feature_nmbr,dmr_value_nmbr,dmr_unit_desc,limit_value_nmbr,limit_begin_date,limit_end_date,statistical_base_type_code, monitoring_period_end_date)%>%
        drop_na()
      
      tempFindings <- rv$crosswalk %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::filter(CRITERION_ID == "79218") %>%
        dplyr::select(CRITERION_ID, Method, CRITERION_VALUE, UNIT_NAME, parameter_code, USE_CLASS_NAME_LOCATION_ETC) %>%
        dplyr::left_join(temp_dmr, by = "parameter_code") %>%
        group_by(NPDES_Pollutant, CRITERION_ID,Method,CRITERION_VALUE,UNIT_NAME,parameter_code,USE_CLASS_NAME_LOCATION_ETC,perm_feature_nmbr)%>%
        summarise(min_value = min(dmr_value_nmbr, na.rm = TRUE),
                  mean_value = mean(dmr_value_nmbr, na.rm = TRUE),
                  max_value = max(dmr_value_nmbr, na.rm = TRUE))%>%
        ungroup()%>%
        mutate(RP = ifelse(max_value > CRITERION_VALUE,"YES", "NO"),
               RWC_rs = max_value)
      
      rv$temp_dmr <- temp_dmr
      
    }
    
    cat("\n joining metals")
    
    # Join numeric hardness-based limits into rwc_criteria (no range placeholders)
    if (nrow(metals_filt) > 0) {
      rwc_criteria <- rwc_criteria %>%
        dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)) %>%
        dplyr::left_join(
          limits_filt %>% dplyr::mutate(CRITERION_ID = as.character(CRITERION_ID)),
          by = "CRITERION_ID"
        ) %>%
        dplyr::mutate(
          CRITERION_VALUE = dplyr::if_else(!is.na(limit), limit, CRITERION_VALUE)
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
    
    
    cat(paste0("\n rwc_criteria columns: ",paste(colnames(rwc_criteria),collapse = ", "),"\n"))
    # Base RP (YES/NO)
    rp_concentration_findings <- rwc_criteria %>%
      #dplyr::left_join(max_dmr_vals, by = "parameter_code")%>%
      dplyr::mutate(RP = ifelse(RWC_rs > CRITERION_VALUE, "YES", "NO"))
    
    cat(paste0("\ncolumns in joined data: ",paste(colnames(rp_concentration_findings), collapse = ", ")))
    
      
    
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
    
    updateSelectInput(session, "selected_outfall",
                      choices = outfalls,
                      selected = outfalls[[1]])
    
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
        RP = factor(RP, levels = c("YES","NO"), ordered = TRUE),
        RWC_rs = round(RWC_rs, 2),
        CRITERION_VALUE = round(CRITERION_VALUE, 2)
      ) %>%
      dplyr::select(
        perm_feature_nmbr,
        CRITERION_ID, NPDES_Pollutant, CRITERION_VALUE, UNIT_NAME,
        USE_CLASS_NAME_LOCATION_ETC, RWC_rs, RP
      ) %>%
      dplyr::arrange(desc(RP)) %>%
      tidyr::drop_na(RWC_rs)%>%
      distinct()
  })
  
  # Render the table with row selection enabled
  output$rp_table <- DT::renderDT({
    req(rp_table_data())
    DT::datatable(
      rp_table_data(),
      colnames = c("Outfall","WQS ID","Pollutant","WQS","Unit","Water Class","RWC","RP"),
      selection = "single",        # enable row click selection
      rownames  = FALSE,
      options = list(
        order = list(list(7, "asc")),
        pageLength = 15,
        scrollX    = TRUE,
        rowCallback = JS(
          "function(row, data) {",
          "  var rpVal = data[7];", 
          "  if (rpVal === 'YES') {",
          "    $('td:eq(7)', row).css({'font-weight': 'bold'});",
          "  }",
          "}"
        )
      )
    )
  })
  
  # MATH
  # Tab 1: Plot Math
  output$math_inspector <- renderUI({
    req(input$selected_pollutant, input$selected_outfall, rv$rp_concentration)
    
    f <- rv$rp_concentration %>%
      dplyr::filter(NPDES_Pollutant == input$selected_pollutant,
                    perm_feature_nmbr == input$selected_outfall) %>%
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
      dplyr::filter(NPDES_Pollutant == pollutant,
                    perm_feature_nmbr == input$selected_outfall) %>%
      dplyr::slice(1)
    
    req(nrow(f) > 0, is.finite(f$MF))
    render_math_formula(f, input$rp_dr %||% 1, input$confidence_level)
  })
  
  
  # output$rp_table_summary <- renderDT({
  #   req(rv$rp_comparisons)
  #   datatable(rv$rp_comparisons,
  #             options = list(pageLength = 15, scrollX = TRUE))
  # })
  # 
  # output$flags_table <- renderDT({
  #   req(rv$data_flags_all)
  #   datatable(rv$data_flags_all,
  #             options = list(pageLength = 15, scrollX = TRUE))
  # })
  # 
  # output$finding_header <- renderUI({
  #   req(rv$rp_comparisons)
  #   if (any(rv$rp_comparisons$compare_group == "WQS" & rv$rp_comparisons$status_upper == "Exceedance", na.rm = TRUE) ||
  #       any(rv$rp_comparisons$compare_group == "Permit" & rv$rp_comparisons$permit_status_upper == "Exceedance", na.rm = TRUE)) {
  #     div(class = "alert alert-danger", h2("Exceedances Detected"))
  #   } else {
  #     div(class = "alert alert-success", h2("No Exceedances Found"))
  #   }
  # })
  
  # --- INTERACTIVE INSPECTOR LOGIC ---
  
  # 1. Update Pollutant Selection
  # Update Pollutant Selection based on selected outfall
  observe({
    req(rv$rp_concentration)
    data <- rv$rp_concentration
    if (!is.null(input$selected_outfall) && nzchar(input$selected_outfall)) {
      data <- data %>% dplyr::filter(perm_feature_nmbr == input$selected_outfall)
    }
    available <- data %>%
      dplyr::filter(RWC_rs > 0 | parameter_code %in% c("00400","00010")) %>%
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
    unit_colors <- c("deg C"="#1b9e77","mg/L"="#d95f02","SU"="#7570b3","mL/L"="#e7298a","ug/L"="#66a61e")
    class_colors <- c("class SB waters"="#1f78b4","class SD waters- drinking water"="#33a02c",
                      "class SG waters- drinking water"="#e31a1c","class SD waters"="#ff7f00",
                      "class SG waters"="#6a3d9a","surface waters"="#b2df8a")
    
    # Subset findings to pollutant + outfall
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(NPDES_Pollutant == input$selected_pollutant,
                    perm_feature_nmbr == input$selected_outfall)
    req(nrow(sub_rp) > 0)
    param_code <- sub_rp$parameter_code[1]
    
    # Observations for that parameter + outfall
    obs <- rv$dmr %>%
      dplyr::filter(parameter_code == param_code,
                    perm_feature_nmbr == input$selected_outfall)%>%
      dplyr::mutate(
        monitoring_period_end_date =
          if (inherits(monitoring_period_end_date, "Date")) {
            monitoring_period_end_date
          } else {
            suppressWarnings(lubridate::mdy(monitoring_period_end_date))
          }
      )%>%
      dplyr::arrange(monitoring_period_end_date)
    req(nrow(obs) > 0)
    
    # If plotting pH, subset to max or min
    if(input$selected_pollutant == "pH (maximum)"){
      obs <- rv$ph_dmr%>%
        filter(statistical_base_type_code == "MAX" & perm_feature_nmbr == input$selected_outfall)%>%
        dplyr::mutate(
          monitoring_period_end_date =
            if (inherits(monitoring_period_end_date, "Date")) {
              monitoring_period_end_date
            } else {
              suppressWarnings(lubridate::mdy(monitoring_period_end_date))
            }
        )
      cat("\nSelected ph Maximum, found: ",nrow(obs),"rows\n")
    }
    
    if(input$selected_pollutant == "pH (minimum)"){
      obs <- rv$ph_dmr%>%
        filter(statistical_base_type_code == "MIN" & perm_feature_nmbr == input$selected_outfall)%>%
        dplyr::mutate(
          monitoring_period_end_date =
            if (inherits(monitoring_period_end_date, "Date")) {
              monitoring_period_end_date
            } else {
              suppressWarnings(lubridate::mdy(monitoring_period_end_date))
            }
        )
      
      cat("\nSelected ph Minimum, found: ",nrow(obs),"rows\n")
    }
    
    # If plotting temperature, pull from temp_dmr
    if(input$selected_pollutant == "Temperature"){
      obs <- rv$temp_dmr%>%
        filter(perm_feature_nmbr == input$selected_outfall)%>%
        dplyr::mutate(
          monitoring_period_end_date =
            if (inherits(monitoring_period_end_date, "Date")) {
              monitoring_period_end_date
            } else {
              suppressWarnings(lubridate::mdy(monitoring_period_end_date))
            }
        )
    }
    
    
    # Build Line Data (WQS, RWC, and Permit Limits)
    rwc_val <- sub_rp$RWC_rs[1]
    wqs_lines <- sub_rp %>% dplyr::select(USE_CLASS_NAME_LOCATION_ETC, CRITERION_VALUE) %>% dplyr::distinct()
    
    # Compute “hardness where limit ≈ RWC” if user is in hardness range mode AND this pollutant is a metal under Class SD
    hardness_note <- NULL
    if (isTRUE(input$hardness_show_range) && is.finite(rwc_val)) {
      metals_ids <- c("79228","79238","79236","79253","79248","79251","79264")
      sub_sd_metal <- sub_rp %>%
        dplyr::filter(CRITERION_ID %in% metals_ids,
                      USE_CLASS_NAME_LOCATION_ETC == "class SD waters")
      if (nrow(sub_sd_metal) > 0 && exists("metal_limits_cache", inherits = TRUE) &&
          !is.null(metal_limits_cache) && nrow(metal_limits_cache) > 0) {
        crit <- sub_sd_metal$CRITERION_ID[1]
        limits_df <- metal_limits_cache %>%
          dplyr::filter(CRITERION_ID == crit)
        if (nrow(limits_df) > 0) {
          h_row <- limits_df %>%
            dplyr::mutate(diff = abs(limit - rwc_val)) %>%
            dplyr::slice_min(diff, n = 1, with_ties = FALSE)
          if (nrow(h_row) == 1 && is.finite(h_row$hardness)) {
            hardness_note <- paste0("RWC would exceed WQS at Hardness values < ",
                                    round(h_row$hardness, 0))
          }
        }
      }
    }
    
    # TAN overlay: if this pollutant has CRITERION_ID "79613", add time-varying TAN limit line
    tan_overlay <- NULL
    if (!is.null(rv$tan_limits) && nrow(sub_rp) > 0 && "79613" %in% sub_rp$CRITERION_ID) {
      tan_overlay <- rv$tan_limits %>%
        dplyr::filter(perm_feature_nmbr == input$selected_outfall) %>%
        dplyr::mutate(monitoring_period_end_date = lubridate::as_date(monitoring_period_end_date)) %>%
        dplyr::arrange(monitoring_period_end_date)
    }
    
    # Inside output$pollutant_plotly
    ## Find lowest value for axis
    vals <- c(obs$dmr_value_nmbr,rwc_val,wqs_lines$CRITERION_VALUE)
    min_val <- min(vals)-1
    
    
    p <- ggplot(obs, aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
      geom_line(color = "lightgrey", linetype = "dotted", alpha = 0.5) +
      geom_hline(aes(yintercept = rwc_val, color = "Calculated RWC"),
                 linetype = "solid", linewidth = 1) +
      geom_hline(data = wqs_lines,
                 aes(yintercept = CRITERION_VALUE, color = USE_CLASS_NAME_LOCATION_ETC),
                 linetype = "dashed", linewidth = 0.7) +
      geom_point(aes(color = dmr_unit_desc,
                     text = paste0("Date: ", monitoring_period_end_date,
                                   "<br>Value: ", round(dmr_value_nmbr, 4), " ", dmr_unit_desc,
                                   "<br>Limit: ", limit_value_nmbr)), size = 2) +
      scale_y_continuous(limits = c(min_val, NA)) +
      scale_color_manual(
        name = "Legend",
        values = c(class_colors, "Calculated RWC" = "#37493b", unit_colors)
      ) +
      labs(title = paste("Analysis for", input$selected_pollutant, "– Outfall", input$selected_outfall),
           x = "Date", y = sub_rp$UNIT_NAME[1]) +
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
        p <- p + annotate(
          "text",
          x = x_pos, y = y_pos,
          label = hardness_note,
          hjust = 0.5, vjust = 0,
          size = 3.5, color = "#2c7fb8"
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
    
    metals_ids <- c("79228","79238","79236","79253","79248","79251","79264")
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(NPDES_Pollutant == input$selected_pollutant,
                    perm_feature_nmbr == input$selected_outfall)
    has_metal <- any(sub_rp$CRITERION_ID %in% metals_ids, na.rm = TRUE)
    has_sd    <- any(sub_rp$USE_CLASS_NAME_LOCATION_ETC == "class SD waters", na.rm = TRUE)
    
    if (isTRUE(has_metal) && isTRUE(has_sd)) {
      tabsetPanel(
        tabPanel("Observed vs WQS/RWC", plotlyOutput("pollutant_plotly", height = "600px")),
        tabPanel("Hardness vs Limit",   plotlyOutput("metal_hardness_plotly", height = "600px"))
      )
    } else {
      plotlyOutput("pollutant_plotly", height = "600px")
    }
  })
  

  output$metal_hardness_plotly <- renderPlotly({
    req(input$selected_pollutant, input$selected_outfall, rv$rp_concentration)
    req(identical(input$hardness_mode, "range"))
    req(!is.null(metal_limits_cache) && nrow(metal_limits_cache) > 0)
    
    metals_ids <- c("79228","79238","79236","79253","79248","79251","79264")
    
    sub_rp <- rv$rp_concentration %>%
      dplyr::filter(NPDES_Pollutant == input$selected_pollutant,
                    perm_feature_nmbr == input$selected_outfall,
                    USE_CLASS_NAME_LOCATION_ETC == "class SD waters",
                    CRITERION_ID %in% metals_ids)
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
        title = paste("Hardness vs Limit –", input$selected_pollutant, "(Class SD)"),
        x = "Hardness (mg/L as CaCO3)",
        y = ylab
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 3. Dynamic Sidebar Stats Card
  # Replace your current output$pollutant_stats_card with this version to include "Prior Limit"
  
  output$pollutant_stats_card <- renderUI({
    req(input$selected_pollutant, input$selected_outfall, rv$rp_concentration, rv$dmr)
    
    # All findings for this pollutant × outfall (may include multiple water classes)
    
    
    sub <- rv$rp_concentration %>%
      dplyr::filter(NPDES_Pollutant == input$selected_pollutant,
                    perm_feature_nmbr == input$selected_outfall)
    req(nrow(sub) > 0)
    
    # Use a representative row for parameter_code/units/etc. (MF/RWC are the same across classes)
    f <- sub %>% dplyr::slice(1)
    
    ## Define DMR based on concentration or other
    if(input$selected_pollutant == "pH (minimum)"){
      dmr_sel <- rv$ph_dmr%>%
        filter(NPDES_Pollutant == "pH (minimum)" & perm_feature_nmbr == input$selected_outfall)
    } else if(input$selected_pollutant == "pH (maximum)"){
      dmr_sel <- rv$ph_dmr%>%
        filter(NPDES_Pollutant == "pH (maximum)" & perm_feature_nmbr == input$selected_outfall)
    } else if(input$selected_pollutant == "Temperature"){
      dmr_sel <- rv$temp_dmr%>%
        filter(NPDES_Pollutant == "Temperature" & perm_feature_nmbr == input$selected_outfall)
    } else{
      dmr_sel <-  rv$dmr
    }
    
    
    # Sample count for selected parameter + outfall
    n_samps <- dmr_sel %>%
      dplyr::filter(parameter_code == f$parameter_code,
                    perm_feature_nmbr == input$selected_outfall) %>%
      nrow()
    
    # Prior limit from DMR (same field used in tooltips)
    lim_row <- dmr_sel %>%
      dplyr::filter(parameter_code == f$parameter_code,
                    perm_feature_nmbr == input$selected_outfall) %>%
      dplyr::mutate(
        begin = suppressWarnings(lubridate::mdy(limit_begin_date)),
        end   = suppressWarnings(lubridate::mdy(limit_end_date)),
        lim_val_num = suppressWarnings(as.numeric(limit_value_nmbr))
      ) %>%
      dplyr::filter(is.finite(lim_val_num) & lim_val_num > 0) %>%
      dplyr::arrange(dplyr::desc(begin), dplyr::desc(end)) %>%
      dplyr::slice(1)
    
    prior_limit_txt <- if (nrow(lim_row) == 1) {
      paste0(round(lim_row$lim_val_num, 3), " ", lim_row$limit_unit_desc %||% "")
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
      tags$li(span(paste0(cls, ": ", val), style = paste0("color:", col, "; font-weight:bold;")))
    })
    
    wellPanel(
      h5("Quick Stats"),
      tags$b("Outfall: "), input$selected_outfall, br(),
      tags$b("Samples: "), n_samps, br(),
      tags$b("Max Value: "), round(f$max_value, 3), " ", f$UNIT_NAME, br(),
      tags$b("Prior Limit: "), prior_limit_txt, br(),
      tags$b("RWC: "), round(f$RWC_rs, 3), br(),
      tags$b("Worst-case RP: "),
      span(worst, style = paste0("color:", worst_color, "; font-weight:bold;")),
      if (nrow(rp_by_class) > 1) tagList(
        tags$hr(),
        tags$b("RP by Water Class:"),
        tags$ul(style="margin: 4px 0 0 18px; padding: 0;", items),
        if (any(rp_by_class$RP == "DEPENDS", na.rm = TRUE)) div(
          style="margin-top:6px; font-size: 90%;",
          "Hardness range selected for metals; see 'Hardness vs Limit' tab for thresholds."
        )
      )
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
        coverage_tbl <- tryCatch({
          if (exists("coverage_tbl_data") && is.function(coverage_tbl_data)) {
            coverage_tbl_data()
          } else {
            npdes_status <- rv$crosswalk %>%
              left_join(npdes_forms, by = c("NPDES_Pollutant" = "Pollutant")) %>%
              select(NPDES_Pollutant, Form, parameter_code) %>%
              filter(Form %in% input$npdes_forms) %>%
              mutate(param_status = ifelse(is.na(parameter_code), "No Ref", parameter_code)) %>%
              select(Pollutant = NPDES_Pollutant, Form, `Ref Param` = param_status) %>%
              distinct()
            
            params_by_code <- rv$dmr %>%
              group_by(parameter_code) %>%
              summarise(`# Samples` = n(), .groups = "drop")
            
            npdes_status %>%
              left_join(params_by_code, by = c("Ref Param" = "parameter_code")) %>%
              mutate(`# Samples` = tidyr::replace_na(`# Samples`, 0))
          }
        }, error = function(e) NULL)
        
        # 3. Write Data Files to Temp Dir
        report_src <- normalizePath("report.qmd", mustWork = TRUE)
        file.copy(report_src, file.path(tmp_dir, "report.qmd"), overwrite = TRUE)
        
        
        readr::write_csv(rv$rp_concentration, file.path(tmp_dir, "rp_concentration.csv"))
        readr::write_csv(rv$dmr, file.path(tmp_dir, "dmr.csv"))
        
        ph_path <- ""
        if (exists("rv") && is.data.frame(rv$ph_dmr) && nrow(rv$ph_dmr) > 0) {
          ph_path <- "ph_dmr.csv"
          readr::write_csv(rv$ph_dmr, file.path(tmp_dir, ph_path))
        }
        
        temp_path <- ""
        if (exists("rv") && is.data.frame(rv$temp_dmr) && nrow(rv$temp_dmr) > 0) {
          temp_path <- "temp_dmr.csv"
          readr::write_csv(rv$temp_dmr, file.path(tmp_dir, temp_path))
        }
        
        if (is.data.frame(coverage_tbl) && nrow(coverage_tbl) > 0) {
          readr::write_csv(coverage_tbl, file.path(tmp_dir, "coverage.csv"))
        }
        
        wqs_info_df <- rv$crosswalk %>%
          dplyr::select(CRITERION_ID, CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER, USE_CLASS_NAME_LOCATION_ETC)
        readr::write_csv(wqs_info_df, file.path(tmp_dir, "wqs_info.csv"))
        
        setProgress(value = 0.8, message = "Rendering PDF...")
        
        # 4. Render the PDF
        out_name <- "report.pdf"
        quarto::quarto_render(
          input         = file.path(tmp_dir, "report.qmd"),
          output_format = "pdf",
          output_file   = out_name,
          execute_params = list(
            permit_id         = input$permit_id %||% "",
            facility_name     = rv$selected_facility$CWPName %||% "",
            date_start        = as.character(input$date_start),
            date_end          = as.character(input$date_end),
            forms             = paste(input$npdes_forms, collapse = ", "),
            dilution_ratio    = input$rp_dr %||% 1,
            confidence_level  = input$confidence_level %||% 0.95,
            target_percentile = input$target_percentile %||% 0.95,
            hardness_mode     = input$hardness_mode %||% "range",
            hardness_value    = input$hardness_value %||% NA_real_,
            rp_path           = "rp_concentration.csv",
            coverage_path     = if (file.exists(file.path(tmp_dir, "coverage.csv"))) "coverage.csv" else "",
            dmr_path          = "dmr.csv",
            ph_dmr_path       = ph_path,
            temp_dmr_path     = temp_path,
            wqs_info_path     = "wqs_info.csv"
          ),
          quiet = FALSE
        )
        
        # 5. Delivery Logic
        if (!isTRUE(input$include_data)) {
          file.copy(file.path(tmp_dir, out_name), file, overwrite = TRUE)
        } else {
          setProgress(value = 0.9, message = "Packaging Data...")
          
          
          # Create summary findings table
          sf_tbl <- rv$rp_concentration%>%
            select(perm_feature_nmbr,NPDES_Pollutant,UNIT_NAME,n_used,min_value,mean_value,max_value,CRITERION_VALUE,RWC_rs,RP)%>%
            filter(RWC_rs > 0)
          
          # Build Excel
          xlsx_name <- "data.xlsx"
          if (requireNamespace("writexl", quietly = TRUE)) {
            sheets <- list(
              Summary_Findings = sf_tbl,
              RP_Concentration = as.data.frame(rv$rp_concentration),
              DMR              = as.data.frame(rv$dmr),
              Coverage         = if (!is.null(coverage_tbl)) as.data.frame(coverage_tbl) else NULL,
              WQS_Info         = as.data.frame(wqs_info_df)
            )
            sheets <- sheets[!vapply(sheets, is.null, logical(1))]
            writexl::write_xlsx(sheets, file.path(tmp_dir, xlsx_name))
          }
          
          # Build README
          readme_name <- "README.txt"
          cat(paste0("RP Report Package\n-----------------\nGenerated:", format(Sys.time()),"\n",
                     "NPDES Permit ID: ",input$permit_id,"\n",
                     "----------------------------------\n",
                     "REPORT SETTINGS:\n ",
                     "Dates Queried: ",input$date_start," to ",input$date_end,"\n",
                     "Hardness: ",rv$hardness), file = file.path(tmp_dir, readme_name))
          
          # ZIP (Strict pathing)
          owd <- setwd(tmp_dir)
          on.exit(setwd(owd), add = TRUE)
          
          files_to_zip <- c(out_name, readme_name)
          if (file.exists(xlsx_name)) files_to_zip <- c(files_to_zip, xlsx_name)
          
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