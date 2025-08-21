# combined_ppp_radars_postgres_module.R
# Professional PostgreSQL-based VALD Assessment Module

# Load libraries
library(tidyverse)
library(shiny)
library(fmsb)
library(DBI)
library(RPostgres)
library(ggplot2)

# Database connection parameters
DB_CONFIG <- list(
  host = "pitch-modeling.cvoesgo8szjk.us-east-2.rds.amazonaws.com",
  port = 5432,
  dbname = "postgres",
  user = "premier3",
  password = "premier3"
)

# Metric column mapping (database names to internal names)
metric_mapping <- list(
  "bodyweight_in_kilograms" = "bw_kg",
  "net_peak_vertical_force" = "net_peak_vertical_force_n",
  "jump_height_imp_mom_in_inches" = "jump_height_imp_mom_in_inches_in",
  "p1_concentric_impulse" = "p1_concentric_impulse_n_s",
  "peak_power" = "peak_power_w",
  "rsi_modified" = "rsi_modified_m_s",
  "eccentric_deceleration_impulse" = "eccentric_deceleration_impulse_n_s",
  "eccentric_deceleration_rfd" = "eccentric_deceleration_rfd_n_s",
  "eccentric_peak_force" = "eccentric_peak_force_n",
  "force_at_zero_velocity" = "force_at_zero_velocity_n",
  "countermovement_depth" = "countermovement_depth_cm",
  "braking_phase_duration" = "braking_phase_duration_ms",
  "eccentric_duration" = "eccentric_duration_ms",
  "concentric_peak_force" = "concentric_peak_force_n",
  "concentric_duration" = "concentric_duration_ms",
  "concentric_peak_velocity" = "concentric_peak_velocity_m_s",
  "concentric_impulse_100ms" = "concentric_impulse_100ms_n_s",
  "concentric_impulse" = "concentric_impulse_n_s",
  "eccentric_braking_impulse" = "eccentric_braking_impulse_n_s",
  "contraction_time" = "contraction_time_ms",
  "eccentric_braking_rfd" = "eccentric_braking_rfd_n_s",
  "relative_ppp" = "relative_PPP",
  "relative_eccentric_peak_force" = "relative_eccentric_peak_force"
)

# Available metrics in database (subset that exists)
available_metrics <- c(
  "bodyweight_in_kilograms",
  "net_peak_vertical_force",
  "jump_height_imp_mom_in_inches",
  "peak_power",
  "rsi_modified",
  "eccentric_peak_force",
  "force_at_zero_velocity",
  "countermovement_depth",
  "braking_phase_duration",
  "eccentric_duration",
  "concentric_peak_force",
  "concentric_duration",
  "concentric_peak_velocity",
  "concentric_impulse_100ms",
  "concentric_impulse",
  "eccentric_braking_impulse",
  "contraction_time",
  "eccentric_braking_rfd",
  "eccentric_deceleration_impulse",
  "eccentric_deceleration_rfd",
  "relative_ppp",
  "relative_eccentric_peak_force"
)

# Friendly choices for UI (filtered to available metrics)
friendly_choices <- c(
  "Body Weight (kg)" = "bodyweight_in_kilograms",
  "Jump Height (in)" = "jump_height_imp_mom_in_inches",
  "Peak Power (W)" = "peak_power",
  "RSI Modified" = "rsi_modified",
  "Ecc Peak Force (N)" = "eccentric_peak_force",
  "Force @ Zero V (N)" = "force_at_zero_velocity",
  "CM Depth (cm)" = "countermovement_depth",
  "Braking Duration (ms)" = "braking_phase_duration",
  "Ecc Duration (ms)" = "eccentric_duration",
  "Con Peak Force (N)" = "concentric_peak_force",
  "Con Duration (ms)" = "concentric_duration",
  "Con Peak Velo (m/s)" = "concentric_peak_velocity",
  "Con Imp @100ms (N·s)" = "concentric_impulse_100ms",
  "Con Impulse (N·s)" = "concentric_impulse",
  "Ecc Braking Imp (N·s)" = "eccentric_braking_impulse",
  "Contraction Time (ms)" = "contraction_time",
  "Ecc Braking RFD (N/s)" = "eccentric_braking_rfd",
  "Ecc Decel Imp (N·s)" = "eccentric_deceleration_impulse",
  "Ecc Decel RFD (N/s)" = "eccentric_deceleration_rfd",
  "Relative PPP (W/kg)" = "relative_ppp",
  "Rel Ecc Peak (N/kg)" = "relative_eccentric_peak_force"
)

# Default selected metrics
default_selected_metrics <- c(
  "peak_power",
  "rsi_modified",
  "concentric_impulse",
  "concentric_peak_velocity",
  "eccentric_peak_force",
  "eccentric_deceleration_rfd"
)

# ---------------------------------------------------------------------------------
# UI Module
combinedPPPRadarsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
      /* Global Styles */
      body { 
        background: #ffffff;
        color: #2c3e50;
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-size: 14px;
        margin: 0;
        padding: 0;
      }
      
      /* Remove all container padding to eliminate gaps */
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        background: #ffffff;
      }
      
      /* Sidebar layout adjustments */
      .row {
        margin: 0 !important;
      }
      
      .col-sm-4, .col-sm-8 {
        padding: 0 !important;
      }

      /* Professional sidebar with no gaps */
      .well {
        background: #ffffff;
        border: none;
        border-radius: 0;
        box-shadow: 2px 0 8px rgba(0, 0, 0, 0.05);
        padding: 0;
        margin: 0;
        height: 100vh;
        overflow-y: auto;
        border-right: 1px solid #e5e7eb;
      }

      /* Main title styling */
      .main-title {
        background: linear-gradient(135deg, #FFD700 0%, #FFC700 100%);
        text-align: center;
        padding: 24px 20px;
        margin: 0;
        font-size: 16px;
        font-weight: 600;
        color: #1a1a1a;
        letter-spacing: 0.5px;
        text-transform: uppercase;
        border-bottom: 2px solid #e6b800;
      }
      
      /* Sidebar content padding */
      .sidebar-content {
        padding: 24px;
        background: #ffffff;
      }

      /* Athlete selector */
      .athlete-selector {
        margin-bottom: 20px;
        padding-bottom: 20px;
        border-bottom: 1px solid #e5e7eb;
      }
      
      .athlete-selector label {
        font-weight: 600;
        color: #1f2937;
        margin-bottom: 8px;
        display: block;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      /* Section styling - collapsible */
      .filter-section {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        margin-bottom: 16px;
        overflow: visible;
        transition: all 0.3s ease;
      }
      
      .filter-section:hover {
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      }
      
      /* Section headers - clickable */
      .section-header {
        padding: 12px 16px;
        margin: 0;
        font-size: 13px;
        font-weight: 600;
        color: #1f2937;
        background: #f9fafb;
        border-bottom: 1px solid #e5e7eb;
        cursor: pointer;
        display: flex;
        justify-content: space-between;
        align-items: center;
        user-select: none;
        transition: background 0.2s ease;
      }
      
      .section-header:hover {
        background: #f3f4f6;
      }
      
      .section-header::after {
        content: '▼';
        font-size: 10px;
        color: #6b7280;
        transition: transform 0.3s ease;
      }
      
      .section-header.collapsed::after {
        transform: rotate(-90deg);
      }
      
      /* Section content */
      .section-content {
        padding: 16px;
        background: #ffffff;
      }

      /* Clean metric selector */
      .metric-selector {
        max-height: 200px;
        overflow-y: auto;
        border: 1px solid #e5e7eb;
        border-radius: 6px;
        padding: 8px;
        background: #f9fafb;
      }
      
      /* Custom scrollbar */
      .metric-selector::-webkit-scrollbar {
        width: 6px;
      }
      
      .metric-selector::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 3px;
      }
      
      .metric-selector::-webkit-scrollbar-thumb {
        background: #9ca3af;
        border-radius: 3px;
      }
      
      .metric-selector::-webkit-scrollbar-thumb:hover {
        background: #6b7280;
      }

      /* Trial selector with tags */
      .trial-tags {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        padding: 8px;
        background: #f9fafb;
        border: 1px solid #e5e7eb;
        border-radius: 6px;
        min-height: 60px;
      }
      
      .trial-tag {
        display: inline-flex;
        align-items: center;
        padding: 4px 10px;
        background: #FFD700;
        color: #1a1a1a;
        border-radius: 16px;
        font-size: 11px;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.2s ease;
      }
      
      .trial-tag:hover {
        background: #FFC700;
        transform: translateY(-1px);
      }
      
      .trial-tag.selected {
        background: #FFA500;
        box-shadow: 0 2px 4px rgba(255, 165, 0, 0.3);
      }
      
      /* Table trial selector */
      .table-trial-selector {
        display: flex;
        flex-direction: column;
        gap: 6px;
      }
      
      .table-trial-item {
        padding: 8px 12px;
        background: #f9fafb;
        border: 1px solid #e5e7eb;
        border-radius: 6px;
        font-size: 12px;
        cursor: pointer;
        transition: all 0.2s ease;
      }
      
      .table-trial-item:hover {
        background: #f3f4f6;
        border-color: #FFD700;
      }
      
      .table-trial-item.selected {
        background: #fef3c7;
        border-color: #FFD700;
        font-weight: 500;
      }

      /* Info text styling */
      .info-text {
        font-size: 11px;
        color: #6b7280;
        margin: 0 0 8px 0;
        font-style: italic;
      }

      /* Enhanced checkbox styling */
      .checkbox {
        margin: 4px 0;
        padding: 4px 8px;
        border-radius: 4px;
        transition: background-color 0.2s ease;
      }
      
      .checkbox:hover {
        background-color: #f0f0f0;
      }

      .checkbox input[type='checkbox'] {
        margin-right: 6px;
        cursor: pointer;
      }

      .checkbox label {
        font-weight: 400;
        margin-bottom: 0;
        cursor: pointer;
        font-size: 12px;
        color: #374151;
      }

      /* Form controls */
      .form-control, .selectize-input {
        font-size: 13px;
        padding: 8px 12px;
        border: 1px solid #e5e7eb;
        border-radius: 6px;
        background: white;
        transition: all 0.2s ease;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #FFD700;
        box-shadow: 0 0 0 2px rgba(255, 215, 0, 0.1);
        outline: none;
      }
      
      /* Select input styling */
      select.form-control {
        cursor: pointer;
      }

      /* Metric counter */
      .metric-counter {
        text-align: right;
        font-size: 11px;
        color: #6b7280;
        margin-top: 8px;
        padding: 4px 8px;
        background: #f9fafb;
        border-radius: 4px;
        display: inline-block;
      }
      
      .metric-counter.valid {
        background: #d4edda;
        color: #155724;
      }
      
      .metric-counter.invalid {
        background: #f8d7da;
        color: #721c24;
      }
      
      /* Download button */
      .download-btn {
        margin-top: 24px;
        background: linear-gradient(135deg, #FFD700 0%, #FFC700 100%);
        color: #1a1a1a;
        font-weight: 600;
        border: none;
        border-radius: 8px;
        padding: 12px 24px;
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(255, 215, 0, 0.3);
        width: 100%;
      }
      
      .download-btn:hover {
        background: linear-gradient(135deg, #FFC700 0%, #FFB700 100%);
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(255, 215, 0, 0.4);
        color: #1a1a1a;
      }
      
      /* Main panel styling - seamless */
      .main-panel-wrapper {
        background: #ffffff;
        padding: 20px;
        margin: 0;
        min-height: 100vh;
      }
      
      /* Remove any logo container issues */
      .logo-container {
        display: none !important;
      }
      
      /* Validation message styling */
      .validation-message {
        color: #dc2626;
        font-weight: 500;
        margin-top: 8px;
        font-size: 11px;
        padding: 6px 10px;
        background: #fef2f2;
        border-radius: 4px;
        border: 1px solid #fecaca;
      }
      
      /* Comparison toggle switches */
      .toggle-switch {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 8px 12px;
        background: #f9fafb;
        border-radius: 6px;
        margin-bottom: 8px;
        cursor: pointer;
        transition: background 0.2s ease;
      }
      
      .toggle-switch:hover {
        background: #f3f4f6;
      }
      
      .toggle-switch label {
        margin: 0;
        cursor: pointer;
        font-size: 12px;
      }
      
      /* Clean subsection headers */
      .subsection-header {
        font-size: 11px;
        font-weight: 600;
        color: #6b7280;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin: 16px 0 8px 0;
        padding-bottom: 4px;
        border-bottom: 1px solid #f3f4f6;
      }
    "))
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # Title integrated into sidebar
        div(
          class = "main-title",
          HTML("Premier Pitching Performance<br><small style='font-size: 12px; font-weight: 400;'>Force & Power Assessment</small>")
        ),
        
        div(
          class = "sidebar-content",
          
          # Athlete selector
          div(
            class = "athlete-selector",
            tags$label("Select Athlete", `for` = ns("select_athlete")),
            selectInput(ns("select_athlete"), NULL,
                        choices = NULL, width = "100%")
          ),
          
          # Velocity Predictions
          uiOutput(ns("predicted_velo_selector")),
          
          # Radar Chart Configuration
          div(
            class = "filter-section",
            div(class = "section-header collapsed", onclick = paste0("toggleSection('", ns("radar_section"), "')"),
                "RADAR CHART CONFIGURATION"),
            div(id = ns("radar_section"), class = "section-content", style = "display: none;",
                
                # Metrics selector
                div(class = "subsection-header", "Select Metrics (3-6)"),
                div(class = "metric-selector",
                    checkboxGroupInput(
                      ns("radar_metrics"),
                      NULL,
                      choices = friendly_choices,
                      selected = default_selected_metrics,
                      inline = FALSE
                    )
                ),
                div(id = ns("radar_counter"), class = "metric-counter",
                    textOutput(ns("radar_selection_count"), inline = TRUE)),
                
                conditionalPanel(
                  condition = paste0("output['", ns("radar_validation"), "']"),
                  div(class = "validation-message",
                      textOutput(ns("radar_validation_text")))
                ),
                
                # Test Trials
                div(class = "subsection-header", "Select Test Trials"),
                div(class = "trial-tags",
                    uiOutput(ns("radar_trial_selector"))
                ),
                
                # Comparisons
                div(class = "subsection-header", "Comparison Overlays"),
                div(
                  div(class = "toggle-switch",
                      checkboxInput(ns("show_avg_all"), "Database Average", value = FALSE, width = "100%")
                  ),
                  selectInput(ns("select_velocity_group"), "Velocity Group",
                              choices = c("None" = "All",
                                          "< 80 mph" = "<80",
                                          "80-85 mph" = "80-85",
                                          "85-90 mph" = "85-90",
                                          "> 90 mph" = ">90"),
                              selected = "All", width = "100%")
                )
            )
          ),
          
          # Trend Analysis
          div(
            class = "filter-section",
            div(class = "section-header collapsed", onclick = paste0("toggleSection('", ns("trend_section"), "')"),
                "TREND ANALYSIS"),
            div(id = ns("trend_section"), class = "section-content", style = "display: none;",
                p("Track metric progression over time", class = "info-text"),
                selectInput(ns("trend_metric"), "Select Metric",
                            choices = friendly_choices,
                            selected = default_selected_metrics[1],
                            width = "100%")
            )
          ),
          
          # Data Table Configuration
          div(
            class = "filter-section",
            div(class = "section-header collapsed", onclick = paste0("toggleSection('", ns("table_section"), "')"),
                "DATA TABLE CONFIGURATION"),
            div(id = ns("table_section"), class = "section-content", style = "display: none;",
                
                # Trial selector for table
                div(class = "subsection-header", "Select Trials (up to 3)"),
                div(class = "table-trial-selector",
                    uiOutput(ns("table_trial_selector"))
                ),
                
                # Metrics selector
                div(class = "subsection-header", "Select Metrics (up to 6)"),
                div(class = "metric-selector",
                    checkboxGroupInput(
                      ns("data_table_metrics"),
                      NULL,
                      choices = friendly_choices,
                      selected = c("bodyweight_in_kilograms", "peak_power", "rsi_modified",
                                   "countermovement_depth", "concentric_peak_velocity", "eccentric_peak_force"),
                      inline = FALSE
                    )
                ),
                div(id = ns("table_counter"), class = "metric-counter",
                    textOutput(ns("data_table_selection_count"), inline = TRUE)),
                
                conditionalPanel(
                  condition = paste0("output['", ns("data_table_validation"), "']"),
                  div(class = "validation-message",
                      textOutput(ns("data_table_validation_text")))
                )
            )
          ),
          
          downloadButton(ns("downloadPlot"), 
                         "DOWNLOAD REPORT", 
                         class = "download-btn")
        )
      ),
      
      mainPanel(
        width = 8,
        div(
          class = "main-panel-wrapper",
          plotOutput(ns("combinedVisualization"), height = "1200px")
        )
      )
    ),
    
    # JavaScript for collapsible sections
    tags$script(HTML("
      function toggleSection(sectionId) {
        var section = document.getElementById(sectionId);
        var header = section.previousElementSibling;
        if (section.style.display === 'none') {
          section.style.display = 'block';
          header.classList.remove('collapsed');
        } else {
          section.style.display = 'none';
          header.classList.add('collapsed');
        }
      }
    "))
  )
}

# ---------------------------------------------------------------------------------
# Server Module
combinedPPPRadarsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create database connection
    con <- reactive({
      dbConnect(RPostgres::Postgres(),
                host = DB_CONFIG$host,
                port = DB_CONFIG$port,
                dbname = DB_CONFIG$dbname,
                user = DB_CONFIG$user,
                password = DB_CONFIG$password)
    })
    
    # Clean up connection on session end
    onStop(function() {
      if (exists("con") && !is.null(con())) {
        dbDisconnect(con())
      }
    })
    
    # Initialize athlete choices from database
    observe({
      conn <- con()
      athletes <- dbGetQuery(conn, "
        SELECT DISTINCT athleteid, pitcher_name 
        FROM vald_assessments 
        WHERE testtype = 'CMJ' AND pitcher_name IS NOT NULL
        ORDER BY pitcher_name
      ")
      
      # Create named vector: display name = pitcher_name, value = athleteid
      athlete_choices <- setNames(athletes$athleteid, athletes$pitcher_name)
      
      updateSelectInput(
        session,
        "select_athlete",
        choices = athlete_choices,
        selected = athlete_choices[1]
      )
    })
    
    # Get all trials for selected athlete with trial numbering
    athlete_trials <- reactive({
      req(input$select_athlete)
      conn <- con()
      
      trials <- dbGetQuery(conn, "
        SELECT 
          trialid,
          athleteid,
          pitcher_name,
          recordedutc,
          bodyweight_in_kilograms,
          net_peak_vertical_force,
          jump_height_imp_mom_in_inches,
          peak_power,
          rsi_modified,
          eccentric_peak_force,
          force_at_zero_velocity,
          countermovement_depth,
          braking_phase_duration,
          eccentric_duration,
          concentric_peak_force,
          concentric_duration,
          concentric_peak_velocity,
          concentric_impulse_100ms,
          concentric_impulse,
          eccentric_braking_impulse,
          contraction_time,
          eccentric_braking_rfd,
          eccentric_deceleration_impulse,
          eccentric_deceleration_rfd,
          relative_ppp,
          relative_eccentric_peak_force,
          max_velo,
          predicted_max_velo
        FROM vald_assessments 
        WHERE athleteid = $1 AND testtype = 'CMJ'
        ORDER BY recordedutc
      ", params = list(input$select_athlete))
      
      # Add trial numbering within each day
      trials <- trials %>%
        mutate(
          test_date = as.Date(recordedutc),
          recordedutc = as.POSIXct(recordedutc)
        ) %>%
        group_by(test_date) %>%
        mutate(
          trial_num = row_number(),
          trial_label = paste0("Trial ", trial_num, " (", format(test_date, "%Y-%m-%d"), ")")
        ) %>%
        ungroup()
      
      return(trials)
    })
    
    # Get all CMJ data for percentile calculations
    all_cmj_data <- reactive({
      conn <- con()
      
      data <- dbGetQuery(conn, "
        SELECT 
          pitcher_name,
          recordedutc,
          bodyweight_in_kilograms,
          net_peak_vertical_force,
          jump_height_imp_mom_in_inches,
          peak_power,
          rsi_modified,
          eccentric_peak_force,
          force_at_zero_velocity,
          countermovement_depth,
          braking_phase_duration,
          eccentric_duration,
          concentric_peak_force,
          concentric_duration,
          concentric_peak_velocity,
          concentric_impulse_100ms,
          concentric_impulse,
          eccentric_braking_impulse,
          contraction_time,
          eccentric_braking_rfd,
          eccentric_deceleration_impulse,
          eccentric_deceleration_rfd,
          relative_ppp,
          relative_eccentric_peak_force,
          max_velo
        FROM vald_assessments 
        WHERE testtype = 'CMJ'
      ")
      
      return(data)
    })
    
    # Validation for radar metrics selection
    output$radar_validation <- reactive({
      length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6
    })
    outputOptions(output, "radar_validation", suspendWhenHidden = FALSE)
    
    output$radar_selection_count <- renderText({
      count <- length(input$radar_metrics)
      if(count >= 3 && count <= 6) {
        paste0("✓ ", count, " selected")
      } else {
        paste0("⚠ ", count, " selected (need 3-6)")
      }
    })
    
    output$radar_validation_text <- renderText({
      if (length(input$radar_metrics) < 3) {
        "Please select at least 3 metrics"
      } else if (length(input$radar_metrics) > 6) {
        "Please select no more than 6 metrics"
      }
    })
    
    # Validation for data table metrics selection
    output$data_table_validation <- reactive({
      length(input$data_table_metrics) > 6
    })
    outputOptions(output, "data_table_validation", suspendWhenHidden = FALSE)
    
    output$data_table_selection_count <- renderText({
      count <- length(input$data_table_metrics)
      if(count <= 6) {
        paste0("✓ ", count, " selected")
      } else {
        paste0("⚠ ", count, " selected (max 6)")
      }
    })
    
    output$data_table_validation_text <- renderText({
      if (length(input$data_table_metrics) > 6) {
        "Please select no more than 6 metrics"
      }
    })
    
    # Predicted velocity selector UI with enhanced styling
    output$predicted_velo_selector <- renderUI({
      req(athlete_trials())
      
      trials_with_pred <- athlete_trials() %>%
        filter(!is.na(predicted_max_velo)) %>%
        arrange(desc(recordedutc))
      
      if (nrow(trials_with_pred) > 0) {
        # Create labels with predicted and actual velocity
        choice_labels <- sapply(1:nrow(trials_with_pred), function(i) {
          trial <- trials_with_pred[i,]
          pv <- ifelse(is.na(trial$predicted_max_velo), "N/A", sprintf("%.1f", trial$predicted_max_velo))
          v <- ifelse(is.na(trial$max_velo), "N/A", sprintf("%.1f", trial$max_velo))
          paste0(trial$trial_label, " | PV: ", pv, " mph | V: ", v, " mph")
        })
        
        div(
          class = "filter-section",
          h4("Velocity Predictions", class = "section-header"),
          selectInput(session$ns("predicted_velo_trial"), NULL,
                      choices = setNames(trials_with_pred$trialid, choice_labels),
                      selected = trials_with_pred$trialid[1],
                      width = "100%")
        )
      } else {
        div(
          class = "filter-section",
          h4("Velocity Predictions", class = "section-header"),
          p("No velocity predictions available", 
            style = "font-size: 12px; color: #6b7280; text-align: center; padding: 10px;")
        )
      }
    })
    
    # Trial selector UI for radar plot
    output$radar_trial_selector <- renderUI({
      req(athlete_trials())
      
      trials <- athlete_trials()
      checkboxGroupInput(session$ns("radar_trials"), NULL,
                         choices = setNames(trials$trialid, trials$trial_label),
                         selected = trials$trialid[1:min(3, nrow(trials))])
    })
    
    # Trial selector UI for data table
    output$table_trial_selector <- renderUI({
      req(athlete_trials())
      
      trials <- athlete_trials()
      checkboxGroupInput(session$ns("table_trials"), NULL,
                         choices = setNames(trials$trialid, trials$trial_label),
                         selected = trials$trialid[1:min(3, nrow(trials))])
    })
    
    # Z-scores calculation (using overall mean/sd as reference)
    zdata <- reactive({
      df <- all_cmj_data()
      metrics <- available_metrics
      
      means <- sapply(df[metrics], mean, na.rm = TRUE)
      sds <- sapply(df[metrics], sd, na.rm = TRUE)
      sds[sds == 0] <- 1
      
      df %>%
        mutate(across(all_of(metrics),
                      ~ (. - means[cur_column()]) / sds[cur_column()],
                      .names = "{col}_z"))
    })
    
    # Percentile data for radar plot
    pctdata <- reactive({
      df <- all_cmj_data()
      metrics <- available_metrics
      
      df %>%
        mutate(across(all_of(metrics),
                      ~ percent_rank(.) * 100,
                      .names = "{col}_pct"))
    })
    
    # Create visualization function with professional styling
    create_visualization <- function() {
      req(input$select_athlete, input$radar_trials,
          input$data_table_metrics, input$trend_metric,
          input$radar_metrics)
      
      # Validate selections
      if (length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6) {
        plot.new()
        text(0.5, 0.5, "Please select 3-6 metrics for the radar plot",
             cex = 2, col = "#dc2626", font = 2, family = "sans")
        return()
      }
      
      if (length(input$data_table_metrics) > 6) {
        plot.new()
        text(0.5, 0.5, "Please select no more than 6 metrics for the data table",
             cex = 2, col = "#dc2626", font = 2, family = "sans")
        return()
      }
      
      # Setup layout with adjusted heights for better proportions
      layout(matrix(c(1,2,3,4), nrow=4, byrow=TRUE),
             heights=c(0.4, 3.2, 2.4, 2.5))
      
      # PART 1: Professional Header Banner
      par(mar=c(0,0,0,0))
      plot.new()
      
      # Get player name from the trials data
      player_name <- unique(athlete_trials()$pitcher_name)[1]
      
      # Create gradient-like effect with multiple rectangles
      rect(0, 0.7, 1, 1, col="#FFD700", border=NA)
      rect(0, 0.65, 1, 0.7, col="#FFC700", border=NA)
      rect(0, 0, 1, 0.65, col="#FFD700", border=NA)
      
      # Add player name with shadow effect
      text(0.5, 0.35, player_name, cex=3.5, font=2, family="sans", col="#00000033")  # Shadow
      text(0.498, 0.352, player_name, cex=3.5, font=2, family="sans", col="#1a1a1a")  # Main text
      
      # PART 2: Enhanced Radar Plot
      radar_metrics <- input$radar_metrics
      radar_pcols <- paste0(radar_metrics, "_pct")
      
      # Get percentile data
      pct_df <- pctdata()
      
      # Build radar data frame
      mins <- rep(0, length(radar_pcols))
      maxs <- rep(100, length(radar_pcols))
      df_radar <- as.data.frame(rbind(maxs, mins))
      rownames(df_radar) <- c("Max", "Min")
      colnames(df_radar) <- radar_pcols
      
      # Add selected trials
      trials_data <- athlete_trials()
      selected_trials <- trials_data %>% filter(trialid %in% input$radar_trials)
      
      for(i in 1:nrow(selected_trials)) {
        trial <- selected_trials[i, ]
        
        # Calculate percentiles for this trial
        trial_pcts <- numeric(length(radar_metrics))
        for(j in seq_along(radar_metrics)) {
          metric <- radar_metrics[j]
          val <- trial[[metric]]
          if(!is.na(val)) {
            all_vals <- all_cmj_data()[[metric]]
            trial_pcts[j] <- sum(all_vals <= val, na.rm = TRUE) / sum(!is.na(all_vals)) * 100
          } else {
            trial_pcts[j] <- 0
          }
        }
        
        df_radar <- rbind(df_radar, trial_pcts)
        rownames(df_radar)[nrow(df_radar)] <- trial$trial_label
      }
      
      # Add comparison overlays if requested
      if (input$show_avg_all) {
        avg_all <- colMeans(pct_df[radar_pcols], na.rm = TRUE)
        df_radar <- rbind(df_radar, avg_all)
        rownames(df_radar)[nrow(df_radar)] <- "Database Average"
      }
      
      if (input$select_velocity_group != "All") {
        velo_range <- switch(input$select_velocity_group,
                             "<80" = c(0, 80),
                             "80-85" = c(80, 85),
                             "85-90" = c(85, 90),
                             ">90" = c(90, 200))
        
        velo_subset <- pct_df %>%
          filter(max_velo >= velo_range[1], max_velo < velo_range[2])
        
        if(nrow(velo_subset) > 0) {
          avg_velo <- colMeans(velo_subset[radar_pcols], na.rm = TRUE)
          df_radar <- rbind(df_radar, avg_velo)
          rownames(df_radar)[nrow(df_radar)] <- paste0(input$select_velocity_group, " mph Average")
        }
      }
      
      # Professional color scheme
      series_names <- rownames(df_radar)[-c(1,2)]
      n_series <- length(series_names)
      
      # Create color palette
      colors <- character(n_series)
      fills <- character(n_series)
      ltys <- numeric(n_series)
      
      # Assign colors based on series type
      for (i in seq_along(series_names)) {
        if (grepl("Trial", series_names[i])) {
          # Use gradient of gold colors for trials
          gold_shades <- c("#FFD700", "#FFC700", "#FFB700")
          colors[i] <- gold_shades[((i - 1) %% 3) + 1]
          fills[i] <- scales::alpha(colors[i], 0.25)
          ltys[i] <- 1
        } else if (grepl("Database Average", series_names[i])) {
          colors[i] <- "#6b7280"
          fills[i] <- scales::alpha(colors[i], 0.15)
          ltys[i] <- 2
        } else if (grepl("mph Average", series_names[i])) {
          colors[i] <- "#3b82f6"
          fills[i] <- scales::alpha(colors[i], 0.15)
          ltys[i] <- 2
        }
      }
      
      # Get friendly names and format labels
      radar_friendly_names <- names(friendly_choices)[match(radar_metrics, friendly_choices)]
      formatted_radar_labels <- sapply(radar_friendly_names, function(name) {
        short_name <- gsub("\\s*\\(.*\\)\\s*$", "", name)
        return(short_name)
      })
      
      # Create radar plot with enhanced styling
      par(mar = c(2, 2, 4, 2), family="sans", bg="white")
      radarchart(df_radar,
                 axistype = 1,
                 pcol = colors,
                 pfcol = fills,
                 plwd = 3,
                 plty = ltys,
                 cglcol = "#9ca3af",
                 cglty = 1,
                 axislabcol = "#374151",
                 vlcex = 1.8,
                 cglcex = 1.4,
                 calcex = 1.4,
                 palcex = 1.4,
                 vlabels = formatted_radar_labels,
                 seg = 4)
      
      # Professional legend with background
      legend_x <- "topright"
      legend("topright", 
             legend = series_names, 
             col = colors, 
             lty = ltys,
             lwd = 3, 
             bty = "n", 
             cex = 1.5,
             text.col = "#374151")
      
      # Add predicted velocity text with styled background
      if(!is.null(input$predicted_velo_trial)) {
        velo_trial <- athlete_trials() %>%
          filter(trialid == input$predicted_velo_trial)
        
        if(nrow(velo_trial) > 0) {
          pv_text <- sprintf("%.1f", ifelse(is.na(velo_trial$predicted_max_velo), 0, velo_trial$predicted_max_velo))
          v_text <- sprintf("%.1f", ifelse(is.na(velo_trial$max_velo), 0, velo_trial$max_velo))
          
          combined_text <- paste0("Predicted: ", pv_text, " mph  |  Actual: ", v_text, " mph")
          
          # Add background box for text
          rect(0.25, -0.18, 0.75, -0.08, col = "#f3f4f6", border = "#e5e7eb", lwd = 1)
          mtext(combined_text, side = 3, line = 0.8, cex = 1.3, font = 2, family = "sans", col = "#1f2937")
        }
      }
      
      # PART 3: Professional Trend Plot
      # Average multiple trials per day for trend plot
      df_trend <- athlete_trials() %>%
        group_by(test_date) %>%
        summarise(
          across(all_of(available_metrics), ~ mean(.x, na.rm = TRUE)),
          .groups = 'drop'
        ) %>%
        arrange(test_date)
      
      y_lab <- names(friendly_choices)[match(input$trend_metric, friendly_choices)]
      
      par(mar=c(4,5,3,2), family="sans", bg="white")
      
      # Handle case where metric might be NA
      trend_vals <- df_trend[[input$trend_metric]]
      valid_idx <- !is.na(trend_vals)
      
      if(sum(valid_idx) > 0) {
        # Create plot with grid
        plot(df_trend$test_date[valid_idx], trend_vals[valid_idx],
             type = "n",
             xlab = "",
             ylab = "",
             main = paste0(y_lab, " Progression"),
             cex.main = 1.8, 
             font.main = 2,
             axes = FALSE)
        
        # Add custom grid
        grid(nx = NULL, ny = NULL, col = "#e5e7eb", lty = 1, lwd = 0.5)
        
        # Add axes with better styling
        axis.Date(1, at = df_trend$test_date[valid_idx],
                  format = "%m/%d/%y", 
                  cex.axis = 1.2,
                  col = "#6b7280",
                  col.axis = "#374151")
        axis(2, cex.axis = 1.2, 
             col = "#6b7280",
             col.axis = "#374151",
             las = 1)
        
        # Add axis labels
        mtext("Test Date", side = 1, line = 2.5, cex = 1.2, col = "#374151")
        mtext(y_lab, side = 2, line = 3.5, cex = 1.2, col = "#374151")
        
        # Add trend line with shadow
        lines(df_trend$test_date[valid_idx], trend_vals[valid_idx],
              lwd = 4, col = "#00000020")  # Shadow
        lines(df_trend$test_date[valid_idx], trend_vals[valid_idx],
              lwd = 3, col = "#FFD700")
        
        # Add points with border
        points(df_trend$test_date[valid_idx], trend_vals[valid_idx],
               pch = 21, cex = 2, 
               bg = "#FFD700", 
               col = "#1a1a1a",
               lwd = 2)
        
        # Add value labels
        for(i in which(valid_idx)) {
          text(df_trend$test_date[i], trend_vals[i],
               sprintf("%.1f", trend_vals[i]),
               pos = 3, offset = 0.5,
               cex = 0.9, font = 2, col = "#374151")
        }
        
        box(col = "#e5e7eb")
      } else {
        plot.new()
        text(0.5, 0.5, "No data available for trend", cex = 1.5, col = "#6b7280")
      }
      
      # PART 4: Professional Data Table
      chosen <- input$data_table_metrics
      
      selected_trials <- athlete_trials()
      if(!is.null(input$table_trials) && length(input$table_trials) > 0) {
        selected_trials <- selected_trials %>%
          filter(trialid %in% input$table_trials) %>%
          arrange(recordedutc)
      } else {
        selected_trials <- selected_trials %>%
          arrange(desc(recordedutc)) %>%
          slice_head(n = 3) %>%
          arrange(recordedutc)
      }
      
      if(nrow(selected_trials) > 0 && length(chosen) > 0) {
        # Prepare metrics data with rounding
        metrics_data <- selected_trials %>%
          select(trial_label, all_of(chosen))
        
        # Calculate z-scores
        scores_data <- data.frame(trial_label = selected_trials$trial_label)
        for(metric in chosen) {
          scores <- numeric(nrow(selected_trials))
          for(i in 1:nrow(selected_trials)) {
            val <- selected_trials[[metric]][i]
            if(!is.na(val)) {
              all_vals <- all_cmj_data()[[metric]]
              mean_val <- mean(all_vals, na.rm = TRUE)
              sd_val <- sd(all_vals, na.rm = TRUE)
              if(sd_val > 0) {
                scores[i] <- round(100 + ((val - mean_val) / sd_val) * 10)
              } else {
                scores[i] <- 100
              }
            } else {
              scores[i] <- NA
            }
          }
          scores_data[[metric]] <- scores
        }
        
        # Format for display with 2 decimal places
        metrics_display <- data.frame(
          Trial = metrics_data$trial_label,
          Type = "Raw"
        )
        for(metric in chosen) {
          metrics_display[[metric]] <- sprintf("%.2f", round(metrics_data[[metric]], 2))
        }
        
        scores_display <- data.frame(
          Trial = scores_data$trial_label,
          Type = "Score"
        )
        for(metric in chosen) {
          scores_display[[metric]] <- as.character(scores_data[[metric]])
        }
        
        combined_data <- rbind(metrics_display, scores_display)
        
        # Rename columns to friendly names
        friendly_names <- names(friendly_choices)[match(chosen, friendly_choices)]
        colnames(combined_data)[3:ncol(combined_data)] <- friendly_names
        
        # Setup plot area
        par(mar=c(1,1,2,1), xpd=TRUE, family="sans", bg="white")
        plot.new()
        
        # Table dimensions with better spacing
        n_rows <- nrow(combined_data)
        n_cols <- ncol(combined_data)
        
        y_pos_top <- 0.92
        row_height <- 0.13
        header_height <- row_height * 1.5
        y_pos_bottom <- y_pos_top - header_height - (row_height * n_rows)
        
        x_pos_left <- 0.02
        x_pos_right <- 0.98
        
        # Calculate column widths
        col_widths <- c(0.20, 0.08)  # Trial and Type columns
        remaining_width <- (x_pos_right - x_pos_left) - sum(col_widths)
        if (n_cols > 2) {
          metric_width <- remaining_width / (n_cols - 2)
          col_widths <- c(col_widths, rep(metric_width, n_cols - 2))
        }
        
        col_positions <- c(x_pos_left, x_pos_left + cumsum(col_widths)[-n_cols])
        
        # Draw table with enhanced styling
        # Main border with shadow effect
        rect(x_pos_left - 0.002, y_pos_bottom - 0.002, 
             x_pos_right + 0.002, y_pos_top + 0.002, 
             col = "#00000020", border = NA)
        rect(x_pos_left, y_pos_bottom, x_pos_right, y_pos_top, 
             col = "white", border = "#d1d5db", lwd = 2)
        
        # Header background with gradient effect
        rect(x_pos_left, y_pos_top - header_height, x_pos_right, y_pos_top,
             col = "#FFD700", border = NA)
        
        # Alternating row backgrounds
        for (i in seq(1, n_rows, by = 2)) {
          y_start <- y_pos_top - header_height - (i * row_height)
          y_end <- y_start + row_height
          rect(x_pos_left, y_start, x_pos_right, y_end,
               col = "#f9fafb", border = NA)
        }
        
        # Column separators
        for (j in 1:(n_cols-1)) {
          x_pos <- col_positions[j] + col_widths[j]
          lines(c(x_pos, x_pos), c(y_pos_bottom, y_pos_top), 
                col = "#e5e7eb", lwd = 1)
        }
        
        # Row separators
        lines(c(x_pos_left, x_pos_right), 
              c(y_pos_top - header_height, y_pos_top - header_height),
              col = "#d1d5db", lwd = 2)
        
        for (i in 1:(n_rows-1)) {
          y_pos <- y_pos_top - header_height - (i * row_height)
          lines(c(x_pos_left, x_pos_right), c(y_pos, y_pos),
                col = "#e5e7eb", lwd = 0.5)
        }
        
        # Column headers
        text(col_positions[1] + col_widths[1]/2, 
             y_pos_top - header_height/2,
             "Trial", font = 2, cex = 1.3, col = "#1a1a1a")
        
        text(col_positions[2] + col_widths[2]/2, 
             y_pos_top - header_height/2,
             "Type", font = 2, cex = 1.3, col = "#1a1a1a")
        
        # Metric headers
        if (n_cols >= 3) {
          for (j in 3:n_cols) {
            col_center <- col_positions[j] + col_widths[j]/2
            category <- friendly_names[j-2]
            
            # Split name and units
            if(grepl("\\(.*\\)", category)) {
              main_name <- gsub("\\s*\\(.*\\)\\s*$", "", category)
              unit <- gsub(".*\\((.*)\\).*", "(\\1)", category)
              
              text(col_center, y_pos_top - header_height * 0.35,
                   main_name, font = 2, cex = 1.1, col = "#1a1a1a")
              text(col_center, y_pos_top - header_height * 0.65,
                   unit, font = 3, cex = 0.9, col = "#4b5563")
            } else {
              text(col_center, y_pos_top - header_height/2,
                   category, font = 2, cex = 1.1, col = "#1a1a1a")
            }
          }
        }
        
        # Draw cell values
        for (i in 1:n_rows) {
          for (j in 1:n_cols) {
            col_center <- col_positions[j] + col_widths[j]/2
            row_center <- y_pos_top - header_height - ((i - 0.5) * row_height)
            
            cell_text <- as.character(combined_data[i,j])
            if(is.na(combined_data[i,j]) || cell_text == "NA" || cell_text == "NaN") {
              cell_text <- "—"
            }
            
            # Different styling for different columns
            if(j == 1) {
              # Trial column - smaller font
              text(col_center, row_center, cell_text,
                   cex = 1.3, col = "#374151")
            } else if(j == 2) {
              # Type column - styled badge
              bg_col <- ifelse(cell_text == "Raw", "#dbeafe", "#fef3c7")
              text_col <- ifelse(cell_text == "Raw", "#1e40af", "#92400e")
              
              rect(col_positions[j] + 0.01, row_center - row_height * 0.25,
                   col_positions[j] + col_widths[j] - 0.01, row_center + row_height * 0.25,
                   col = bg_col, border = NA)
              text(col_center, row_center, cell_text,
                   cex = 1.1, font = 2, col = text_col)
            } else {
              # Data columns
              text(col_center, row_center, cell_text,
                   cex = 1.5, font = 1, col = "#111827")
            }
          }
        }
      } else {
        par(mar=c(0,0,3,0), xpd=TRUE, family="sans")
        plot.new()
        text(0.5, 0.5, "No data available for table", cex = 1.5, col = "#6b7280")
      }
    }
    
    # Render the visualization
    output$combinedVisualization <- renderPlot({
      create_visualization()
    })
    
    # Download handler
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # Get the actual pitcher name for filename
        player_name <- unique(athlete_trials()$pitcher_name)[1]
        paste0("VALD_Assessment_", gsub(" ", "_", player_name), "_",
               format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        if (length(input$radar_metrics) < 3 || length(input$radar_metrics) > 6) {
          return()
        }
        
        png(file, width = 1800, height = 2400, res = 150)
        create_visualization()
        dev.off()
      }
    )
  })
}

# ---------------------------------------------------------------------------------
# STANDALONE APP RUNNER
# ---------------------------------------------------------------------------------
# This section runs the module as a standalone app for testing

# Define UI for standalone app
ui <- fluidPage(
  combinedPPPRadarsUI("ppp_module")
)

# Define server for standalone app
server <- function(input, output, session) {
  combinedPPPRadarsServer("ppp_module")
}

# Run the application
shinyApp(ui = ui, server = server)

