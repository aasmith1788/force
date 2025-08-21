library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(rlang)
library(tidyr)
library(plotly)

### Silence R CMD check warnings about NSE variables ###
globalVariables(c(
  "athlete_name", "velo_group", "concentric_peak_force_n",
  "concentric_peak_velocity_m_s", "max_fb_velo", "is_highlighted",
  "concentric_duration_ms" # add your duration column here if needed
))

### Load the dataset from Google Sheets ###
gs4_deauth()  ### Access public Google Sheet without authentication ###
sheet_url <- "https://docs.google.com/spreadsheets/d/1C4llBzMOXvOg_DQYraNQ3wxcqDZNMTyM2fkA0X2DCi4"

### Read and Clean Data ###
read_data <- function() {
  df <- read_sheet(sheet_url, sheet = "R_Data_Setup")
  df <- df %>%
    mutate(
      velo_group = case_when(
        .data$velo_group == 79 ~ "<80 mph",
        .data$velo_group == 83 ~ "80–84 mph",
        .data$velo_group == 87 ~ "85–89 mph",
        .data$velo_group == 91 ~ "90+ mph",
        TRUE ~ "Not Recorded"
      ),
      max_fb_velo = ifelse(is.na(.data$max_fb_velo), "Not Recorded", as.character(as.integer(.data$max_fb_velo))),
      concentric_peak_force_n = as.numeric(.data$concentric_peak_force_n),
      concentric_peak_velocity_m_s = as.numeric(.data$concentric_peak_velocity_m_s),
      concentric_duration_ms = as.numeric(.data$concentric_duration_ms)  # Add this if not already numeric
    )
  return(df)
}

### Set up User interface within the module ###
forceVelocityUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("athlete"), "Choose Athlete", choices = NULL),
        checkboxGroupInput(ns("velos"), "Chart options",
          choices = c("<80 mph", "80–84 mph", "85–89 mph", "90+ mph", "Not Recorded", "Highlighted"),
          selected = c("<80 mph", "80–84 mph", "85–89 mph", "Not Recorded", "Highlighted"))
      ),
      mainPanel(
        plotlyOutput(ns("forceVelocityPlot"), height = "600px"),
        plotlyOutput(ns("forceDurationPlot"), height = "600px")
      )
    )
  )
}

### Set up server for module ###
forceVelocityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    df <- reactiveVal()

    observe({
      data <- read_data()
      req(nrow(data) > 0)
      df(data)
      updateSelectInput(
        session,
        "athlete",
        choices = sort(unique(df()$athlete_name)),
        selected = unique(data$athlete_name)[1]
      )
    })

    # First Plot: Peak Velocity vs Force
    output$forceVelocityPlot <- renderPlotly({
      req(df())
      data <- df()

      x_med <- median(data$concentric_peak_force_n, na.rm = TRUE)
      y_med <- median(data$concentric_peak_velocity_m_s, na.rm = TRUE)
      x_min <- min(data$concentric_peak_force_n, na.rm = TRUE)
      x_max <- max(data$concentric_peak_force_n, na.rm = TRUE)
      y_min <- min(data$concentric_peak_velocity_m_s, na.rm = TRUE)
      y_max <- max(data$concentric_peak_velocity_m_s, na.rm = TRUE)

      filtered <- data %>%
        filter(
          .data$velo_group %in% input$velos |
            (.data$athlete_name == input$athlete & "Highlighted" %in% input$velos)
        ) %>%
        mutate(is_highlighted = ifelse(.data$athlete_name == input$athlete, "Highlighted", .data$velo_group))

      filtered <- filtered %>%
        mutate(tooltip_text = ifelse(is_highlighted == "Highlighted",
                                  paste0("Date: ", .data$test_date),
                                  ""))

      p <- ggplot(filtered, aes(x = .data$concentric_peak_force_n, y = .data$concentric_peak_velocity_m_s,
                                text = tooltip_text)) +
        geom_point(aes(color = .data$is_highlighted), size = ifelse(filtered$is_highlighted == "Highlighted", 4, 2), alpha = 0.8) +
        geom_vline(xintercept = x_med, color = "black", linewidth = 1) +
        geom_hline(yintercept = y_med, color = "black", linewidth = 1) +
        scale_color_manual(values = c(
          "<80 mph" = "green", "80–84 mph" = "yellow", "85–89 mph" = "orange",
          "90+ mph" = "red", "Not Recorded" = "gray", "Highlighted" = "purple"
        )) +
        labs(
          title = paste("Concentric Peak Velocity vs Concentric Peak Force--", input$athlete),
          subtitle = paste0(
            "Max FB Velo: ", filtered$max_fb_velo[filtered$athlete_name == input$athlete][1], "\n",
            "Concentric Peak Force: ", filtered$concentric_peak_force_n[filtered$athlete_name == input$athlete][1], "\n",
            "Concentric Peak Velocity: ", filtered$concentric_peak_velocity_m_s[filtered$athlete_name == input$athlete][1]
          ),
          x = "Concentric Peak Force (N)",
          y = "Concentric Peak Velocity (m/s)",
          color = "Velocity Group"
        ) +
        theme_minimal(base_size = 14) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))

      ggplotly(p, tooltip = "text")
    })

    # Second Plot: Peak Force vs Concentric Duration
    output$forceDurationPlot <- renderPlotly({
      req(df())
      data <- df()

      x_med <- median(data$concentric_duration_ms, na.rm = TRUE)
      y_med <- median(data$concentric_peak_force_n, na.rm = TRUE)
      x_min <- min(data$concentric_duration_ms, na.rm = TRUE)
      x_max <- max(data$concentric_duration_ms, na.rm = TRUE)
      y_min <- min(data$concentric_peak_force_n, na.rm = TRUE)
      y_max <- max(data$concentric_peak_force_n, na.rm = TRUE)

      filtered <- data %>%
        filter(
          .data$velo_group %in% input$velos |
            (.data$athlete_name == input$athlete & "Highlighted" %in% input$velos)
        ) %>%
        mutate(is_highlighted = ifelse(.data$athlete_name == input$athlete, "Highlighted", .data$velo_group))

      filtered <- filtered %>%
        mutate(tooltip_text = ifelse(is_highlighted == "Highlighted",
                                  paste0("Date: ", .data$test_date),
                                  ""))

      p2 <- ggplot(filtered, aes(x = .data$concentric_duration_ms, y = .data$concentric_peak_force_n, text = tooltip_text)) +
        geom_point(aes(color = .data$is_highlighted), size = ifelse(filtered$is_highlighted == "Highlighted", 4, 2), alpha = 0.8) +
        geom_vline(xintercept = x_med, color = "black", linewidth = 1) +
        geom_hline(yintercept = y_med, color = "black", linewidth = 1) +
        scale_color_manual(values = c(
          "<80 mph" = "green", "80–84 mph" = "yellow", "85–89 mph" = "orange",
          "90+ mph" = "red", "Not Recorded" = "gray", "Highlighted" = "purple"
        )) +
        labs(
          title = paste("Concentric Peak Force vs Concentric Duration--", input$athlete),
          subtitle = paste0(
            "Max FB Velo: ", filtered$max_fb_velo[filtered$athlete_name == input$athlete][1], "\n",
            "Concentric Peak Force: ", filtered$concentric_peak_force_n[filtered$athlete_name == input$athlete][1], "\n",
            "Concentric Duration: ", filtered$concentric_duration_ms[filtered$athlete_name == input$athlete][1]
          ),
          x = "Concentric Duration (ms)",
          y = "Concentric Peak Force (N)",
          color = "Velocity Group"
        ) +
        theme_minimal(base_size = 14) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))

      ggplotly(p2, tooltip = "text")
    })
  })
}
