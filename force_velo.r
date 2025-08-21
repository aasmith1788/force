library(shiny)
library(DBI)
library(RPostgres)
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
  "concentric_duration_ms", "trial_label", "test_date"
))

### Database configuration (same as forceapp.r) ###
DB_CONFIG <- list(
  host = "pitch-modeling.cvoesgo8szjk.us-east-2.rds.amazonaws.com",
  port = 5432,
  dbname = "postgres",
  user = "premier3",
  password = "premier3"
)

### Read and Clean Data from PostgreSQL ###
read_data <- function() {
  conn <- dbConnect(RPostgres::Postgres(),
                    host = DB_CONFIG$host,
                    port = DB_CONFIG$port,
                    dbname = DB_CONFIG$dbname,
                    user = DB_CONFIG$user,
                    password = DB_CONFIG$password)
  on.exit(dbDisconnect(conn), add = TRUE)

  df <- dbGetQuery(conn, "
    SELECT
      trialid,
      athleteid,
      pitcher_name AS athlete_name,
      recordedutc,
      max_velo,
      concentric_peak_force AS concentric_peak_force_n,
      concentric_peak_velocity AS concentric_peak_velocity_m_s,
      concentric_duration AS concentric_duration_ms
    FROM vald_assessments
    WHERE concentric_peak_force IS NOT NULL
      AND concentric_peak_velocity IS NOT NULL
      AND concentric_duration IS NOT NULL
    ORDER BY recordedutc
  ")

  df <- df %>%
    mutate(
      recordedutc = as.POSIXct(recordedutc),
      test_date = as.Date(recordedutc),
      max_fb_velo = ifelse(is.na(max_velo), "Not Recorded", as.character(as.integer(max_velo))),
      velo_group = case_when(
        is.na(max_velo) ~ "Not Recorded",
        max_velo < 80 ~ "<80 mph",
        max_velo < 85 ~ "80–84 mph",
        max_velo < 90 ~ "85–89 mph",
        TRUE ~ "90+ mph"
      )
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
        uiOutput(ns("trial_selector")),
        checkboxGroupInput(ns("velos"), "Chart options",
          choices = c("<80 mph", "80–84 mph", "85–89 mph", "90+ mph", "Not Recorded"),
          selected = c("<80 mph", "80–84 mph", "85–89 mph", "Not Recorded"))
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

    athlete_trials <- reactive({
      req(df(), input$athlete)
      df() %>%
        filter(athlete_name == input$athlete) %>%
        arrange(recordedutc) %>%
        group_by(test_date) %>%
        mutate(
          trial_num = row_number(),
          trial_label = paste0("Trial ", trial_num, " (", format(test_date, "%Y-%m-%d"), ")")
        ) %>%
        ungroup()
    })

    output$trial_selector <- renderUI({
      req(athlete_trials())
      trials <- athlete_trials()
      checkboxGroupInput(session$ns("trials"), "Select Trials",
                         choices = setNames(trials$trialid, trials$trial_label),
                         selected = trials$trialid[1])
    })

    # First Plot: Peak Velocity vs Force
    output$forceVelocityPlot <- renderPlotly({
      req(df())
      data <- df()
      trials <- athlete_trials()

      x_med <- median(data$concentric_peak_force_n, na.rm = TRUE)
      y_med <- median(data$concentric_peak_velocity_m_s, na.rm = TRUE)
      x_min <- min(data$concentric_peak_force_n, na.rm = TRUE)
      x_max <- max(data$concentric_peak_force_n, na.rm = TRUE)
      y_min <- min(data$concentric_peak_velocity_m_s, na.rm = TRUE)
      y_max <- max(data$concentric_peak_velocity_m_s, na.rm = TRUE)

      filtered <- data %>%
        left_join(trials %>% select(trialid, trial_label), by = "trialid") %>%
        filter(velo_group %in% input$velos | trialid %in% input$trials) %>%
        mutate(
          is_highlighted = ifelse(trialid %in% input$trials, "Highlighted", velo_group),
          tooltip_text = ifelse(is_highlighted == "Highlighted",
                                paste0("Date: ", test_date, " | ", trial_label),
                                "")
        )

      selected_highlight <- filtered %>% filter(is_highlighted == "Highlighted")

      subtitle_text <- if (nrow(selected_highlight) > 0) {
        paste0(
          "Max FB Velo: ", selected_highlight$max_fb_velo[1], "\n",
          "Concentric Peak Force: ", selected_highlight$concentric_peak_force_n[1], "\n",
          "Concentric Peak Velocity: ", selected_highlight$concentric_peak_velocity_m_s[1]
        )
      } else { NULL }

      p <- ggplot(filtered, aes(x = concentric_peak_force_n, y = concentric_peak_velocity_m_s, text = tooltip_text)) +
        geom_point(aes(color = is_highlighted, size = is_highlighted), alpha = 0.8) +
        geom_vline(xintercept = x_med, color = "black", linewidth = 1) +
        geom_hline(yintercept = y_med, color = "black", linewidth = 1) +
        scale_color_manual(values = c(
          "<80 mph" = "green", "80–84 mph" = "yellow", "85–89 mph" = "orange",
          "90+ mph" = "red", "Not Recorded" = "gray", "Highlighted" = "purple"
        )) +
        scale_size_manual(values = c(
          "<80 mph" = 2, "80–84 mph" = 2, "85–89 mph" = 2,
          "90+ mph" = 2, "Not Recorded" = 2, "Highlighted" = 4
        ), guide = "none") +
        labs(
          title = paste("Concentric Peak Velocity vs Concentric Peak Force--", input$athlete),
          subtitle = subtitle_text,
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
      trials <- athlete_trials()

      x_med <- median(data$concentric_duration_ms, na.rm = TRUE)
      y_med <- median(data$concentric_peak_force_n, na.rm = TRUE)
      x_min <- min(data$concentric_duration_ms, na.rm = TRUE)
      x_max <- max(data$concentric_duration_ms, na.rm = TRUE)
      y_min <- min(data$concentric_peak_force_n, na.rm = TRUE)
      y_max <- max(data$concentric_peak_force_n, na.rm = TRUE)

      filtered <- data %>%
        left_join(trials %>% select(trialid, trial_label), by = "trialid") %>%
        filter(velo_group %in% input$velos | trialid %in% input$trials) %>%
        mutate(
          is_highlighted = ifelse(trialid %in% input$trials, "Highlighted", velo_group),
          tooltip_text = ifelse(is_highlighted == "Highlighted",
                                paste0("Date: ", test_date, " | ", trial_label),
                                "")
        )

      selected_highlight <- filtered %>% filter(is_highlighted == "Highlighted")

      subtitle_text <- if (nrow(selected_highlight) > 0) {
        paste0(
          "Max FB Velo: ", selected_highlight$max_fb_velo[1], "\n",
          "Concentric Peak Force: ", selected_highlight$concentric_peak_force_n[1], "\n",
          "Concentric Duration: ", selected_highlight$concentric_duration_ms[1]
        )
      } else { NULL }

      p2 <- ggplot(filtered, aes(x = concentric_duration_ms, y = concentric_peak_force_n, text = tooltip_text)) +
        geom_point(aes(color = is_highlighted, size = is_highlighted), alpha = 0.8) +
        geom_vline(xintercept = x_med, color = "black", linewidth = 1) +
        geom_hline(yintercept = y_med, color = "black", linewidth = 1) +
        scale_color_manual(values = c(
          "<80 mph" = "green", "80–84 mph" = "yellow", "85–89 mph" = "orange",
          "90+ mph" = "red", "Not Recorded" = "gray", "Highlighted" = "purple"
        )) +
        scale_size_manual(values = c(
          "<80 mph" = 2, "80–84 mph" = 2, "85–89 mph" = 2,
          "90+ mph" = 2, "Not Recorded" = 2, "Highlighted" = 4
        ), guide = "none") +
        labs(
          title = paste("Concentric Peak Force vs Concentric Duration--", input$athlete),
          subtitle = subtitle_text,
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
