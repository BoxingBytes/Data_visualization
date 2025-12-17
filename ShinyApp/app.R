library(shiny)
library(dplyr)
library(ggplot2)
library(arrow)
library(lubridate)
library(scales)
library(DT)

# -------------------------------
# Load ONLY pre-aggregated data (NO full validations dataset)
# -------------------------------
daily_network <- read_parquet("data/pre_aggregated/daily_network.parquet")
station_daily <- read_parquet("data/pre_aggregated/station_daily.parquet")
station_metadata <- read_parquet("data/pre_aggregated/station_metadata.parquet")
weekly_network <- read_parquet("data/pre_aggregated/weekly_network.parquet")
mode_totals <- read_parquet("data/pre_aggregated/mode_totals.parquet")
norm_by_weekday <- read_parquet("data/pre_aggregated/norm_by_weekday.parquet")

# Ensure date columns are Date type
daily_network$JOUR <- as.Date(daily_network$JOUR)
station_daily$JOUR <- as.Date(station_daily$JOUR)
station_metadata$min_date <- as.Date(station_metadata$min_date)
station_metadata$max_date <- as.Date(station_metadata$max_date)
weekly_network$week <- as.Date(weekly_network$week)

# Compute norm_data from daily_network
covid_start <- as.Date("2020-03-01")
covid_end <- as.Date("2022-06-30")

norm_data <- daily_network %>%
  filter(
    is_holiday == "Normal Day",
    !(JOUR >= covid_start & JOUR <= covid_end)
  )

# UI
ui <- fluidPage(
  titlePanel("Île-de-France Rail Network Ridership Dashboard (2018-2025)"),
  tabsetPanel(
    tabPanel("Compare with Norm",
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Select Period to Compare Against the Norm"),
                        p("The 'Norm' is defined as average ridership during typical weeks, excluding holidays and COVID period (Mar 2020 - Jun 2022)."),
                        dateRangeInput("norm_dates", "Select comparison dates:",
                                       start = max(daily_network$JOUR) - 30,
                                       end = max(daily_network$JOUR),
                                       min = min(daily_network$JOUR),
                                       max = max(daily_network$JOUR))
                      )
               )
             ),
             fluidRow(
               column(12,
                      h3("Comparison with Norm - Key Statistics"),
                      DTOutput("norm_comparison_table")
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("norm_weekday_plot", height = "400px")
               ),
               column(6,
                      plotOutput("selected_weekday_plot", height = "400px")
               )
             ),
             fluidRow(
               column(12,
                      h3("Deviation from Norm by Weekday"),
                      plotOutput("deviation_plot", height = "400px")
               )
             )
    ),
    
    tabPanel("Period Comparison",
             fluidRow(
               column(6,
                      wellPanel(
                        h4("Reference Period"),
                        dateRangeInput("ref_dates", "Select dates:",
                                       start = min(daily_network$JOUR),
                                       end = min(daily_network$JOUR) + 30,
                                       min = min(daily_network$JOUR),
                                       max = max(daily_network$JOUR))
                      )
               ),
               column(6,
                      wellPanel(
                        h4("Comparison Period"),
                        dateRangeInput("comp_dates", "Select dates:",
                                       start = max(daily_network$JOUR) - 30,
                                       end = max(daily_network$JOUR),
                                       min = min(daily_network$JOUR),
                                       max = max(daily_network$JOUR))
                      )
               )
             ),
             fluidRow(
               column(12,
                      h3("Key Statistics Comparison"),
                      DTOutput("comparison_table")
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("ref_weekday_plot", height = "400px")
               ),
               column(6,
                      plotOutput("comp_weekday_plot", height = "400px")
               )
             ),
             fluidRow(
               column(12,
                      h3("Daily Trends Comparison"),
                      plotOutput("daily_comparison_plot", height = "400px")
               )
             )
    ),
    
    tabPanel("Station Explorer",
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Select a Station"),
                        selectInput("station_select", "Station:",
                                    choices = NULL),
                        hr(),
                        h4("Station Information"),
                        verbatimTextOutput("station_info")
                      )
               ),
               column(8,
                      h4("Station Ridership Trends (Plots)")
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("station_trend_plot", height = "400px")
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("station_weekday_plot", height = "350px")
               ),
               column(6,
                      plotOutput("station_category_plot", height = "350px")
               )
             )
    ),
    
    tabPanel("Overall Trends",
             fluidRow(
               column(12,
                      h3("Weekly Ridership Evolution"),
                      plotOutput("weekly_trend_plot", height = "500px")
               )
             ),
             fluidRow(
               column(6,
                      h3("Top 20 Busiest Stations"),
                      plotOutput("top_stations_plot", height = "500px")
               ),
               column(6,
                      h3("Ridership by Transport Mode"),
                      plotOutput("mode_plot", height = "500px")
               )
             )
    ),
    tabPanel("Advanced Forecast Model",
             sidebarLayout(
               sidebarPanel(
                 h4("Model Controls"),
                 sliderInput("future_days", "Days to Forecast:", 30, 365, 180),
                 sliderInput("growth_modifier", "Annual Trend Adjustment (%):", -10, 10, 0),
                 hr(),
                 h4("Scenarios:"),
                 checkboxInput("apply_remote_friday", "Simulate 20% Friday Drop", FALSE),
                 checkboxInput("apply_event", "Major Event Scenario", FALSE),
                 conditionalPanel(
                   condition = "input.apply_event == true",
                   sliderInput("event_boost", "Event Ridership Boost (%):", 
                               min = 0, max = 100, value = 30, step = 5),
                   dateRangeInput("event_dates", "Event Period:", 
                                  start = "2024-07-14", 
                                  end   = as.Date("2024-07-14") + 15)
                 ),
                 helpText("This simulates different scenarios of ridership variance."),
                 hr(),
                 helpText("Model: Linear Regression trained on post-covid data.  ")
               ),
               mainPanel(
                 plotOutput("adv_forecast_plot", height = "500px"),
                 wellPanel(
                   h4("Forecast Insights"),
                   tableOutput("forecast_metrics")
                 )
               )
             )
    
    
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update station choices (by name)
  station_choices <- setNames(
    station_metadata$nom_lda,
    station_metadata$nom_lda
  )
  updateSelectInput(session, "station_select", choices = station_choices)
  
  # Reactive data for norm comparison
  norm_comp_data <- reactive({
    daily_network %>%
      filter(JOUR >= input$norm_dates[1] & JOUR <= input$norm_dates[2]) %>%
      select(JOUR, day_of_week, daily_total = NB_VALD)
  })
  
  # Norm comparison table
  output$norm_comparison_table <- renderDT({
    selected <- norm_comp_data() %>%
      group_by(day_of_week) %>%
      summarise(avg_val = mean(daily_total, na.rm=TRUE), .groups="drop")
    
    comparison <- norm_by_weekday %>%
      left_join(selected, by="day_of_week") %>%
      mutate(
        deviation_pct = round((avg_val - norm_avg)/norm_avg*100,1),
        deviation_abs = round(avg_val - norm_avg,0)
      ) %>%
      select(
        Weekday = day_of_week,
        `Norm Average` = norm_avg,
        `Selected Period` = avg_val,
        `Deviation (%)` = deviation_pct,
        `Deviation (abs)` = deviation_abs
      )
    
    datatable(comparison, options=list(dom='t', ordering=FALSE), rownames=FALSE) %>%
      formatCurrency(c("Norm Average","Selected Period","Deviation (abs)"), currency="", digits=0) %>%
      formatStyle('Deviation (%)', backgroundColor = styleInterval(c(-10,10), c('#ffcccc','white','#ccffcc')))
  })
  
  # Norm plots
  output$norm_weekday_plot <- renderPlot({
    ggplot(norm_by_weekday, aes(x=day_of_week, y=norm_avg)) +
      geom_col(fill="darkblue", alpha=0.7) +
      geom_errorbar(aes(ymin=norm_avg-norm_sd, ymax=norm_avg+norm_sd), width=0.2, color="red") +
      scale_y_continuous(labels=comma) +
      labs(title="THE NORM - Average Ridership by Weekday",
           subtitle="Error bars show ± 1 SD",
           x="Day", y="Average Validations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45,hjust=1))
  })
  
  output$selected_weekday_plot <- renderPlot({
    selected <- norm_comp_data() %>%
      group_by(day_of_week) %>%
      summarise(avg_val = mean(daily_total, na.rm=TRUE), .groups="drop")
    
    ggplot(selected, aes(x=day_of_week, y=avg_val)) +
      geom_col(fill="darkorange", alpha=0.7) +
      scale_y_continuous(labels=comma) +
      labs(title="Selected Period - Average by Weekday",
           x="Day", y="Average Validations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45,hjust=1))
  })
  
  output$deviation_plot <- renderPlot({
    selected <- norm_comp_data() %>%
      group_by(day_of_week) %>%
      summarise(avg_val = mean(daily_total, na.rm=TRUE), .groups="drop")
    
    norm_by_weekday %>%
      left_join(selected, by="day_of_week") %>%
      mutate(deviation_pct = (avg_val-norm_avg)/norm_avg*100) %>%
      ggplot(aes(x=day_of_week, y=deviation_pct, fill=deviation_pct>0)) +
      geom_col(show.legend=FALSE) +
      geom_hline(yintercept=0, linetype="dashed") +
      scale_fill_manual(values=c("red","green")) +
      labs(title="Deviation from Norm (%)", x="Day", y="Deviation (%)") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # Period comparison reactives
  ref_data <- reactive({
    daily_network %>%
      filter(JOUR >= input$ref_dates[1] & JOUR <= input$ref_dates[2]) %>%
      select(JOUR, day_of_week, total = NB_VALD)
  })
  
  comp_data <- reactive({
    daily_network %>%
      filter(JOUR >= input$comp_dates[1] & JOUR <= input$comp_dates[2]) %>%
      select(JOUR, day_of_week, total = NB_VALD)
  })
  
  # Comparison table
  output$comparison_table <- renderDT({
    ref <- ref_data() %>%
      summarise(Total_Validations=sum(total, na.rm=TRUE), Avg_Daily=mean(total, na.rm=TRUE),
                Active_Stations=nrow(station_metadata)) %>%
      mutate(Period="Reference")
    
    comp <- comp_data() %>%
      summarise(Total_Validations=sum(total, na.rm=TRUE), Avg_Daily=mean(total, na.rm=TRUE),
                Active_Stations=nrow(station_metadata)) %>%
      mutate(Period="Comparison")
    
    diff <- data.frame(
      Period="Difference (%)",
      Total_Validations=round((comp$Total_Validations-ref$Total_Validations)/ref$Total_Validations*100,2),
      Avg_Daily=round((comp$Avg_Daily-ref$Avg_Daily)/ref$Avg_Daily*100,2),
      Active_Stations=comp$Active_Stations-ref$Active_Stations
    )
    
    bind_rows(ref, comp, diff) %>%
      datatable(options=list(dom='t', ordering=FALSE), rownames=FALSE) %>%
      formatCurrency(c("Total_Validations","Avg_Daily"), currency="", digits=0)
  })
  
  # Weekday plots for reference and comparison
  output$ref_weekday_plot <- renderPlot({
    ref_data() %>%
      group_by(day_of_week) %>%
      summarise(avg_val=mean(total, na.rm=TRUE), .groups="drop") %>%
      ggplot(aes(x=day_of_week, y=avg_val, fill=day_of_week)) +
      geom_col(show.legend=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Reference Period - Avg by Weekday", x="Day", y="Avg Validations") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$comp_weekday_plot <- renderPlot({
    comp_data() %>%
      group_by(day_of_week) %>%
      summarise(avg_val=mean(total, na.rm=TRUE), .groups="drop") %>%
      ggplot(aes(x=day_of_week, y=avg_val, fill=day_of_week)) +
      geom_col(show.legend=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Comparison Period - Avg by Weekday", x="Day", y="Avg Validations") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$daily_comparison_plot <- renderPlot({
    bind_rows(ref_data() %>% mutate(Period="Reference"),
              comp_data() %>% mutate(Period="Comparison")) %>%
      ggplot(aes(x=JOUR, y=total, color=Period)) +
      geom_line(linewidth=1) +
      scale_y_continuous(labels=comma) +
      labs(title="Daily Ridership Comparison", x="Date", y="Total Validations") +
      theme_minimal()
  })
  
  # --- Station Explorer ---
  selected_station_data <- reactive({
    req(input$station_select)
    station_daily %>% filter(nom_lda == input$station_select)
  })
  
  selected_station_meta <- reactive({
    req(input$station_select)
    station_metadata %>% filter(nom_lda == input$station_select)
  })
  
  output$station_info <- renderText({
    meta <- selected_station_meta()
    paste0(
      "Station: ", meta$nom_lda, "\n",
      "Type: ", meta$type_arret, "\n",
      "Total Validations: ", format(meta$total_validations, big.mark=","), "\n",
      "Avg Daily: ", format(round(meta$avg_daily,0), big.mark=","), "\n",
      "Date Range: ", meta$min_date, " to ", meta$max_date
    )
  })
  
  output$station_trend_plot <- renderPlot({
    st <- selected_station_data()
    st %>%
      group_by(JOUR) %>%
      summarise(daily_total = sum(total_validations, na.rm=TRUE), .groups="drop") %>%
      mutate(week = floor_date(JOUR, "week")) %>%
      group_by(week) %>%
      summarise(total = sum(daily_total, na.rm=TRUE), .groups="drop") %>%
      ggplot(aes(x=week, y=total)) +
      geom_line(color="darkblue", linewidth=1) +
      geom_smooth(method="loess", color="red", se=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Weekly Ridership Trend", x="Week", y="Total Validations") +
      theme_minimal()
  })
  
  output$station_weekday_plot <- renderPlot({
    st <- selected_station_data()
    st %>%
      group_by(JOUR, day_of_week) %>%
      summarise(daily_total = sum(total_validations, na.rm=TRUE), .groups="drop") %>%
      group_by(day_of_week) %>%
      summarise(avg_val = mean(daily_total, na.rm=TRUE), .groups="drop") %>%
      ggplot(aes(x=day_of_week, y=avg_val, fill=day_of_week)) +
      geom_col(show.legend=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Average by Weekday", x="Day", y="Avg Validations") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$station_category_plot <- renderPlot({
    st <- selected_station_data()
    st %>%
      group_by(CATEGORIE_TITRE) %>%
      summarise(total=sum(total_validations, na.rm=TRUE), .groups="drop") %>%
      filter(!is.na(CATEGORIE_TITRE)) %>%
      ggplot(aes(x=reorder(CATEGORIE_TITRE,-total), y=total, fill=CATEGORIE_TITRE)) +
      geom_col(show.legend=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Ridership by Pass Category", x="Category", y="Total Validations") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # --- Overall trends ---
  output$weekly_trend_plot <- renderPlot({
    weekly_network %>%
      filter(week!=min(week) & week!=max(week)) %>%
      ggplot(aes(x=week,y=total)) +
      geom_line(color="#2C3E50", linewidth=1) +
      geom_smooth(method="loess", color="red", se=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Weekly Total Validations Over Time", x="Week", y="Total Validations") +
      theme_minimal()
  })
  
  output$top_stations_plot <- renderPlot({
    station_metadata %>%
      slice_max(avg_daily, n=20) %>%
      ggplot(aes(x=reorder(nom_lda, avg_daily), y=avg_daily)) +
      geom_col(fill="#3498DB") +
      coord_flip() +
      scale_y_continuous(labels=comma) +
      labs(title="Top 20 Stations by Avg Daily Ridership", x=NULL, y="Avg Daily Validations") +
      theme_minimal()
  })
  
  output$mode_plot <- renderPlot({
    mode_totals %>%
      ggplot(aes(x=reorder(type_arret,-total), y=total, fill=type_arret)) +
      geom_col(show.legend=FALSE) +
      scale_y_continuous(labels=comma) +
      labs(title="Total Ridership by Transport Mode", x="Mode", y="Total Validations") +
      theme_minimal()
  })
  
  ####- Prediction
  # Train the model 
  trained_model <- reactive({
    train_df <- daily_network %>%
      filter(JOUR > as.Date("2022-07-01")) %>%
      mutate(
        time_index = as.numeric(JOUR - min(JOUR)),
        day_num = as.factor(lubridate::wday(JOUR, week_start = 1)),
        month_num = as.factor(lubridate::month(JOUR)),
        is_holiday = as.factor(as.character(is_holiday))
      )
    lm(NB_VALD ~ time_index + day_num + month_num + is_holiday, data = train_df)
  })
  
  #  Forecast Engine
  forecast_engine <- reactive({
    model <- trained_model()
    req(model)
    
    last_date <- max(daily_network$JOUR)
    
    # 1. Create future dates with month_num
    future_dates <- data.frame(
      JOUR = seq(last_date + 1, by = "day", length.out = input$future_days)
    ) %>%
      mutate(
        time_index = as.numeric(JOUR - min(daily_network$JOUR)),
        day_num = factor(lubridate::wday(JOUR, week_start = 1), 
                         levels = levels(model$model$day_num)),
        month_num = factor(lubridate::month(JOUR), 
                           levels = levels(model$model$month_num)),
        is_holiday = factor("Normal Day", levels = levels(model$model$is_holiday))
      )
    
    preds <- predict(model, newdata = future_dates, interval = "prediction")
    growth_factor <- 1 + (input$growth_modifier / 100 * (future_dates$time_index / 365))
    
    last_real_val <- tail(daily_network$NB_VALD, 1)
    
    last_date_df <- data.frame(
      time_index = as.numeric(max(daily_network$JOUR) - min(daily_network$JOUR)),
      day_num = factor(lubridate::wday(max(daily_network$JOUR), week_start = 1), 
                       levels = levels(model$model$day_num)),
      month_num = factor(lubridate::month(max(daily_network$JOUR)), 
                         levels = levels(model$model$month_num)),
      is_holiday = factor(tail(daily_network$is_holiday, 1), 
                          levels = levels(model$model$is_holiday))
    )
    
    last_model_pred <- predict(model, newdata = last_date_df)
    offset <- last_real_val - last_model_pred
    
    res <- future_dates %>%
      mutate(
        predicted_total = (preds[,1] + offset) * growth_factor,
        type = "Forecast"
      )
    
    if(input$apply_remote_friday) {
      res <- res %>%
        mutate(predicted_total = ifelse(lubridate::wday(JOUR, week_start = 1) == 5, 
                                        predicted_total * 0.8, predicted_total))
    }
    
    if(input$apply_event) {
      boost_multiplier <- 1 + (input$event_boost / 100)
      ev_start <- as.Date(input$event_dates[1])
      ev_end <- as.Date(input$event_dates[2])
      
      res <- res %>%
        mutate(predicted_total = ifelse(JOUR >= ev_start & JOUR <= ev_end, 
                                        predicted_total * boost_multiplier, predicted_total))
    }
    
    res
  })
  # Forecast Plot Output
  output$adv_forecast_plot <- renderPlot({
    fore_df <- forecast_engine()
    req(fore_df)
    
    hist_df <- daily_network %>% 
      filter(JOUR > max(JOUR) - 60) %>%
      mutate(type = "History", predicted_total = NB_VALD) %>%
      select(JOUR, predicted_total, type)
    
    plot_data <- bind_rows(hist_df, fore_df %>% select(JOUR, predicted_total, type))
    
    p <- ggplot(plot_data, aes(x = JOUR, y = predicted_total, color = type)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = c("History" = "#2C3E50", "Forecast" = "#3498DB")) +
      theme_minimal() +
      labs(title = "Network Ridership Forecast", x = "Date", y = "Validations")
    
    if(input$apply_event) {
      p <- p + annotate("rect", xmin = as.Date(input$event_dates[1]), xmax = as.Date(input$event_dates[2]), 
                        ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "green") +
        annotate("text", x = as.Date(input$event_dates[1]), y = max(plot_data$predicted_total, na.rm=T), 
                 label = paste0("EVENT BOOST: +", input$event_boost, "%"), 
                 vjust = 2, hjust = 0, color = "darkgreen", fontface = "bold")
    }
    p
  })

  #  Metrics Table
  output$forecast_metrics <- renderTable({
    df <- forecast_engine()
    req(df)
    
    recent_avg <- mean(daily_network$NB_VALD[daily_network$JOUR > max(daily_network$JOUR) - 60], na.rm = TRUE)
    forecast_avg <- mean(df$predicted_total)
    growth_pct <- ((forecast_avg - recent_avg) / recent_avg) * 100
    
    total_vol <- sum(df$predicted_total)
    peak_val  <- max(df$predicted_total)
    peak_date <- df$JOUR[which.max(df$predicted_total)]
    
    data.frame(
      `Avg Daily` = format(round(forecast_avg, 0), big.mark=","),
      `Peak Volume` = format(round(peak_val, 0), big.mark=","),
      `Peak Date` = as.character(peak_date),
      `Total Volume` = format(round(total_vol, 0), big.mark=","),
      `Vs. History` = paste0(round(growth_pct, 1), "%"),
      `Scenarios` = ifelse(input$apply_remote_friday || input$apply_event, "Active", "None"),
      check.names = FALSE 
    )
  }, 
  striped = FALSE, 
  hover = TRUE, 
  width = "100%", 
  align = 'c'      
  )

  
  
}

# Run the app
shinyApp(ui, server)