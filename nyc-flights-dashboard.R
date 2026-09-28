# ============================================================
# NYC Flights Interactive Visualization Dashboard (R Shiny)
# Data: nycflights13 (all flights departing NYC in 2013)
# Tables used: flights, airlines, airports, weather
# ============================================================

required_pkgs <- c(
  "shiny", "tidyverse", "lubridate", "scales", "viridis",
  "ggdist", "ggrepel", "DT", "nycflights13"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them with: install.packages(c(",
    paste0("'", missing_pkgs, "'", collapse = ", "), "))"
  )
}

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(ggdist)
library(ggrepel)
library(DT)
library(nycflights13)

# The full flights table has ~337k rows. Lower this number if the app feels slow.
max_rows <- 400000

# ------------------------------------------------------------
# 1. Data preparation
# ------------------------------------------------------------

airline_names <- airlines %>%
  mutate(carrier_name = str_remove_all(name, " Inc\\.| Co\\.| Corporation| Airlines| Air Lines")) %>%
  select(carrier, carrier_name)

# weather has one row per airport-hour (a few duplicates exist, so keep one)
weather_clean <- weather %>%
  distinct(origin, time_hour, .keep_all = TRUE) %>%
  select(origin, time_hour, temp, dewp, humid, wind_speed, wind_gust, pressure, visib)

airport_info <- airports %>%
  select(dest = faa, dest_name = name, dest_lat = lat, dest_lon = lon)

df <- flights %>%
  left_join(airline_names, by = "carrier") %>%
  left_join(weather_clean, by = c("origin", "time_hour")) %>%
  left_join(airport_info, by = "dest") %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    month_lbl = month(date, label = TRUE, abbr = TRUE),
    cancelled = is.na(dep_time),
    route = paste0(origin, "-", dest),
    dist_bin = cut(
      distance,
      breaks = c(0, 500, 1000, 1500, 2000, 3000, Inf),
      labels = c("<500", "500-1,000", "1,000-1,500", "1,500-2,000", "2,000-3,000", "3,000+"),
      include.lowest = TRUE
    ),
    carrier_name = coalesce(carrier_name, carrier),
    dest_name = coalesce(dest_name, dest)
  )

if (nrow(df) > max_rows) {
  set.seed(42)
  df <- df %>% slice_sample(n = max_rows)
}

# NYC reference point for the flight-path map
nyc_lon <- -73.95
nyc_lat <- 40.70

# Palette
pal_main <- "#1d4e6f"
pal_accent <- "#e09f3e"
pal_muted <- "#a8bdc9"
pal_dark <- "#0f2c3f"
pal_bad <- "#c8553d"
pal_origin <- c(EWR = pal_main, JFK = pal_accent, LGA = "#5b9279")

# Unified theme
theme_us <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15), color = pal_dark),
      plot.subtitle = element_text(color = "grey45", margin = margin(b = 10)),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      axis.title = element_text(color = "grey30"),
      axis.text = element_text(color = "grey40"),
      legend.title = element_text(color = "grey30", size = rel(0.9)),
      legend.text = element_text(color = "grey40", size = rel(0.85))
    )
}

# Filter choices
origin_choices <- c(
  "Newark (EWR)" = "EWR",
  "John F. Kennedy (JFK)" = "JFK",
  "LaGuardia (LGA)" = "LGA"
)

carrier_tbl <- df %>% distinct(carrier, carrier_name) %>% arrange(carrier_name)
carrier_choices <- setNames(carrier_tbl$carrier, paste0(carrier_tbl$carrier_name, " (", carrier_tbl$carrier, ")"))

weather_choices <- c(
  "Temperature (F)" = "temp",
  "Dew point (F)" = "dewp",
  "Humidity (%)" = "humid",
  "Wind speed (mph)" = "wind_speed",
  "Visibility (miles)" = "visib",
  "Pressure (mb)" = "pressure"
)

# Metrics used by the group comparison and worst-routes charts
metric_choices <- c(
  "Share of flights delayed" = "pct",
  "Median departure delay" = "med_dep",
  "Median arrival delay" = "med_arr"
)

metric_value <- function(dep, arr, metric, threshold) {
  switch(
    metric,
    pct = mean(dep >= threshold, na.rm = TRUE),
    med_dep = median(dep, na.rm = TRUE),
    med_arr = median(arr, na.rm = TRUE)
  )
}

# Keeps the middle 99% of a variable so a few sensor outliers do not squash the plot
inside_q <- function(x, lo = 0.005, hi = 0.995) {
  x >= quantile(x, lo, na.rm = TRUE) & x <= quantile(x, hi, na.rm = TRUE)
}

# Horizontal lollipop chart shared by the comparison and worst-routes plots.
# `data` needs: group (ordered factor), value, n
lollipop_plot <- function(data, metric, title, subtitle) {
  is_pct <- metric == "pct"
  fmt <- if (is_pct) function(x) percent(x, accuracy = 0.1) else function(x) paste0(number(x, accuracy = 0.1), " min")
  data <- data %>% mutate(lbl = paste0(fmt(value), "  (n = ", comma(n), ")"))
  
  ggplot(data, aes(value, group)) +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_segment(aes(x = 0, xend = value, yend = group), color = pal_muted, linewidth = 1) +
    geom_point(size = 3.8, color = pal_accent) +
    geom_text(aes(label = lbl, hjust = ifelse(value >= 0, -0.15, 1.15)), size = 3.4, color = "grey30") +
    scale_x_continuous(
      labels = if (is_pct) percent else label_number(),
      expand = expansion(mult = c(0.15, 0.35))
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_us()
}

# ------------------------------------------------------------
# 2. User interface
# ------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
 
      body {
        background:
          radial-gradient(circle at 10% 10%, rgba(224, 159, 62, 0.18), transparent 28%),
          radial-gradient(circle at 90% 0%, rgba(29, 78, 111, 0.22), transparent 30%),
          linear-gradient(135deg, #f6f9fb 0%, #eef4f7 45%, #f8fbfc 100%);
        font-family: 'Inter', Arial, sans-serif;
        color: #172832;
      }
 
      .container-fluid { max-width: 1500px; }
 
      .hero-card {
        background: linear-gradient(135deg, #0f2c3f 0%, #1d4e6f 58%, #316f8e 100%);
        color: white;
        border-radius: 24px;
        padding: 30px 34px;
        margin: 22px 0;
        box-shadow: 0 18px 45px rgba(15, 44, 63, 0.24);
        position: relative;
        overflow: hidden;
      }
 
      .hero-card:before {
        content: '';
        position: absolute;
        width: 360px; height: 360px;
        border-radius: 50%;
        background: rgba(224, 159, 62, 0.22);
        right: -120px; top: -160px;
      }
 
      .hero-card:after {
        content: '';
        position: absolute;
        width: 260px; height: 260px;
        border-radius: 50%;
        border: 1px solid rgba(255,255,255,0.20);
        right: 90px; bottom: -170px;
      }
 
      .hero-kicker {
        font-size: 13px; font-weight: 700;
        letter-spacing: 0.12em; text-transform: uppercase;
        color: #ffd79a; margin-bottom: 8px;
        position: relative; z-index: 1;
      }
 
      .hero-title {
        font-size: 34px; font-weight: 800; line-height: 1.15;
        margin: 0 0 10px 0; position: relative; z-index: 1;
      }
 
      .hero-subtitle {
        font-size: 16px; color: rgba(255,255,255,0.84);
        max-width: 870px; margin: 0;
        position: relative; z-index: 1;
      }
 
      .well {
        background: rgba(255,255,255,0.92);
        border-radius: 22px;
        border: 1px solid rgba(168,189,201,0.48);
        box-shadow: 0 12px 30px rgba(15,44,63,0.10);
        padding: 22px;
      }
 
      .well h3 { font-weight: 800; margin-top: 0; color: #0f2c3f; }
 
      label { color: #0f2c3f; font-weight: 700; margin-top: 10px; }
 
      .selectize-input, .form-control {
        border-radius: 12px !important;
        border: 1px solid #cad9e1 !important;
        box-shadow: none !important;
      }
 
      .selectize-input.focus {
        border-color: #1d4e6f !important;
        box-shadow: 0 0 0 3px rgba(29,78,111,0.12) !important;
      }
 
      .selectize-control.multi .selectize-input > div {
        background: #e9f2f6;
        border: 1px solid #c7dbe5;
        color: #0f2c3f;
        border-radius: 999px;
        padding: 4px 9px;
      }
 
      .irs--shiny .irs-bar, .irs--shiny .irs-single { background: #1d4e6f; border-color: #1d4e6f; }
      .irs--shiny .irs-handle { border-color: #1d4e6f; box-shadow: 0 2px 8px rgba(29,78,111,0.28); }
 
      .nav-tabs {
        border-bottom: 0; margin-bottom: 16px;
        display: flex; flex-wrap: wrap; gap: 8px;
      }
 
      .nav-tabs > li { margin-bottom: 0; }
 
      .nav-tabs > li > a {
        color: #1d4e6f; font-weight: 700;
        border: 1px solid rgba(168,189,201,0.48) !important;
        border-radius: 999px !important;
        background: rgba(255,255,255,0.78);
        padding: 10px 16px;
        transition: all 0.18s ease;
      }
 
      .nav-tabs > li > a:hover { background: #eaf4f8; color: #0f2c3f; transform: translateY(-1px); }
 
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background: linear-gradient(135deg, #1d4e6f, #0f2c3f) !important;
        color: white !important;
        border-color: transparent !important;
        box-shadow: 0 8px 18px rgba(29,78,111,0.22);
      }
 
      .tab-content {
        background: rgba(255,255,255,0.88);
        border: 1px solid rgba(168,189,201,0.45);
        border-radius: 24px;
        padding: 24px;
        box-shadow: 0 12px 30px rgba(15,44,63,0.08);
      }
 
      .tab-content h3 { font-weight: 800; color: #0f2c3f; margin-top: 4px; }
 
      .metric-card {
        background: linear-gradient(180deg, #ffffff 0%, #f7fbfd 100%);
        border-left: 6px solid #1d4e6f;
        border-radius: 18px;
        padding: 18px;
        margin-bottom: 14px;
        box-shadow: 0 10px 24px rgba(15,44,63,0.10);
      }
 
      .metric-card.alt { border-left-color: #e09f3e; }
 
      .metric-title {
        color: #61717c; font-size: 12px; font-weight: 800;
        text-transform: uppercase; letter-spacing: 0.07em;
      }
 
      .metric-value { color: #0f2c3f; font-size: 28px; font-weight: 800; margin-top: 4px; }
 
      .note { color: #61717c; font-size: 13px; }
 
      hr { border-top: 1px solid #dce8ee; }
 
      .dataTables_wrapper { font-size: 13px; }
    "))
  ),
  
  div(
    class = "hero-card",
    div(class = "hero-kicker", "R Shiny · nycflights13 visual analytics"),
    div(class = "hero-title", "NYC Flights Interactive Visualization Dashboard"),
    p(
      class = "hero-subtitle",
      "Explore delays, cancellations, carriers, routes, destinations, and weather effects for every flight leaving New York City in 2013."
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      
      checkboxGroupInput(
        inputId = "origin_filter",
        label = "Origin Airport:",
        choices = origin_choices,
        selected = origin_choices
      ),
      
      selectizeInput(
        inputId = "carrier_filter",
        label = "Airline:",
        choices = carrier_choices,
        selected = carrier_choices,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      
      sliderInput(
        inputId = "month_filter",
        label = "Month Range (2013):",
        min = 1, max = 12, value = c(1, 12), step = 1, sep = ""
      ),
      
      sliderInput(
        inputId = "delay_threshold",
        label = "Count a flight as delayed after (min):",
        min = 5, max = 60, value = 15, step = 5
      ),
      
      checkboxInput(
        inputId = "trim_extreme",
        label = "Exclude extreme delays (over 3 hours)",
        value = TRUE
      ),
      
      hr(),
      p(class = "note", "Cancelled flights are excluded from delay plots. They are counted in the Overview and shown in the Cancellations tab."),
      p(class = "note", "Delays are in minutes. Negative values mean the flight left or arrived early.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          h3("Dataset Overview"),
          fluidRow(
            column(4, div(class = "metric-card", div(class = "metric-title", "Flights After Filtering"), div(class = "metric-value", textOutput("row_count")))),
            column(4, div(class = "metric-card alt", div(class = "metric-title", "Cancellation Rate"), div(class = "metric-value", textOutput("cancel_rate")))),
            column(4, div(class = "metric-card", div(class = "metric-title", "Average Departure Delay"), div(class = "metric-value", textOutput("avg_dep_delay"))))
          ),
          fluidRow(
            column(4, div(class = "metric-card alt", div(class = "metric-title", "Share of Flights Delayed"), div(class = "metric-value", textOutput("share_delayed")))),
            column(4, div(class = "metric-card", div(class = "metric-title", "Busiest Destination"), div(class = "metric-value", textOutput("top_dest")))),
            column(4, div(class = "metric-card alt", div(class = "metric-title", "Most Delayed Airline"), div(class = "metric-value", textOutput("worst_carrier"))))
          ),
          h4("Dashboard Purpose"),
          p("This dashboard looks at when, where, and why flights leave late or do not leave at all. It covers the worst days, seasonality, time-of-day patterns, airline and route comparisons, cancellations, a destination map, weather effects, and how much time flights recover in the air.")
        ),
        
        tabPanel(
          "Worst Days",
          h3("Average Daily Departure Delay"),
          plotOutput("daily_smooth", height = 460)
        ),
        
        tabPanel(
          "Seasonality",
          h3("Graded Error Bars: How Much Do Days Vary Within a Month?"),
          plotOutput("monthly_variation", height = 460)
        ),
        
        tabPanel(
          "Time of Day",
          h3("Average Delay by Scheduled Hour"),
          plotOutput("hourly_delay", height = 440),
          hr(),
          h3("Weekday x Hour Heatmap"),
          plotOutput("wday_heatmap", height = 440)
        ),
        
        tabPanel(
          "Airlines",
          h3("Departure Delay Distribution by Airline"),
          plotOutput("half_eye", height = 620)
        ),
        
        tabPanel(
          "Compare Groups",
          h3("Compare Groups by Median Delay or Share Delayed"),
          fluidRow(
            column(4, selectInput(
              "group_var", "Group by:",
              choices = c(
                "Airline" = "carrier_name",
                "Origin airport" = "origin",
                "Month" = "month_lbl",
                "Distance band" = "dist_bin"
              ),
              selected = "carrier_name"
            )),
            column(8, radioButtons(
              "cmp_metric", "Metric:",
              choices = metric_choices, selected = "pct", inline = TRUE
            ))
          ),
          plotOutput("compare_plot", height = 520)
        ),
        
        tabPanel(
          "Cancellations",
          h3("Cancelled Flights per Day"),
          p(class = "note", "A flight counts as cancelled when it has no departure time. Spikes usually line up with major storms."),
          plotOutput("cancellations", height = 460)
        ),
        
        tabPanel(
          "Routes & Map",
          h3("Worst Routes"),
          fluidRow(
            column(6, radioButtons(
              "route_metric", "Rank routes by:",
              choices = metric_choices, selected = "pct"
            )),
            column(6, sliderInput(
              "min_route_flights", "Minimum flights per route:",
              min = 50, max = 500, value = 150, step = 50
            ))
          ),
          plotOutput("worst_routes", height = 560),
          hr(),
          h3("Where Flights Go and How Late They Arrive"),
          p(class = "note", "Continental US destinations with at least 50 flights. Alaska, Hawaii and Puerto Rico are left out to keep the map readable. Install the 'maps' package to add state outlines."),
          plotOutput("dest_map", height = 640)
        ),
        
        tabPanel(
          "Weather Space",
          h3("Weather and Delays"),
          fluidRow(
            column(4, selectInput("weather_x", "X axis:", choices = weather_choices, selected = "temp")),
            column(4, selectInput("weather_y", "Y axis:", choices = weather_choices, selected = "humid")),
            column(4, selectInput(
              "weather_fill", "Color cells by:",
              choices = c("Number of flights" = "count", "Average departure delay" = "delay"),
              selected = "delay"
            ))
          ),
          plotOutput("weather_space", height = 520)
        ),
        
        tabPanel(
          "Delay Recovery",
          h3("Departure Delay vs Arrival Delay"),
          plotOutput("delay_recovery", height = 520)
        ),
        
        tabPanel("Data Table", h3("Filtered Flight Records"), DTOutput("data_table"))
      )
    )
  )
)

# ------------------------------------------------------------
# 3. Server
# ------------------------------------------------------------

server <- function(input, output) {
  
  # All flights matching the sidebar filters (including cancelled ones)
  filtered_data <- reactive({
    req(input$origin_filter, input$carrier_filter, input$month_filter)
    
    df %>%
      filter(
        origin %in% input$origin_filter,
        carrier %in% input$carrier_filter,
        month >= input$month_filter[1],
        month <= input$month_filter[2]
      )
  })
  
  # Flights that actually departed and have a departure delay
  flown_data <- reactive({
    d <- filtered_data() %>% filter(!cancelled, !is.na(dep_delay))
    if (isTRUE(input$trim_extreme)) {
      d <- d %>% filter(dep_delay <= 180)
    }
    d
  })
  
  # ---------------- Overview ----------------
  
  output$row_count <- renderText({
    comma(nrow(filtered_data()))
  })
  
  output$cancel_rate <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("No data")
    percent(mean(d$cancelled), accuracy = 0.1)
  })
  
  output$avg_dep_delay <- renderText({
    d <- flown_data()
    if (nrow(d) == 0) return("No data")
    paste0(round(mean(d$dep_delay), 1), " min")
  })
  
  output$share_delayed <- renderText({
    d <- flown_data()
    if (nrow(d) == 0) return("No data")
    paste0(percent(mean(d$dep_delay >= input$delay_threshold), accuracy = 0.1), " (", input$delay_threshold, "+ min)")
  })
  
  output$top_dest <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("No data")
    d %>% count(dest, sort = TRUE) %>% slice(1) %>% pull(dest)
  })
  
  output$worst_carrier <- renderText({
    d <- flown_data() %>%
      group_by(carrier_name) %>%
      summarise(n = n(), avg = mean(dep_delay), .groups = "drop") %>%
      filter(n >= 100)
    if (nrow(d) == 0) return("No data")
    d %>% slice_max(avg, n = 1, with_ties = FALSE) %>% pull(carrier_name)
  })
  
  # ---------------- Worst days ----------------
  
  output$daily_smooth <- renderPlot({
    data <- flown_data() %>%
      group_by(date) %>%
      summarise(avg_delay = mean(dep_delay), .groups = "drop")
    
    validate(need(nrow(data) > 10, "Not enough data for this plot. Try selecting more months or airports."))
    
    worst_days <- data %>% slice_max(avg_delay, n = 3, with_ties = FALSE)
    
    ggplot(data, aes(date, avg_delay)) +
      geom_point(color = pal_muted, alpha = 0.45, size = 1.2) +
      geom_smooth(method = "loess", formula = y ~ x, span = 0.15, se = FALSE, color = pal_main, linewidth = 1.1) +
      geom_text_repel(
        data = worst_days,
        aes(label = format(date, "%b %d")),
        color = pal_dark, fontface = "bold", size = 3.6, seed = 42
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(
        title = "Average Departure Delay per Day",
        subtitle = "Each dot is one day; navy line is a LOESS smoother; the three worst days are labeled",
        x = NULL,
        y = "Average delay (min)"
      ) +
      theme_us()
  })
  
  # ---------------- Seasonality ----------------
  
  output$monthly_variation <- renderPlot({
    data <- flown_data() %>%
      group_by(month, date) %>%
      summarise(avg_delay = mean(dep_delay), .groups = "drop") %>%
      group_by(month) %>%
      summarise(
        med = median(avg_delay),
        q25 = quantile(avg_delay, 0.25),
        q75 = quantile(avg_delay, 0.75),
        mn = min(avg_delay),
        mx = max(avg_delay),
        .groups = "drop"
      )
    
    validate(need(nrow(data) > 0, "No monthly variation data available."))
    
    ggplot(data, aes(month, med)) +
      geom_linerange(aes(ymin = mn, ymax = mx), color = pal_main, linewidth = 0.8, alpha = 0.5) +
      geom_linerange(aes(ymin = q25, ymax = q75), color = pal_main, linewidth = 2.5) +
      geom_point(size = 2.8, color = pal_accent) +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      labs(
        title = "Day-to-Day Variation Within Each Month",
        subtitle = "Amber dot = median day · thick bar = middle 50% of days · thin bar = best to worst day",
        x = NULL,
        y = "Daily average delay (min)"
      ) +
      theme_us()
  })
  
  # ---------------- Time of day ----------------
  
  output$hourly_delay <- renderPlot({
    data <- flown_data() %>%
      filter(hour >= 5, hour <= 23) %>%
      group_by(origin, hour) %>%
      summarise(n = n(), avg_delay = mean(dep_delay), se = sd(dep_delay) / sqrt(n), .groups = "drop") %>%
      filter(n >= 20)
    
    validate(need(nrow(data) > 0, "No hourly data available."))
    
    ggplot(data, aes(hour, avg_delay, color = origin, fill = origin)) +
      geom_ribbon(aes(ymin = avg_delay - 1.96 * se, ymax = avg_delay + 1.96 * se), alpha = 0.15, color = NA) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = pal_origin, name = "Origin") +
      scale_fill_manual(values = pal_origin, name = "Origin") +
      scale_x_continuous(breaks = seq(5, 23, 2), labels = function(x) sprintf("%02d:00", x)) +
      labs(
        title = "Delays Build Up Through the Day",
        subtitle = "Average departure delay by scheduled hour; shaded band = 95% confidence interval",
        x = "Scheduled departure hour",
        y = "Average delay (min)"
      ) +
      theme_us() +
      theme(legend.position = "bottom")
  })
  
  output$wday_heatmap <- renderPlot({
    data <- flown_data() %>%
      filter(hour >= 5, hour <= 23) %>%
      group_by(wday, hour) %>%
      summarise(n = n(), avg_delay = mean(dep_delay), .groups = "drop") %>%
      filter(n >= 20) %>%
      mutate(wday = fct_rev(wday))
    
    validate(need(nrow(data) > 0, "No heatmap data available."))
    
    ggplot(data, aes(hour, wday, fill = avg_delay)) +
      geom_tile(color = "white", linewidth = 0.6) +
      scale_fill_viridis_c(option = "mako", direction = -1, name = "Avg delay\n(min)") +
      scale_x_continuous(breaks = seq(5, 23, 2), labels = function(x) sprintf("%02d:00", x), expand = c(0, 0)) +
      labs(
        title = "When Are Delays Worst?",
        subtitle = "Average departure delay by weekday and scheduled hour",
        x = "Scheduled departure hour",
        y = NULL
      ) +
      theme_us() +
      theme(panel.grid = element_blank())
  })
  
  # ---------------- Airlines ----------------
  
  output$half_eye <- renderPlot({
    data <- flown_data() %>%
      filter(dep_delay >= -30, dep_delay <= 120) %>%
      group_by(carrier_name) %>%
      filter(n() >= 100) %>%
      ungroup()
    
    validate(need(nrow(data) > 10, "Not enough data. Try selecting more airlines or months."))
    
    if (nrow(data) > 150000) {
      set.seed(42)
      data <- data %>% slice_sample(n = 150000)
    }
    
    data <- data %>% mutate(carrier_name = fct_reorder(carrier_name, dep_delay, median))
    
    ggplot(data, aes(x = dep_delay, y = carrier_name)) +
      stat_halfeye(
        .width = c(0.5, 0.95),
        adjust = 1.2,
        point_interval = "median_qi",
        fill = pal_main,
        slab_alpha = 0.75
      ) +
      labs(
        title = "Departure Delay by Airline",
        subtitle = "Density · dot = median · thick bar = middle 50% · thin bar = 95% range · airlines with 100+ flights, delays shown from -30 to 120 min",
        x = "Departure delay (min)",
        y = NULL
      ) +
      theme_us()
  })
  
  # ---------------- Compare groups (median / % delayed) ----------------
  
  output$compare_plot <- renderPlot({
    metric <- input$cmp_metric
    grp <- input$group_var
    thr <- input$delay_threshold
    
    data <- flown_data() %>%
      group_by(group = .data[[grp]]) %>%
      summarise(
        n = n(),
        value = metric_value(dep_delay, arr_delay, metric, thr),
        .groups = "drop"
      ) %>%
      filter(n >= 100, !is.na(value))
    
    validate(need(nrow(data) > 0, "No data for this comparison. Try selecting more airlines or months."))
    
    # Airlines and airports are ranked by value; months and distance bands keep their natural order
    data <- if (grp %in% c("carrier_name", "origin")) {
      data %>% mutate(group = fct_reorder(as.character(group), value))
    } else {
      data %>% mutate(group = fct_rev(group))
    }
    
    group_label <- names(which(c(
      "Airline" = "carrier_name", "Origin airport" = "origin",
      "Month" = "month_lbl", "Distance band" = "dist_bin"
    ) == grp))
    metric_label <- names(metric_choices)[metric_choices == metric]
    
    sub <- if (metric == "pct") {
      paste0("Share of flights leaving ", thr, "+ minutes late · groups with 100+ flights")
    } else {
      "Median is robust to the long tail of very late flights · groups with 100+ flights"
    }
    
    lollipop_plot(data, metric, title = paste(metric_label, "by", tolower(group_label)), subtitle = sub)
  })
  
  # ---------------- Cancellations ----------------
  
  output$cancellations <- renderPlot({
    data <- filtered_data() %>%
      filter(cancelled) %>%
      count(date, origin)
    
    validate(need(nrow(data) > 0, "No cancelled flights for the current filters."))
    
    worst_days <- data %>%
      group_by(date) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      slice_max(n, n = 3, with_ties = FALSE)
    
    ggplot(data, aes(date, n, fill = origin)) +
      geom_col(width = 1) +
      geom_text_repel(
        data = worst_days,
        aes(date, n, label = paste0(format(date, "%b %d"), ": ", comma(n))),
        inherit.aes = FALSE,
        nudge_y = 10, color = pal_dark, fontface = "bold", size = 3.6, seed = 42
      ) +
      scale_fill_manual(values = pal_origin, name = "Origin") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(
        title = "Cancelled Flights per Day",
        subtitle = "Stacked by origin airport; the three worst days are labeled",
        x = NULL,
        y = "Cancelled flights"
      ) +
      theme_us() +
      theme(legend.position = "bottom")
  })
  
  # ---------------- Worst routes ----------------
  
  output$worst_routes <- renderPlot({
    metric <- input$route_metric
    thr <- input$delay_threshold
    
    data <- flown_data() %>%
      group_by(route) %>%
      summarise(
        n = n(),
        value = metric_value(dep_delay, arr_delay, metric, thr),
        .groups = "drop"
      ) %>%
      filter(n >= input$min_route_flights, !is.na(value))
    
    validate(need(nrow(data) > 0, "No routes meet the minimum flight count. Lower the slider or widen the filters."))
    
    data <- data %>%
      slice_max(value, n = 15, with_ties = FALSE) %>%
      mutate(group = fct_reorder(route, value))
    
    metric_label <- names(metric_choices)[metric_choices == metric]
    sub <- paste0(
      if (metric == "pct") paste0("Share of flights leaving ", thr, "+ minutes late") else metric_label,
      " · top 15 routes with ", input$min_route_flights, "+ flights · route = origin-destination"
    )
    
    lollipop_plot(data, metric, title = "Worst Routes out of NYC", subtitle = sub)
  })
  
  # ---------------- Destination map ----------------
  
  output$dest_map <- renderPlot({
    data <- flown_data() %>%
      filter(
        !is.na(arr_delay), !is.na(dest_lat), !is.na(dest_lon),
        dest_lon >= -130, dest_lon <= -65, dest_lat >= 24, dest_lat <= 50
      ) %>%
      group_by(dest, dest_lat, dest_lon) %>%
      summarise(n = n(), avg_arr = mean(arr_delay), .groups = "drop") %>%
      filter(n >= 50)
    
    validate(need(nrow(data) > 3, "Not enough destinations for this map. Try selecting more months or airlines."))
    
    labels_df <- data %>% slice_max(n, n = 12, with_ties = FALSE)
    
    base_map <- if (requireNamespace("maps", quietly = TRUE)) {
      geom_polygon(
        data = ggplot2::map_data("state"),
        aes(x = long, y = lat, group = group),
        fill = "grey96", color = "grey80", linewidth = 0.2
      )
    } else {
      NULL
    }
    
    ggplot() +
      base_map +
      geom_segment(
        data = data,
        aes(x = nyc_lon, y = nyc_lat, xend = dest_lon, yend = dest_lat),
        color = pal_muted, alpha = 0.35
      ) +
      geom_point(data = data, aes(dest_lon, dest_lat, size = n, color = avg_arr), alpha = 0.9) +
      geom_text_repel(
        data = labels_df,
        aes(dest_lon, dest_lat, label = dest),
        size = 3.4, fontface = "bold", color = pal_dark, seed = 42, max.overlaps = Inf
      ) +
      scale_size_area(max_size = 10, labels = comma, name = "Flights") +
      scale_color_gradient2(
        low = pal_main, mid = "grey90", high = pal_bad, midpoint = 0,
        name = "Avg arrival\ndelay (min)"
      ) +
      coord_quickmap() +
      labs(
        title = "Destinations from NYC",
        subtitle = "Dot size = number of flights · color = average arrival delay · the 12 busiest destinations are labeled",
        x = NULL,
        y = NULL
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = pal_dark, size = 16),
        plot.subtitle = element_text(color = "grey45"),
        legend.position = "right"
      )
  })
  
  # ---------------- Weather ----------------
  
  output$weather_space <- renderPlot({
    validate(need(input$weather_x != input$weather_y, "Pick two different weather variables."))
    
    xv <- input$weather_x
    yv <- input$weather_y
    x_lab <- names(weather_choices)[weather_choices == xv]
    y_lab <- names(weather_choices)[weather_choices == yv]
    
    data <- flown_data() %>%
      filter(!is.na(.data[[xv]]), !is.na(.data[[yv]])) %>%
      filter(inside_q(.data[[xv]]), inside_q(.data[[yv]]))
    
    validate(need(nrow(data) > 10, "Not enough weather data available."))
    
    p <- ggplot(data, aes(x = .data[[xv]], y = .data[[yv]]))
    
    if (input$weather_fill == "count") {
      p <- p +
        geom_bin_2d(bins = 30) +
        scale_fill_viridis_c(option = "mako", labels = comma, name = "Flights")
      sub <- "2D histogram: darker/lighter cells show where flights depart most often"
    } else {
      p <- p +
        stat_summary_2d(aes(z = dep_delay), bins = 30) +
        scale_fill_viridis_c(option = "mako", direction = -1, name = "Avg delay\n(min)")
      sub <- "Each cell shows the average departure delay of flights in that weather range; sparse cells can be noisy"
    }
    
    p +
      labs(
        title = paste(x_lab, "x", y_lab),
        subtitle = paste0(sub, " · middle 99% of each variable"),
        x = x_lab,
        y = y_lab
      ) +
      theme_us()
  })
  
  # ---------------- Delay recovery ----------------
  
  output$delay_recovery <- renderPlot({
    data <- flown_data() %>%
      filter(!is.na(arr_delay), dep_delay >= -30, dep_delay <= 180, arr_delay >= -60, arr_delay <= 200)
    
    validate(need(nrow(data) > 10, "Not enough data for this plot."))
    
    if (nrow(data) > 150000) {
      set.seed(42)
      data <- data %>% slice_sample(n = 150000)
    }
    
    ggplot(data, aes(dep_delay, arr_delay)) +
      geom_bin_2d(bins = 45) +
      geom_abline(slope = 1, intercept = 0, color = pal_accent, linewidth = 0.9, linetype = "dashed") +
      scale_fill_viridis_c(option = "mako", trans = "log10", labels = comma, name = "Flights") +
      labs(
        title = "Late Departures Often Arrive Less Late",
        subtitle = "Dashed line: arrival delay = departure delay · cells below it are flights that made up time in the air",
        x = "Departure delay (min)",
        y = "Arrival delay (min)"
      ) +
      theme_us()
  })
  
  # ---------------- Data table ----------------
  
  output$data_table <- renderDT({
    filtered_data() %>%
      select(
        date, carrier_name, flight, origin, dest,
        sched_dep_time, dep_delay, arr_delay, air_time, distance,
        temp, humid, wind_speed, cancelled
      ) %>%
      head(1000) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        filter = "top"
      )
  })
}

shinyApp(ui = ui, server = server)