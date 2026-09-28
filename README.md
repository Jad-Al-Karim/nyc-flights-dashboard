# NYC Flights Interactive Dashboard (R Shiny)

An interactive dashboard for exploring every flight that left New York City in 2013 (JFK, LGA, EWR), built on the `nycflights13` package. It covers when flights run late, which airlines and routes are worst, when flights are cancelled, and how weather and distance relate to delays.

![Dashboard overview]("C:\Users\NTC\Documents\R projects\nyc-flights-dashboard\overview.png")

![Worst days trend](docs/worst-days.png)

## Quick start

Clone the repository and open the folder in R or RStudio:

```bash
git clone https://github.com/<your-username>/nyc-flights-dashboard.git
cd nyc-flights-dashboard
```

Then install the packages and run the app:

```r
install.packages(c(
  "shiny", "tidyverse", "lubridate", "scales", "viridis",
  "ggdist", "ggrepel", "DT", "nycflights13"
))

# Optional: adds state outlines to the destination map
install.packages("maps")

shiny::runApp()
```

If a required package is missing, the app stops at startup and prints the exact `install.packages()` call you need.

## Data

| Table | Use |
|---|---|
| `flights` | ~337k flights: delays, times, origin, destination, distance |
| `airlines` | Carrier names (suffixes like "Inc." and "Airlines" are stripped) |
| `airports` | Destination names and coordinates for the map |
| `weather` | Hourly weather per origin airport, joined on `origin` + `time_hour` |

Notes:
- A flight is **cancelled** when it has no departure time (`is.na(dep_time)`).
- Delay plots use only flights that departed. Cancellations are counted in the Overview and shown in their own tab.
- Weather has a few duplicate airport-hours, so one row per airport-hour is kept before joining.
- A `route` is `origin-destination`, for example `JFK-LAX`.
- `max_rows` at the top of the script caps the number of flights. The default (400,000) is above the dataset size, so nothing is sampled. Lower it if the app feels slow.

## Sidebar filters

These apply to every tab.

| Filter | Effect |
|---|---|
| Origin airport | EWR / JFK / LGA |
| Airline | Multi-select carriers |
| Month range | 1-12 |
| Delayed after (min) | Threshold used for "share delayed" (5-60, default 15) |
| Exclude extreme delays | Drops departures more than 3 hours late from delay plots |

## Tabs and plots

| Tab | Plot | What it shows |
|---|---|---|
| Overview | Metric cards | Flight count, cancellation rate, average delay, share delayed, busiest destination, most delayed airline |
| Worst Days | Daily trend | Average delay per day with a LOESS smoother; the three worst days are labeled |
| Seasonality | Monthly variation | Median day, middle 50% of days, and best-to-worst day for each month |
| Time of Day | Hourly delay | Average delay by scheduled hour and airport, with 95% confidence bands |
| Time of Day | Weekday x hour heatmap | Average delay for each weekday and hour |
| Airlines | Half-eye | Delay distribution per airline (density, median, 50% and 95% intervals) |
| Compare Groups | Lollipop | Group by airline, origin, month, or distance band; metric is share delayed, median departure delay, or median arrival delay |
| Cancellations | Stacked daily columns | Cancelled flights per day by origin; the three worst days are labeled |
| Routes & Map | Worst routes | Top 15 routes by share delayed or median delay, with an adjustable minimum flight count |
| Routes & Map | Destination map | Continental US destinations: dot size = flights, color = average arrival delay |
| Weather Space | 2D bins | Any two weather variables, colored by flight count or average delay |
| Delay Recovery | Dep vs arr delay | Flights below the dashed line made up time in the air |
| Data Table | Searchable table | First 1,000 filtered records |

## Design notes

- **Median and share delayed instead of mean with CI.** Delay distributions are heavily right-skewed, so a handful of very late flights dominate means. The comparison charts use medians or the share of flights past the delay threshold, which are more stable and easier to read.
- **Minimum sample sizes.** Groups need 100+ flights (airlines, comparisons), 50+ (map destinations), or the routes slider value, so small groups do not produce misleading rankings.
- **Consistent look.** All ggplot charts share `theme_us()` and one palette; airports keep the same color (EWR navy, JFK amber, LGA green) across plots.
- **Reusable helper.** `lollipop_plot()` draws both the group comparison and the worst-routes chart. Add a metric by extending `metric_choices` and `metric_value()`.

## Troubleshooting

- **Map has no state outlines:** install the `maps` package.
- **Slow first load:** the joins run once at startup; plots are rendered on demand.
- **"Not enough data" messages:** the current filters are too narrow; widen the month range or select more airlines or airports.
- **Fonts:** the UI loads Inter from Google Fonts and falls back to Arial when offline.

## Files

- `app.R`: the complete dashboard (data prep, UI, server)
- `README.md`: this file
- `docs/`: screenshots used in this README
- `.gitignore`: ignores R session files
