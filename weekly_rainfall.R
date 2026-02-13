library(rvest)
library(plotly)
library(htmlwidgets)
library(httr)
library(jsonlite)

# Get historical weather data
webpage = read_html("http://www.georgiaweather.net/?variable=7d&site=BYRON")
table_node = html_nodes(webpage, "table")
table_content <- html_table(table_node)[[6]]
colnames(table_content) = table_content[1,]
table_content = table_content[-1,]
table_content$`Rain  in` = as.numeric(table_content$`Rain  in`)
cumulative_rainfall_week = sum(table_content$`Rain  in`)
# Calculate rainfall for last 4 days
cumulative_rainfall_4days = sum(tail(table_content$`Rain  in`, 4))
colnames(table_content)[c(1,4,5,6,12)] <- c("Date","High (°F)","Low (°F)","R.H. (%)","Rain (in.)")

# Add data type column
table_content$Type = "Historical"
# get rid of unwanted columns number 2 and 3
table_content = table_content[, c("Date", "High (°F)", "Low (°F)", "R.H. (%)", "Rain (in.)", "Type")] 


# Get NOAA forecast data for Byron, GA (32.6518°N, 83.7588°W)
# First, get the forecast office and grid points
lat = 32.6518
lon = -83.7588

# Initialize in case forecast fails
cumulative_forecast_3days = 0

tryCatch({
  # Get grid point data
  points_url = paste0("https://api.weather.gov/points/", lat, ",", lon)
  points_response = GET(points_url, user_agent("Byron Rainfall Tracker"))
  points_data = fromJSON(content(points_response, "text"))
  
  # Get forecast
  forecast_url = points_data$properties$forecast
  forecast_response = GET(forecast_url, user_agent("Byron Rainfall Tracker"))
  forecast_data = fromJSON(content(forecast_response, "text"))
  
  # Extract next 7 days of forecast
  periods = forecast_data$properties$periods
  
  # Create forecast dataframe - process daytime periods
  forecast_df = data.frame(
    Date = character(),
    `High (°F)` = numeric(),
    `Low (°F)` = numeric(),
    `R.H. (%)` = character(),
    `Rain (in.)` = numeric(),
    Type = character(),
    stringsAsFactors = FALSE
  )
  
  # Process forecast periods (day/night pairs)
  for (i in seq(1, min(14, nrow(periods)), by = 2)) {
    day_period = periods[i, ]
    night_period = if (i + 1 <= nrow(periods)) periods[i + 1, ] else NULL
    
    # Extract date
    date_str = format(as.Date(day_period$startTime), "%b %d")
    
    # Get high temp (from day)
    high_temp = ifelse(day_period$isDaytime, day_period$temperature, 
                       ifelse(!is.null(night_period), night_period$temperature, NA))
    
    # Get low temp (from night)
    low_temp = ifelse(!is.null(night_period) && !night_period$isDaytime, 
                     night_period$temperature, day_period$temperature)
    
    # Estimate rainfall from precipitation probability
    precip_prob = day_period$probabilityOfPrecipitation$value
    if (is.null(precip_prob)) precip_prob = 0
    
    # Simple estimation: if >50% chance, estimate 0.1-0.3 inches based on probability
    rain_est = ifelse(precip_prob > 50, (precip_prob - 50) / 100 * 0.3, 0)
    
    forecast_df = rbind(forecast_df, data.frame(
      Date = date_str,
      `High (°F)` = high_temp,
      `Low (°F)` = low_temp,
      `R.H. (%)` = "-",
      `Rain (in.)` = round(rain_est, 2),
      Type = "Forecast",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
    
    if (nrow(forecast_df) >= 7) break
  }
  
  # Combine historical and forecast data
  table_content = rbind(table_content, forecast_df)
  
  # Calculate projected rainfall for next 3 days
  cumulative_forecast_3days = sum(head(forecast_df$`Rain (in.)`, 3))
  
}, error = function(e) {
  message("Error fetching NOAA forecast: ", e$message)
  message("Continuing with historical data only")
})

table_content$Date = factor(table_content$Date, levels=table_content$Date)

# Create plotly bar and line graph

# Separate historical and forecast data
historical_data = table_content[table_content$Type == "Historical", ]
forecast_data = table_content[table_content$Type == "Forecast", ]

# Create a single plotly plot with dual y-axes
fig <- plot_ly()

# Historical temperature lines (solid)
fig <- fig %>% add_lines(x = historical_data$Date, y = as.numeric(historical_data$`High (°F)`), 
                         name = 'High Temp', line = list(color = 'red'), yaxis = 'y1')
fig <- fig %>% add_lines(x = historical_data$Date, y = as.numeric(historical_data$`Low (°F)`), 
                         name = 'Low Temp', line = list(color = 'blue'), yaxis = 'y1')

# Forecast temperature lines (dotted) with connecting segment
if (nrow(forecast_data) > 0) {
  # Add connecting dotted lines from last historical to first forecast
  last_hist_idx = nrow(historical_data)
  connect_dates_high = c(historical_data$Date[last_hist_idx], forecast_data$Date[1])
  connect_temps_high = c(as.numeric(historical_data$`High (°F)`[last_hist_idx]), 
                         as.numeric(forecast_data$`High (°F)`[1]))
  connect_dates_low = c(historical_data$Date[last_hist_idx], forecast_data$Date[1])
  connect_temps_low = c(as.numeric(historical_data$`Low (°F)`[last_hist_idx]), 
                        as.numeric(forecast_data$`Low (°F)`[1]))
  
  fig <- fig %>% add_lines(x = connect_dates_high, y = connect_temps_high,
                           line = list(color = 'red', dash = 'dot'), yaxis = 'y1',
                           showlegend = FALSE)
  fig <- fig %>% add_lines(x = connect_dates_low, y = connect_temps_low,
                           line = list(color = 'blue', dash = 'dot'), yaxis = 'y1',
                           showlegend = FALSE)
  
  # Add forecast lines
  fig <- fig %>% add_lines(x = forecast_data$Date, y = as.numeric(forecast_data$`High (°F)`), 
                           name = 'High Temp (Forecast)', 
                           line = list(color = 'red', dash = 'dot'), yaxis = 'y1')
  fig <- fig %>% add_lines(x = forecast_data$Date, y = as.numeric(forecast_data$`Low (°F)`), 
                           name = 'Low Temp (Forecast)', 
                           line = list(color = 'blue', dash = 'dot'), yaxis = 'y1')
}

# Historical rainfall bars
fig <- fig %>% add_bars(x = historical_data$Date, y = historical_data$`Rain (in.)`, 
                        name = 'Rainfall', marker = list(color = 'lightblue'), yaxis = 'y2', opacity = 0.5)

# Forecast rainfall bars (lighter blue)
if (nrow(forecast_data) > 0) {
  fig <- fig %>% add_bars(x = forecast_data$Date, y = forecast_data$`Rain (in.)`, 
                          name = 'Rainfall (Forecast)', marker = list(color = 'lightblue'), 
                          yaxis = 'y2', opacity = 0.3)
}

fig <- fig %>% layout(
  xaxis = list(title = '', automargin = TRUE, tickangle = -45),
  yaxis = list(title = 'Temperature (°F)', automargin = TRUE),
  yaxis2 = list(title = 'Rainfall (inches)', overlaying = 'y', side = 'right', automargin = TRUE),
  legend = list(orientation = 'h', x = 0.5, xanchor = 'center'),
  margin = list(t = 80, b = 120)
)
saveWidget(fig, "stats_plotly.html", selfcontained = T)

# Create HTML output

# Create table with forecast rows styled in light blue
table_rows = ""
for (i in 1:nrow(table_content)) {
  row = table_content[i, ]
  row_style = if (row$Type == "Forecast") ' style="background-color: #ADD8E6;"' else ''
  table_rows = paste0(table_rows,
    '<tr', row_style, '>\n',
    '<td>', row$Date, '</td>\n',
    '<td>', row$`High (°F)`, '</td>\n',
    '<td>', row$`Low (°F)`, '</td>\n',
    '<td>', row$`Rain (in.)`, '</td>\n',
    '</tr>\n'
  )
}

html_table = paste0(
  '<table>\n<thead>\n<tr>\n',
  '<th>Date</th>\n',
  '<th>High (°F)</th>\n',
  '<th>Low (°F)</th>\n',
  '<th>Rain (in.)</th>\n',
  '</tr>\n</thead>\n<tbody>\n',
  table_rows,
  '</tbody>\n</table>'
)

html_content <- paste0(
  '<!DOCTYPE html>\n<html>\n<head>\n',
  '<title>Byron, GA, 7 day Rainfall Report</title>\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<style>\n',
  'body { font-family: Arial, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }\n',
  'table { border-collapse: collapse; width: 100%; margin: 20px 0; }\n',
  'th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n',
  'th { background-color: #f2f2f2; }\n',
  '.summary { background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 20px 0; }\n',
  'iframe { width: 100%; height: 600px; border: none; }\n',
  '</style>\n</head>\n<body>\n',
  '<h1>Weekly Rainfall Report</h1>\n',
  '<div class="summary">\n',
  '<p><strong>Cumulative rainfall for the past week:</strong> ', cumulative_rainfall_week, ' inches</p>\n',
  '<p><strong>Recommended irrigation:</strong> ', max(0, 1-cumulative_rainfall_week), ' inches</p>\n',
  '<p><strong>Recommended irrigation (last 4 days + 3 day projection):</strong> ', max(0, 1-(cumulative_rainfall_4days + cumulative_forecast_3days)), ' inches</p>\n',
  '</div>\n',
  '<h2>Detailed Data</h2>\n',
  html_table,
  '\n<h2>Rainfall & Temps</h2>\n',
  '<iframe src="./stats_plotly.html"></iframe>\n',
  '\n<p><em>Last updated: ', format(Sys.time(), "%B %d, %Y %H:%M:%S"), '</em></p>\n',
  '</body>\n</html>'
)

# Write the HTML file
writeLines(html_content, "index.html")
