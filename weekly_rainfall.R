library(rvest)
library(plotly)
library(htmlwidgets)

webpage = read_html("http://www.georgiaweather.net/?variable=7d&site=BYRON")
table_node = html_nodes(webpage, "table")
table_content <- html_table(table_node)[[6]]
colnames(table_content) = table_content[1,]
table_content = table_content[-1,]
table_content$`Rain  in` = as.numeric(table_content$`Rain  in`)
cumulative_rainfall_week = sum(table_content$`Rain  in`)
colnames(table_content)[c(1,4,5,6,12)] <- c("Date","High (°F)","Low (°F)","R.H. (%)","Rain (in.)")

# Create plotly bar and line graph

# Temperature plot (top)

# Create a single plotly plot with dual y-axes
fig <- plot_ly()
fig <- fig %>% add_lines(x = table_content$Date, y = as.numeric(table_content$`High (°F)`), name = 'High Temp', line = list(color = 'red'), yaxis = 'y1')
fig <- fig %>% add_lines(x = table_content$Date, y = as.numeric(table_content$`Low (°F)`), name = 'Low Temp', line = list(color = 'blue'), yaxis = 'y1')
fig <- fig %>% add_bars(x = table_content$Date, y = table_content$`Rain (in.)`, name = 'Rainfall', marker = list(color = 'lightblue'), yaxis = 'y2', opacity = 0.5)
fig <- fig %>% layout(
  #title = list(text = 'Rainfall and Temperatures over last 7 days', xref = 'paper', x = 0.5, font = list(size = 24)),
  xaxis = list(title = '', automargin = TRUE, tickangle = -45),
  yaxis = list(title = 'Temperature (°F)', automargin = TRUE),
  yaxis2 = list(title = 'Rainfall (inches)', overlaying = 'y', side = 'right', automargin = TRUE),
  legend = list(orientation = 'h', x = 0.5, xanchor = 'center'),
  margin = list(t = 80, b = 120)
)
saveWidget(fig, "stats_plotly.html", selfcontained = T)

# Create HTML output

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
  '<p><strong>Recommended irrigation:</strong> ', 1-cumulative_rainfall_week, ' inches</p>\n',
  '</div>\n',
  '<h2>Detailed Data</h2>\n',
  knitr::kable(table_content[,c(1,4,5,12)], format = "html"),
  '\n<h2>Rainfall & Temps</h2>\n',
  '<iframe src="https://raw.githubusercontent.com/jeekinlau/byron_rainfall/refs/heads/main/stats_plotly.html"></iframe>\n',
  '\n<p><em>Last updated: ', format(Sys.time(), "%B %d, %Y %H:%M:%S"), '</em></p>\n',
  '</body>\n</html>'
)

# Write the HTML file
writeLines(html_content, "index.html")
