p_forecast <- function(fc, show_legend = TRUE, ci_col = c("#ADDD8E", "#D9F0A3")){
  p <- NULL
  if(class(fc) == "forecast"){
    p <- plotly::plot_ly() %>% 
      plotly::add_lines(x = time(fc$x), y = fc$x, name = "Actual",
                        line = list(color =  "rgb(22, 96, 167)"),
                        showlegend = show_legend) %>%
      plotly::add_ribbons(x = time(fc$mean), ymin = fc$lower[, 2], ymax = fc$upper[, 2],
                          line = list(color = ci_col[2]),
                          fillcolor = ci_col[2], name = "95% confidence") %>%
      plotly::add_ribbons(x = time(fc$mean), ymin = fc$lower[, 1], ymax = fc$upper[, 1],
                          line = list(color = ci_col[1]),
                          fillcolor = ci_col[1], name = "80% confidence") %>%
      plotly::add_lines(x = time(fc$mean), y = fc$mean, name = "Forecast",
                        showlegend = show_legend,
                        line = list(dash = "dash", color =  "rgb(22, 96, 167)"))
  } else if(class(fc) == "bsts.prediction"){
    p <- plotly::plot_ly() %>% 
      plotly::add_lines(x = zoo::index(fc$original.series), y = fc$original.series, name = "Actual",
                        line = list(color =  "rgb(22, 96, 167)"),
                        showlegend = show_legend) %>%
      plotly::add_ribbons(x = (max(zoo::index(fc$original.series)) + 1):(max(zoo::index(fc$original.series)) + length(fc$mean)), 
                          ymin = fc$interval[1, ], ymax = fc$interval[2, ],
                          line = list(color = ci_col[2]),
                          fillcolor = ci_col[2], name = "95% confidence") %>%
      plotly::add_lines(x = (max(zoo::index(fc$original.series)) + 1):(max(zoo::index(fc$original.series)) + length(fc$mean)), 
                        y = fc$mean, name = "Forecast",
                        showlegend = show_legend,
                        line = list(dash = "dash", color =  "rgb(22, 96, 167)"))
  }
  
  return(p)
  
}