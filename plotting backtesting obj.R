#' Ploting Backtesting Object
#' @export
#' @param backtesting.obj A ts_backtesting object (ts_backtesting function output)
#' @param by A string, defines the error metric to be used in the plot c("MAPE", "RMSE")
#' @param type A string, defines the type of plot to display, c("period", "box", "forecast", "all").
#' 
#' "period" - plot the error rate by testing period of the selected models
#' "box" - box-plot representive of the error rate of the selected models
#' "forecast" - plot the lead forecast (by the selected error metric)
#' "all" - plot the three options above together (default)
#' @param palette A string, set the color palette to be used in the plot. Can select any of the color palettes available in the RColorBrewer or viridis packages
#' @param top An integer, optional, select the top models by accuarcy (according to the selected error metric). 
#' If set to NULL (default) will display all available models


plot_backtesting <- function(backtesting.obj, by = "MAPE", type = "all", palette = "viridis", top = NULL){

  obj.name <- output_plot <- NULL
  output_plot <- list()
  obj.name <- backtesting.obj$parameters$series.name
# Error handling  
# Setting the color palette
# Checking the palette argument
brewer_palettes <- row.names(RColorBrewer::brewer.pal.info)
viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")

if(!palette %in% c(brewer_palettes, viridis_palettes)){
  warning("The 'palette' argument is not valid, using default option ('BrBG'")
  palette <- "BrBG"
}


if(!base::is.null(top)){
  if(top %% 1 != 0 || top > base::nrow(backtesting.obj$leaderboard)){
    warning("The value of the 'top' argument is not valid, will plot all available models")
    top <- base::nrow(backtesting.obj$leaderboard)
  }
} else if(base::is.null(top)){
  top <- base::nrow(backtesting.obj$leaderboard)
}



# Setting the color palette
if(palette %in% viridis_palettes){
  color_ramp <- viridis::viridis_pal(option = base::eval(palette))(top)
} else if(palette %in% brewer_palettes){
  n_colors <- NULL
  n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
  color_ramp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(top)
}

results_df <- backtesting.obj$results
leaderboard <- backtesting.obj$leaderboard %>% dplyr::arrange(!!base::as.symbol(base::paste("avg", by, sep = "")))


leaderboard$model_type <- base::paste(leaderboard$model, " (", base::substr(leaderboard$window_type, 1, 1), ")", sep = "")
results_df$model_type <- base::factor(base::paste(results_df$model, 
                                                  " (", 
                                                  base::substr(results_df$window_type, 1, 1),
                                                  ")", 
                                                  sep = ""), 
                                      levels = leaderboard$model_type)



m <- base::levels(results_df$model_type)[1:top]
p1 <- plotly::plot_ly()
p2 <- plotly::plot_ly()


for(i in seq_along(m)){
  p_df <- NULL
  p_df <- results_df %>% dplyr::filter(model_type == m[i])
  p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                 y = p_df[, tolower(by)], 
                                 name = p_df$model_type, 
                                 legendgroup = p_df$model_type, 
                                 line = list(color = color_ramp[i])) %>% 
    plotly::layout(yaxis = list(title = by),
                   xaxis = list(title = "Period"))
  
  p2 <- p2 %>% plotly::add_trace(y = p_df[, base::tolower(by)],
                                 type = "box",
                                 boxpoints = "all",
                                 jitter = 0.3,
                                 pointpos = -1.8, 
                                 name = p_df$model_type, 
                                 legendgroup = p_df$model_type,
                                 line = list(color = color_ramp[i]),
                                 marker = list(color = color_ramp[i]),
                                 showlegend=F) %>% 
    plotly::layout(title = base::paste("Backtesting Models Error Rate (",
                                       by,")", sep = ""),
                   yaxis = list(title = by),
                   xaxis = list(title = "Model",
                                tickangle = 45,
                                tickfont = list(size = 8)))
}



p3 <- TSstudio::plot_forecast(backtesting.obj$forecast[[base::paste(backtesting.obj$leaderboard$model[1], 
                                                                 base::substr(backtesting.obj$leaderboard$window_type[1], 1, 1),
                                                                 sep = "_" )]][["forecast"]]) %>%
  plotly::layout(annotations = list(
    text = paste(obj.name, " Best Forecast by ", error, " - ", 
                 backtesting.obj$leaderboard$model[1]," with ", 
                 base::toupper(base::substr(backtesting.obj$leaderboard$window_type[1], 1, 1)), 
                 base::substr(backtesting.obj$leaderboard$window_type[1], 2, 
                              base::nchar(backtesting.obj$leaderboard$window_type[1])),
                 " Window",  sep = ""),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  ))
# output_plot$plot3 <- p3 
output_plot$summary_plot <- plotly::subplot(plotly::subplot(p1, p2, 
                                                             shareY = TRUE, 
                                                             titleX = TRUE, 
                                                             titleY = TRUE, 
                                                             nrows = 1),
                                             titleY = TRUE,
                                             p3, nrows = 2, margin = 0.1) %>%
  plotly::layout(title = "Error Dist. by Period/Model")


return(output_plot)
}


c("viridis", "magma", "plasma", "inferno", "cividis")
palette = "viridis"
plot_backtesting(x, palette = palette)
plot_backtesting(x, top = 5, palette = palette)
