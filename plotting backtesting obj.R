str(backtesting.obj, max.level = 1)
names(backtesting.obj)



top <- nrow(backtesting.obj$leaderboard)


# Setting the color palette
# Checking the palette argument
brewer_palettes <- row.names(RColorBrewer::brewer.pal.info)
viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")

if(!palette %in% c(brewer_palettes, viridis_palettes)){
  warning("The 'palette' argument is not valid, using default option ('BrBG'")
  palette <- "BrBG"
}


if(palette %in% viridis_palettes){
  color_ramp <- viridis::viridis_pal(option = base::eval(palette))(top)
} else if(palette %in% brewer_palettes){
  n_colors <- NULL
  n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
  color_ramp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(top)
}

results_df <- backtesting.obj$results
head(results_df)

leaderboard <- backtesting.obj$leaderboard %>% dplyr::arrange(!!base::as.symbol(base::paste("avg", error, sep = "")))
leaderboard

leaderboard$model_type <- base::paste(leaderboard$model, " (", base::substr(leaderboard$window_type, 1, 1), ")", sep = "")
results_df$model_type <- base::factor(base::paste(results_df$model, " (", base::substr(results_df$window_type, 1, 1), ")", sep = ""), levels = leaderboard$model_type)



m <- base::levels(results_df$model_type)
p1 <- plotly::plot_ly()
p2 <- plotly::plot_ly()


for(i in seq_along(m)){
  p_df <- NULL
  p_df <- results_df %>% dplyr::filter(model_type == m[i])
  p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                 y = p_df[, tolower(error)], 
                                 name = p_df$model_type, 
                                 legendgroup = p_df$model_type, 
                                 line = list(color = color_ramp[i])) %>% 
    plotly::layout(yaxis = list(title = error),
                   xaxis = list(title = "Period"))
  
  p2 <- p2 %>% plotly::add_trace(y = p_df[, base::tolower(error)],
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
                                       error,")", sep = ""),
                   yaxis = list(title = error),
                   xaxis = list(title = "Model",
                                tickangle = 45,
                                tickfont = list(size = 8)))
}

