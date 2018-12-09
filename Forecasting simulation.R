library(TSstudio)
data("USgas")

md <- forecast::auto.arima(USgas)
fc <- forecast::forecast(md, h = 60)
ts_sim <- function(model, h, n){
  s <- NULL
  s <- lapply(1:n, function(i){
    sim <- NULL
    sim <- stats::simulate(model,nsim = h)
    sim_df <- base::data.frame(x = stats::time(sim), y = base::as.numeric(sim))
    return(sim_df)
  }) 
  
  p <- plotly::plot_ly()
  
  for(i in 1:n){
    p <- p %>% plotly::add_lines(x = s[[i]]$x, y = s[[i]]$y, line = list(color = "blue"), opacity = 0.05,)
  }
  s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
    dplyr::summarise(p50 = median(y))
  p <- p %>% plotly::add_lines(x = s1$x, y = s1$p50, 
                               
                               line = list(color = "black", 
                                           dash = "dash", 
                                           width = 3)) 
  
  p <- p %>% plotly::add_lines(x = time(model$x), y = model$x)
  
  return(p)
}

p <-  ts_sim(model = md, h = 60, n = 50)
p
p1 <- plot_forecast(fc)
length(s)
plotly::subplot(p, p1, nrows = 2)
s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
  dplyr::summarise(p50 = median(y))
p %>% plotly::add_lines(x = s1$x, y = s1$p50, line = list(color = "black", dash = "dash", width = 3)) 
