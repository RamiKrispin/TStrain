library(TSstudio)
data("USgas")

md <- forecast::auto.arima(USgas)
fc <- forecast::forecast(md, h = 60)
ts_sim <- function(model, h, n){
  s <- NULL
  s <- lapply(1:n, function(i){
    sim <- NULL
    sim <- stats::simulate(model,nsim = h)
    sim_df <- base::data.frame(x = base::as.numeric(stats::time(sim)), y = base::as.numeric(sim))
    sim_df$n <- paste0("test_", i)
    return(sim_df)
  }) 
  s %>% dplyr::bind_rows() %>% tidyr::spread(key = n, value = y)
  p <- plotly::plot_ly()
  
  for(i in 1:n){
    p <- p %>% plotly::add_lines(x = s[[i]]$x, y = s[[i]]$y, line = list(color = "blue"), opacity = 0.05)
  }
  s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
    dplyr::summarise(p50 = median(y))
  p <- p %>% plotly::add_lines(x = s1$x, y = s1$p50, 
                               
                               line = list(color = "black", 
                                           dash = "dash", 
                                           width = 3)) 
  
  p <- p %>% plotly::add_lines(x = time(model$x), y = model$x, line = list(color = "#00526d"))
  
  return(p)
}

p <-  ts_sim(model = md, h = 60, n = 100)
p
p1 <- plot_forecast(fc)
length(s)
plotly::subplot(p, p1, nrows = 2)
s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
  dplyr::summarise(p50 = median(y))
p %>% plotly::add_lines(x = s1$x, y = s1$p50, line = list(color = "black", dash = "dash", width = 3)) 

x <- 1:5
mod1 <- lm(c(1:3, 7, 6) ~ x)
S1 <- simulate(mod1, nsim = 4)
## repeat the simulation:
.Random.seed <- attr(S1, "seed")
