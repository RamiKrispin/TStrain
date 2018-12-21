library(TSstudio)
data("USgas")

md1 <- forecast::auto.arima(USgas)
md2 <- forecast::ets(USgas)
md3 <- HoltWinters(USgas)
md4 <- forecast::nnetar(USgas, P = 2, p = 3, repeats = 50)
md5 <- forecast::holt(USgas)
md6 <- stl(USgas, t.window=13, s.window="periodic", robust=TRUE)
md7 <- forecast::Arima(USgas, order(c(1,0,0), seasonal = c(0,1,0)))

ts_sim <- function(model, h, n, sim_color = "blue", opacity = 0.05){
  
  # Setting variables
  s <- s1 <- sim_output <- p <- output <- NULL
  
  # Error handling
  if(!any(class(model) %in% c("ARIMA", "ets", "nnetar"))){
    stop("The model argument is not valid")
  }
  if(!is.numeric(h)){
    stop("The value of the 'h' argument is not valid")
  } else if(h %% 1 != 0){
    stop("The 'h' argument is not integer")
  } else if(h < 1){
    stop("The value of the 'h' argument is not valid")
  }
  
  s <- lapply(1:n, function(i){
    sim <- sim_df <- NULL
    sim <- stats::simulate(model,nsim = h)
    sim_df <- base::data.frame(x = base::as.numeric(stats::time(sim)), 
                               y = base::as.numeric(sim))
    sim_df$n <- base::paste0("sim_", i)
    return(sim_df)
  }) 
  sim_output <- s %>% dplyr::bind_rows() %>% 
    tidyr::spread(key = n, value = y) %>% 
    dplyr::select(-x) %>% 
    ts(start = stats::start(stats::simulate(model,nsim = 1)), 
       frequency = stats::frequency(stats::simulate(model,nsim = 1))) 
  
  p <- plotly::plot_ly()
  
  for(i in 1:n){
    p <- p %>% plotly::add_lines(x = s[[i]]$x, y = s[[i]]$y, 
                                 line = list(color = sim_color), 
                                 opacity = opacity, showlegend = FALSE, 
                                 name = paste("Sim", i, sep = " "))
  }
  s1 <- s %>% dplyr::bind_rows() %>% dplyr::group_by(x) %>%
    dplyr::summarise(p50 = median(y))
  p <- p %>% plotly::add_lines(x = s1$x, y = s1$p50, 
                               
                               line = list(color = "#00526d", 
                                           dash = "dash", 
                                           width = 3), name = "Median") 
  
  p <- p %>% plotly::add_lines(x = time(model$x), 
                               y = model$x, 
                               line = list(color = "#00526d"), 
                               name = "Actual")
  print(p)
  
  output <- list()
  output$plot <- p
  output$forecast_sim <- sim_output
  output$series <- model$x
  return(output)
}

p <-  ts_sim(model = md6, h = 60, n = 100, sim_color = "blue", opacity = 0.03)

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
