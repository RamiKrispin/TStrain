# Tunning the gamma parameter of Holt Winters model
?HoltWinters

# Using the USgas dataset as an example
library(TSstudio)
data(USgas)
ts <- USgas


# Setting the window parameters
periods <- 6

window_size <- 12

window_space <- 1

h <- 12

# Identify init alpha and beta parameters
md1 <- HoltWinters(ts)
summary(md1)
md1$coefficients


alpha <- md1$alpha
beta <- md1$beta

# Setting a sequence for the gamma parameter
gamma <- seq(0, 1, 0.01)
gamma[1] <- gamma[1] + 0.0001


s <- length(ts) - window_space * periods
e <- length(ts) 
w <- seq(from = s, by = window_space, to = e)

# Testing the sequance of gamma parameters over a window of 7 periods
holt_output <- lapply(w, function(i){
  ts_sub <- gamma_df <-  NULL
  ts_sub <- stats::window(ts, start = stats::start(ts), end = stats::time(ts)[i])
  partition <- TSstudio::ts_split(ts_sub, sample.out = h)
  train <- partition$train
  test <- partition$test
  gamma_df <- data.frame(gamma = gamma, 
                         MAPE = rep(NA, length(gamma)), 
                         RMSE = rep(NA, length(gamma)))
  for(i in seq_along(gamma)){
    md <- HoltWinters(train, alpha = alpha, beta = beta, gamma = gamma[i])
    fc <- forecast::forecast(md, h = h)
    gamma_df$MAPE[i]  <- forecast::accuracy(fc, test)[10]
    gamma_df$RMSE[i] <- forecast::accuracy(fc, test)[4]
    
  }
  return(gamma_df)
}
)

p_mape <- plotly::plot_ly()
for(i in 1:length(holt_output)){
  p_mape <- p_mape %>%plotly::add_lines(x = holt_output[[i]]$gamma, y = holt_output[[i]]$MAPE, name = i)
}
p_mape <- p_mape %>% plotly::layout(title = "MAPE as function of gamma", 
                          yaxis = list(title = "MAPE"),
                          xaxis = list(title = "gamma"))


p_rmse <- plotly::plot_ly()
for(i in 1:length(holt_output)){
  p_rmse <- p_rmse %>%plotly::add_lines(x = holt_output[[i]]$gamma, y = holt_output[[i]]$RMSE, name = i)
}
p_rmse <- p_rmse %>% plotly::layout(title = "RMSE as function of gamma", 
                          yaxis = list(title = "RMSE"),
                          xaxis = list(title = "gamma"))
p_rmse
