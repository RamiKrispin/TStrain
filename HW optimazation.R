# Tunning the gamma parameter of Holt Winters model
?HoltWinters

# Using the USgas dataset as an example
library(TSstudio)
data(USgas)
USgas_split <- ts_split(USgas, sample.out = h)

USgas_train <- USgas_split$train
USgas_test <- USgas_split$test

# Setting the window parameters
periods <- 6

window_size <- 12

window_space <- 1

h <- 12

# Setting a sequence for the gamma parameter
alpha <- seq(0, 1, 0.1)
beta <- seq(0, 1, 0.1)
gamma <- seq(0, 1, 0.1)
alpha[1] <- alpha[1] + 0.0001
beta[1] <- beta[1] + 0.0001
gamma[1] <- gamma[1] + 0.0001

grid_df1 <- base::expand.grid(alpha, beta, gamma)
names(grid_df1) <- c("alpha", "beta", "gamma")
head(grid_df1)
tail(grid_df1)

s <- length(USgas_train) - window_space * (periods - 1)
e <- length(USgas_train) 
w <- seq(from = s, by = window_space, to = e)
score_df <- as.data.frame(matrix(NA, ncol = length(w), nrow = nrow(grid_df1)))
names(score_df) <- paste0("period_", 1:length(w), sep = "")
grid_df <- cbind(grid_df1, score_df)
head(grid_df)
# Testing the sequance of gamma parameters over a window of 7 periods
  
  
for(n in 1:length(w)){
    ts_sub <- gamma_df <-  NULL
    ts_sub <- stats::window(USgas_train, 
                            start = stats::start(USgas_train), 
                            end = stats::time(USgas_train)[w[n]])
    partition <- TSstudio::ts_split(ts_sub, sample.out = h)
    train <- partition$train
    test <- partition$test
    for(i in 1:nrow(grid_df)){
      md <- fc <- NULL
      md <- HoltWinters(train, alpha = grid_df$alpha[i], 
                        beta = grid_df$beta[i], 
                        gamma = grid_df$gamma[i])
      fc <- forecast::forecast(md, h = h)
      grid_df[i, n + 3]  <- forecast::accuracy(fc, test)[10]
      
    }
    print(n)
  }

grid_df$mean <- (grid_df$period_1 + grid_df$period_2  + grid_df$period_3  + 
                grid_df$period_4 + grid_df$period_5 + grid_df$period_6) / 6


grid_df <- grid_df %>% dplyr::arrange(mean)
grid_top <- grid_df[1:3 , ]


alpha <- seq(min(grid_top$alpha), max(grid_top$alpha), 0.01)
beta <- seq(min(grid_top$beta), max(grid_top$beta), 0.01)
gamma <- seq(min(grid_top$gamma), max(grid_top$gamma), 0.01)
alpha[1] <- alpha[1] + 0.0001
beta[1] <- beta[1] + 0.0001
gamma[1] <- gamma[1] + 0.0001



md <- HoltWinters(USgas_train, alpha = grid_df$alpha[r], 
                  beta = grid_df$beta[r], 
                  gamma = grid_df$gamma[r])
fc <- forecast::forecast(md, h = h)
forecast::accuracy(fc, USgas_test)

md1 <- HoltWinters(USgas_train)
fc1 <- forecast::forecast(md1, h = h)
forecast::accuracy(fc1, USgas_test)

holt_output <- lapply(w, function(i){
  ts_sub <- gamma_df <-  NULL
  ts_sub <- stats::window(ts, start = stats::start(ts), end = stats::time(ts)[i])
  partition <- TSstudio::ts_split(ts_sub, sample.out = h)
  train <- partition$train
  test <- partition$test
  gamma_df <- data.frame(gamma = gamma, 
                         MAPE = rep(NA, length(gamma)), 
                         RMSE = rep(NA, length(gamma)))
  for(i in 1:nrow(grid_df)){
    md <- HoltWinters(train, alpha = grid_df$alpha[i], 
                      beta = grid_df$beta[i], 
                      gamma = grid_df$gamma[i])
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

p_mape


p_rmse <- plotly::plot_ly()
for(i in 1:length(holt_output)){
  p_rmse <- p_rmse %>%plotly::add_lines(x = holt_output[[i]]$gamma, y = holt_output[[i]]$RMSE, name = i)
}
p_rmse <- p_rmse %>% plotly::layout(title = "RMSE as function of gamma", 
                          yaxis = list(title = "RMSE"),
                          xaxis = list(title = "gamma"))
p_rmse

md2 <- HoltWinters(ts, alpha = alpha, beta = beta, gamma = 0.51)
fc2 <- forecast::forecast(md2, h = h)
TSstudio::plot_forecast(fc2)
