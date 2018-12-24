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


alpha1 <- seq(min(grid_top$alpha), max(grid_top$alpha), 0.05)
beta1 <- seq(min(grid_top$beta), max(grid_top$beta), 0.05)
gamma1 <- seq(min(grid_top$gamma), max(grid_top$gamma), 0.05)


grid_df2 <- base::expand.grid(alpha1, beta1, gamma1)
names(grid_df2) <- c("alpha", "beta", "gamma")
head(grid_df2)
tail(grid_df2)

score_df2 <- as.data.frame(matrix(NA, ncol = length(w), nrow = nrow(grid_df2)))
names(score_df2) <- paste0("period_", 1:length(w), sep = "")
grid_df3 <- cbind(grid_df2, score_df2)
head(grid_df3)
# Testing the sequance of gamma parameters over a window of 7 periods


for(n in 1:length(w)){
  ts_sub <- gamma_df <-  NULL
  ts_sub <- stats::window(USgas_train, 
                          start = stats::start(USgas_train), 
                          end = stats::time(USgas_train)[w[n]])
  partition <- TSstudio::ts_split(ts_sub, sample.out = h)
  train <- partition$train
  test <- partition$test
  for(i in 1:nrow(grid_df3)){
    md <- fc <- NULL
    md <- HoltWinters(train, alpha = grid_df3$alpha[i], 
                      beta = grid_df3$beta[i], 
                      gamma = grid_df3$gamma[i])
    fc <- forecast::forecast(md, h = h)
    grid_df3[i, n + 3]  <- forecast::accuracy(fc, test)[10]
    
  }
  print(n)
}

grid_df3$mean <- (grid_df3$period_1 + grid_df3$period_2  + grid_df3$period_3  + 
                   grid_df3$period_4 + grid_df3$period_5 + grid_df3$period_6) / 6

grid_df3 <- grid_df3 %>% dplyr::arrange(mean)
grid_top3 <- grid_df3[1:3 , ]


alpha2 <- seq(min(grid_top3$alpha), max(grid_top3$alpha), 0.01)
beta2 <- seq(min(grid_top3$beta), max(grid_top3$beta), 0.01)
gamma2 <- seq(min(grid_top3$gamma), max(grid_top3$gamma), 0.01)


grid_df4 <- base::expand.grid(alpha2, beta2, gamma2)
names(grid_df4) <- c("alpha", "beta", "gamma")
head(grid_df4)
tail(grid_df4)

score_df4 <- as.data.frame(matrix(NA, ncol = length(w), nrow = nrow(grid_df4)))
names(score_df4) <- paste0("period_", 1:length(w), sep = "")
grid_df5 <- cbind(grid_df4, score_df4)
head(grid_df5)
# Testing the sequance of gamma parameters over a window of 7 periods


for(n in 1:length(w)){
  ts_sub <- gamma_df <-  NULL
  ts_sub <- stats::window(USgas_train, 
                          start = stats::start(USgas_train), 
                          end = stats::time(USgas_train)[w[n]])
  partition <- TSstudio::ts_split(ts_sub, sample.out = h)
  train <- partition$train
  test <- partition$test
  for(i in 1:nrow(grid_df5)){
    md <- fc <- NULL
    md <- HoltWinters(train, alpha = grid_df5$alpha[i], 
                      beta = grid_df5$beta[i], 
                      gamma = grid_df5$gamma[i])
    fc <- forecast::forecast(md, h = h)
    grid_df5[i, n + 3]  <- forecast::accuracy(fc, test)[10]
    
  }
  print(n)
}

grid_df5$mean <- (grid_df5$period_1 + grid_df5$period_2  + grid_df5$period_3  + 
                    grid_df5$period_4 + grid_df5$period_5 + grid_df5$period_6) / 6



r <- which.min(grid_df5$mean)


md <- HoltWinters(USgas_train, alpha = grid_df5$alpha[r], 
                  beta = grid_df5$beta[r], 
                  gamma = grid_df5$gamma[r])
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
