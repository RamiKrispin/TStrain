library(TSstudio)
data(USgas)
data(USVSales)
h <- 12
periods <- 3
window_size <- h
window_space <- h
ts_partition <- ts_split(USgas, sample.out = h)

valid <- ts_partition$test

ts.obj <- ts_partition$train
s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w <- seq(from = s, by = window_space, to = e)


alpha <- beta <- gamma <- seq(0, 1, 0.05)
alpha[1] <- 0.00001
beta[1] <- 0.00001
gamma[1] <- 0.00001
grid_df <- expand.grid(alpha, beta, gamma)
names(grid_df) <- c("alpha", "beta", "gamma")

temp <- base::data.frame(base::matrix(NA, nrow = base::nrow(grid_df), ncol = periods))
names(temp) <- base::paste0("period_", 1:periods)

mape_grid <- cbind(grid_df, temp)
mape_grid$valid <- NA

for(i in base::seq_along(w)){
  ts_sub <- train <- test <- c <- NULL
  ts_sub <- stats::window(ts.obj, start = stats::start(ts.obj), end = stats::time(ts.obj)[w[i]])
  train <- ts_split(ts_sub, sample.out = h)$train
  test <- ts_split(ts_sub, sample.out = h)$test
  c <- which(names(mape_grid) == base::paste("period_", i, sep = ""))
  for(l in 1:base::nrow(mape_grid)){
    md <- fc <- NULL
    md <- stats::HoltWinters(train, 
                             alpha = mape_grid$alpha[l],
                             beta = mape_grid$beta[l],
                             gamma = mape_grid$gamma[l])
    fc <- forecast::forecast(md, h)
    mape_grid[l, c] <- forecast::accuracy(fc,test)[10]
  }
  
  md1 <- stats::HoltWinters(train)
  fc1 <- forecast::forecast(md1, h)
  print(forecast::accuracy(fc1,test)[10])
  
}

for(l in 1:base::nrow(mape_grid)){
  md <- fc <- NULL
  md <- stats::HoltWinters(ts.obj, 
                           alpha = mape_grid$alpha[l],
                           beta = mape_grid$beta[l],
                           gamma = mape_grid$gamma[l])
  fc <- forecast::forecast(md, h)
  mape_grid$valid[l] <- forecast::accuracy(fc,valid)[10]
}


mape_grid$avg <- (mape_grid$period_1 + mape_grid$period_2 + mape_grid$period_3 ) / periods #+
                    # mape_grid$period_4 + mape_grid$period_5 + mape_grid$period_6) / periods


md2 <- stats::HoltWinters(ts.obj)
fc2 <- forecast::forecast(md2, h)
forecast::accuracy(fc2,valid)[10]

mape_grid <- mape_grid %>% dplyr::arrange(avg)
alpha_min <- min(mape_grid$alpha[1:5])
alpha_max <- max(mape_grid$alpha[1:5])
alpha <- seq(alpha_min, alpha_max, 0.01)

beta_min <- min(mape_grid$beta[1:5])
beta_max <- max(mape_grid$beta[1:5])
beta <- seq(beta_min, beta_max, 0.01)

gamma_min <- min(mape_grid$gamma[1:5])
gamma_max <- max(mape_grid$gamma[1:5])
gamma <- seq(gamma_min, gamma_max, 0.05)

grid_df2 <- expand.grid(alpha, beta, gamma)
names(grid_df2) <- c("alpha", "beta", "gamma")

temp <- base::data.frame(base::matrix(NA, nrow = base::nrow(grid_df2), ncol = periods))
names(temp) <- base::paste0("period_", 1:periods)

mape_grid2 <- cbind(grid_df2, temp)
mape_grid2$valid <- NA

for(i in base::seq_along(w)){
  ts_sub <- train <- test <- c <- NULL
  ts_sub <- stats::window(ts.obj, start = stats::start(ts.obj), end = stats::time(ts.obj)[w[i]])
  train <- ts_split(ts_sub, sample.out = h)$train
  test <- ts_split(ts_sub, sample.out = h)$test
  c <- which(names(mape_grid2) == base::paste("period_", i, sep = ""))
  for(l in 1:base::nrow(mape_grid2)){
    md <- fc <- NULL
    md <- stats::HoltWinters(train, 
                             alpha = mape_grid2$alpha[l],
                             beta = mape_grid2$beta[l],
                             gamma = mape_grid2$gamma[l])
    fc <- forecast::forecast(md, h)
    mape_grid2[l, c] <- forecast::accuracy(fc,test)[10]
  }
  
  md1 <- stats::HoltWinters(train)
  fc1 <- forecast::forecast(md1, h)
  print(forecast::accuracy(fc1,test)[10])
  
}

for(l in 1:base::nrow(mape_grid2)){
  md <- fc <- NULL
  md <- stats::HoltWinters(ts.obj, 
                           alpha = mape_grid2$alpha[l],
                           beta = mape_grid2$beta[l],
                           gamma = mape_grid2$gamma[l])
  fc <- forecast::forecast(md, h)
  mape_grid2$valid[l] <- forecast::accuracy(fc,valid)[10]
}


mape_grid2$avg <- (mape_grid2$period_1 + mape_grid2$period_2 + mape_grid2$period_3 +
                     mape_grid2$period_4 + mape_grid2$period_5 + mape_grid2$period_6) / periods


md2 <- stats::HoltWinters(ts.obj)
fc2 <- forecast::forecast(md2, h)
forecast::accuracy(fc2,valid)[10]
