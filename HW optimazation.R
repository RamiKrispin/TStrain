# Tunning the gamma parameter of Holt Winters model
?HoltWinters

# Using the USgas dataset as an example
library(TSstudio)
data(USgas)
data("USVSales")
USgas_split <- ts_split(AirPassengers, sample.out = h)

USgas_train <- USgas_split$train
USgas_test <- USgas_split$test

# Setting the window parameters
periods <- 6

window_size <- 12

window_space <- 3

h <- 12

# Setting a sequence for the gamma parameter
alpha <- seq(0, 1, 0.1)
beta <- seq(0, 1, 0.1)
gamma <- seq(0, 1, 0.1)
alpha[1] <- alpha[1] + 0.0001
beta[1] <- beta[1] + 0.0001
gamma[1] <- gamma[1] + 0.0001

grid_df_init_a <- base::expand.grid(alpha, beta, gamma)
names(grid_df_init_a) <- c("alpha", "beta", "gamma")
head(grid_df_init_a)
tail(grid_df_init_a)

s <- length(USgas_train) - window_space * (periods - 1)
e <- length(USgas_train) 
w <- seq(from = s, by = window_space, to = e)
score_df <- as.data.frame(matrix(NA, 
                                 ncol = length(w), 
                                 nrow = nrow(grid_df_init_a)))

names(score_df) <- paste0("period_", 1:length(w), sep = "")
grid_df_init <- cbind(grid_df_init_a, score_df)
head(grid_df_init)
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
      md <- HoltWinters(train, alpha = grid_df_init$alpha[i], 
                        beta = grid_df_init$beta[i], 
                        gamma = grid_df_init$gamma[i])
      fc <- forecast::forecast(md, h = h)
      grid_df_init[i, n + 3]  <- forecast::accuracy(fc, test)[10]
      
    }
    print(n)
  }

grid_df_init$mean <- (grid_df_init$period_1 + grid_df_init$period_2  + grid_df_init$period_3  + 
                        grid_df_init$period_4 + grid_df_init$period_5 + grid_df_init$period_6) / 6


grid_df_init <- grid_df_init %>% dplyr::arrange(mean)
grid_init_top <- grid_df_init[1:5 , ]


alpha1 <- seq(min(grid_init_top$alpha), max(grid_init_top$alpha), 0.01)
beta1 <- seq(min(grid_init_top$beta), max(grid_init_top$beta), 0.01)
gamma1 <- seq(min(grid_init_top$gamma), max(grid_init_top$gamma), 0.01)


grid_df_second_a <- base::expand.grid(alpha1, beta1, gamma1)
names(grid_df_second_a) <- c("alpha", "beta", "gamma")
head(grid_df_second_a)
tail(grid_df_second_a)

score_df_second <- as.data.frame(matrix(NA, 
                                        ncol = length(w), 
                                        nrow = nrow(grid_df_second_a)))
names(score_df_second) <- paste0("period_", 1:length(w), sep = "")
grid_df_second <- cbind(grid_df_second_a, score_df_second)
head(grid_df_second)
# Testing the sequance of gamma parameters over a window of 7 periods
for(n in 1:length(w)){
  ts_sub <-  NULL
  ts_sub <- stats::window(USgas_train, 
                          start = stats::start(USgas_train), 
                          end = stats::time(USgas_train)[w[n]])
  partition <- TSstudio::ts_split(ts_sub, sample.out = h)
  train <- partition$train
  test <- partition$test
  for(i in 1:nrow(grid_df_second)){
    md <- fc <- NULL
    md <- HoltWinters(train, alpha = grid_df_second$alpha[i], 
                      beta = grid_df_second$beta[i], 
                      gamma = grid_df_second$gamma[i])
    fc <- forecast::forecast(md, h = h)
    grid_df_second[i, n + 3]  <- forecast::accuracy(fc, test)[10]
    
  }
  print(n)
}

grid_df_second$mean <- (grid_df_second$period_1 + grid_df_second$period_2  + grid_df_second$period_3  + 
                   grid_df_second$period_4 + grid_df_second$period_5 + grid_df_second$period_6) / 6

grid_df_second <- grid_df_second %>% dplyr::arrange(mean)



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



r <- which.min(grid_df_second$mean)


md <- HoltWinters(USgas_train, alpha = grid_df_second$alpha[r], 
                  beta = grid_df_second$beta[r], 
                  gamma = grid_df_second$gamma[r])
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

periods <- 6 # Number of testing periods
window_space <- 6 # Space between each testing partition
window_length <- 36 # Length of training window for sliding backtesting
window_test <- 12 # Length of the testing partition
models <- "abehntwp"  # Type of models
window_type <- "both" # Type of backtesting window 




ts_grid <- function(ts.obj, 
                    model, 
                    periods,
                    window_length = NULL, 
                    window_space,
                    window_test,
                    hyper_params,
                    search_criteria, 
                    parallel = TRUE,
                    n.cores = "auto"){
 
  # Error handling
  if(!stats::is.ts(ts.obj)){
    stop("The input object is not 'ts' object")
  } else if(stats::is.mts(ts.obj)){
    stop("The input object is 'mts' object, please use 'ts'")
  }
  
  if(!base::is.logical(parallel)){
    warning("The 'parallel' argument is not a boolean operator, setting it to TRUE")
    parallel <- TRUE
  }
  
  if(n.cores != "auto"){
    if(!base::is.numeric(n.cores)){
      warning("The value of the 'n.cores' argument is not valid,", 
              " setting it to 'auto' mode")
      n.cores <- "auto"
    } else if(base::is.numeric(n.cores) && 
              (n.cores %% 1 != 0 || n.cores < 1)){
      warning("The value of the 'n.cores' argument is not valid,", 
              " setting it to 'auto' mode")
      n.cores <- "auto"
    } else{
      if(future::availableCores() < n.cores){
        warning("The value of the 'n.cores' argument is not valid,", 
                "(the requested number of cores are greater than available)",
                ", setting it to 'auto' mode")
        n.cores <- "auto"
      }
    }
  }
  
  if(n.cores == "auto"){
    n.cores <- base::as.numeric( future::availableCores() - 1)
  }
  if(!model %in% c("HoltWinters")){
    stop("The 'model' argument is not valid")
  }
  
  # Set the backtesting partitions
  s <- length(ts.obj) - window_space * (periods - 1) # the length of the first partition
  e <- length(ts.obj)  # the end of the backtesting partition
  w_end <- seq(from = s, by = window_space, to = e) # Set the cutting points for the backtesting partions
  
  if(!base::is.null(window_length)){
    w_start <- w_end - window_test - window_length + 1
  } else {
    w_start <- base::rep(1, base::length(w_end))
  }
  
  
  if(model == "HoltWinters"){
    hw_par <- c("alpha", "beta", "gamma")
    if(!base::all(hyper_params %in% hw_par)){
      stop("The 'hyper_params' argument is invalid")
    }
    if("alpha" %in% base::names(hyper_params)){
      if(base::any(which(hyper_params$alpha < 0)) || 
         base::any(which(hyper_params$alpha > 1))){
        stop("The value of the 'alpha' parameter is out of range,",
             " cannot exceed 1 or be less or equal to 0")
      } else if(any(which(hyper_params$alpha == 0))){
        hyper_params$alpha[base::which(hyper_params$alpha == 0)] <- 1e-5
        warning("The value of the 'alpha' parameter cannot be equal to 0",
                " replacing 0 with 1e-5")
      }
      alpha <- NULL
      alpha <- hyper_params$alpha
      
    } else {
      alpha <- NULL
    }
    
    if("beta" %in% base::names(hyper_params)){
      if(base::any(which(hyper_params$beta < 0)) || 
         base::any(which(hyper_params$beta > 1))){
        stop("The value of the 'beta' parameter is out of range,",
             " cannot exceed 1 or be less or equal to 0")
      } else if(any(which(hyper_params$beta == 0))){
        hyper_params$beta[base::which(hyper_params$beta == 0)] <- 1e-5
        warning("The value of the 'beta' parameter cannot be equal to 0",
                " replacing 0 with 1e-5")
      }
      beta <- NULL
      beta <- hyper_params$beta
      
    } else {
      beta <- NULL
    }
    
    if("gamma" %in% base::names(hyper_params)){
      if(base::any(which(hyper_params$gamma < 0)) || 
         base::any(which(hyper_params$gamma > 1))){
        stop("The value of the 'gamma' parameter is out of range,",
             " cannot exceed 1 or be less or equal to 0")
      } else if(any(which(hyper_params$gamma == 0))){
        hyper_params$alpha[base::which(hyper_params$gamma == 0)] <- 1e-5
        warning("The value of the 'gamma' parameter cannot be equal to 0",
                " replacing 0 with 1e-5")
      }
      gamma <- NULL
      gamma <- hyper_params$gamma
      
    } else {
      gamma <- NULL
    }
    
    grid_df <- base::eval(
      base::parse(text = base::paste("base::expand.grid(", 
                                     base::paste(base::names(hyper_params), 
                                                 collapse = ", "),
                                     ")", 
                                     sep = "")))
    base::names(grid_df) <- c(base::names(hyper_params))
    
   grid_model <- base::paste("stats::HoltWinters(x = train", sep = "")
  for(i in hw_par){
    if(i %in% base::names(grid_df)){
      grid_model <- base::paste(grid_model, ", ", i, " = search_df$", i, "[i]", 
                              sep = "" )
    } else {
      grid_model <- base::paste(grid_model, ", ", i, " = NULL", sep = "")
    }
  }
   grid_model <- base::paste(grid_model, ")", sep = "")
   }
  

  
  
grid_output <- NULL

start_time <- Sys.time()
grid_output <- base::lapply(1:periods, function(n){
  ts_sub <- train <- test <- search_df <- NULL
  
  search_df <- grid_df
  search_df$period <- n
  search_df$mape <- NA
  ts_sub <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[w_start[n]], 
                          end = stats::time(ts.obj)[w_end[n]])
  partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
  train <- partition$train
  test <- partition$test
  
  for(i in 1:nrow(search_df)){
    md <- fc <- NULL
    md <- base::eval(base::parse(text = grid_model))
    fc <- forecast::forecast(md, h = window_test)
    search_df$mape[i] <- forecast::accuracy(fc, test)[10]
  }
  
  return(search_df)
  }) %>% 
  dplyr::bind_rows()
end <- Sys.time() - start_time
end

}



