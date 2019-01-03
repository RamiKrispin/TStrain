rm(list = ls())
library(TSstudio)
ts.obj <- USgas
periods <- 6
window_space <- 6
window_length <- 36
window_test <- 12
models <- "abehntwp"
window_type <- "both"


# Creating a grid data frame

models_map <- base::data.frame(model_abb = c("a","b", "e", "h", "n", "t", "w", "p"),
                               model = c("auto.arima", "bsts", "est", "hybridModel", 
                                         "nnetar", "tbats", "HoltWinters", "prophet"),
                               stringsAsFactors = FALSE)
  


models_map

periods_list <- base::paste0("period_", 1:periods)
partition_list <- list()
s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w_end <- seq(from = s, by = window_space, to = e)
w_end
periods_map <- base::data.frame(w_end = w_end,
                                period = 1:periods)

periods_map
w_start <- w_end - window_test - window_length + 1

create_par <- function(ts.obj, w_start, w_end){
par_list <- lapply(base::seq_along(w_end), function(i){
  ts_sub <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[w_start[i]],
                          end = stats::time(ts.obj)[w_end[i]])
  
  ts_partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
  train <- ts_partition$train
  test <- ts_partition$test
  
  return(base::list(ts_sub = ts_sub, train = train, test = test))
}) %>% stats::setNames(base::paste0("period_", 1:periods))

return(par_list)
}

par_setting <- base::list()

if(window_type == "both"){
  par_setting[["sliding"]] <- create_par(ts.obj = ts.obj, 
                                         w_start = w_end - window_test - window_length + 1, 
                                         w_end = w_end) 
  
  par_setting[["expanding"]] <- create_par(ts.obj = ts.obj, 
                                           w_start = base::rep(1, periods), 
                                           w_end = w_end)
  
} else if(window_type == "sliding"){
  par_setting[["sliding"]] <- create_par(ts.obj = ts.obj, 
                                         w_start = w_end - window_test - window_length + 1, 
                                         w_end = w_end) 
} else if(window_type == "expanding"){
  par_setting[["expanding"]] <- create_par(ts.obj = ts.obj, 
                                           w_start = base::rep(1, periods), 
                                           w_end = w_end) 
}

model_list <- base::strsplit(models, "") %>% base::unlist()
grid_df <- base::expand.grid(model_list, w_end, w_type, stringsAsFactors = FALSE)
names(grid_df) <- c("model_abb", "w_end", "w_type")
head(grid_df)
tail(grid_df)

grid_df$w_start <- ifelse(grid_df$w_type == "sliding",
                          grid_df$w_end - window_test - window_length + 1,
                          1)

grid_df <- grid_df %>% 
           dplyr::left_join(models_map) %>% 
           dplyr::left_join(periods_map)

head(grid_df)



backtesting_lapply <- base::lapply(1:base::nrow(grid_df), function(i){
  ts_sub <- train <- test <- ts_partition <- output <- NULL
  
  ts_sub <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[grid_df$w_start[i]],
                          end = stats::time(ts.obj)[grid_df$w_end[i]])
  
  ts_partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
  train <- ts_partition$train
  test <- ts_partition$test
  
  if(grid_df$model[i] == "a"){
    md <- fc <- RMSE <- MAPE <- NULL
    if("xreg" %in% names(a.arg)){
      a.xreg.train <- xreg.arima[1:length(train),]
      a.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_test),]
      a.arg.xreg <- a.arg
      a.arg.xreg$xreg <- a.xreg.train
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg.xreg))
      fc <- forecast::forecast(md, h = window_size, xreg = a.xreg.test)
    } else {
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
      fc <- forecast::forecast(md, h = window_size)
    }
    MAPE <-  base::round(forecast::accuracy(fc,test)[10], 2)
    RMSE <-  base::round(forecast::accuracy(fc,test)[4], 2)
    
    output <- base::list(model_name = "auto.arima",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model[i] == "b"){
    md <- fc <- ss <- RMSE <- MAPE <-  NULL
    ss <- list()
    if(b.arg$linear_trend){
      ss <- bsts::AddLocalLinearTrend(ss, ts.obj) 
    }
    if(b.arg$seasonal){
      ss <- bsts::AddSeasonal(ss, ts.obj, 
                              nseasons = stats::frequency(ts.obj))
    }
    
    md <- bsts::bsts(train, 
                     state.specification = ss, 
                     niter = b.arg$niter, 
                     ping= b.arg$ping, 
                     seed= b.arg$seed,
                     family = b.arg$family)
    
    fc <- stats::predict(md, horizon = window_test, quantiles = c(.025, .975))
    
    
    pred <- fc$mean
    MAPE <- base::round(mean(100 * base::abs((test - pred) / test)), 2)
    RMSE <- base::round((mean((test - pred)^ 2)) ^ 0.5, 2)
    
    output <- base::list(model_name = "bsts",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if("e" %in% model_char){
      md <- fc <- MAPE <- RMSE <- output <- NULL
      md <- base::do.call(forecast::ets, c(list(train), e.arg))
      fc <- forecast::forecast(train, h = window_size)
      MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
      
      output <- base::list(model_name = "ets",
                           model = md,
                           forecast = fc,
                           period = grid_df$period[i],
                           window_type = grid_df$w_type[i],
                           MAPE = MAPE,
                           RMSE = RMSE)
    } else if("h" %in% model_char){
      md <- fc <- MAPE <- RMSE <- output <-  NULL
      
      if("xreg" %in% names(h.arg$a.args) ||
         "xreg" %in% names(h.arg$n.args) ||
         "xreg" %in% names(h.arg$s.args)){
        h.arg.xreg <- h.test <-  NULL
        h.arg.xreg <- h.arg
        if("xreg" %in% names(h.arg$a.args)){
          h.arg.xreg$a.args$xreg <- xreg.hybrid.arima[1:length(train),]
          h.test <- xreg.hybrid.arima[(length(train) + 1):(length(train) + window_size),]
        }
        
        if("xreg" %in% names(h.arg$n.args)){
          h.arg.xreg$n.args$xreg <- xreg.hybrid.nnetar[1:length(train),]
          h.test <- xreg.hybrid.nnetar[(length(train) + 1):(length(train) + window_size),]
        }
        
        if("xreg" %in% names(h.arg$s.args)){
          h.arg.xreg$s.args$xreg <- xreg.hybrid.stlm[1:length(train),]
          h.test <- xreg.hybrid.stlm[(length(train) + 1):(length(train) + window_size),]
        }
        
        md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg.xreg))
        fc <- forecast::forecast(md, h = window_size, xreg = base::as.data.frame(h.test))
      } else {
        md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
        fc <- forecast::forecast(md, h = window_size)
      }
      
      MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
      RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
      
      output <- base::list(model_name = "hybridModel",
                           model = md,
                           forecast = fc,
                           period = grid_df$period[i],
                           window_type = grid_df$w_type[i],
                           MAPE = MAPE,
                           RMSE = RMSE)
    }
  
  
  output <- base::list()
  
})

length(backtesting_lapply)
backtesting_lapply[[1]]
