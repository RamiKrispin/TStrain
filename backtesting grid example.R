rm(list = ls())
library(TSstudio)
data("USgas")

ts.obj <- USgas
models = "abehntw"
models = "benw"
periods = 6
window_space = 6
window_test = 12
window_length = 48
window_type = "both"
error = "MAPE"
top = NULL
h = 60
plot = TRUE
a.arg = NULL
b.arg = NULL
e.arg = NULL
h.arg = NULL
n.arg = NULL
t.arg = NULL
w.arg = NULL
xreg.h = NULL
parallel = TRUE


ts_test <- function(ts.obj, 
                           models = "abehntw", 
                           periods = 6, 
                           window_space = 6,
                           window_test = 12,
                           window_length = NULL,
                           window_type = "expending",
                           error = "MAPE", 
                           #window_size = 3,
                           h = 3,
                           plot = TRUE,
                           top = NULL,
                           a.arg = NULL,
                           b.arg = NULL,
                           e.arg = NULL,
                           h.arg = NULL,
                           n.arg = NULL,
                           t.arg = NULL,
                           w.arg = NULL,
                           xreg.h = NULL,
                           parallel = TRUE,
                           n_cores = future::availableCores() - 1,
                           palette = "BrBG"){

`%>%` <- magrittr::`%>%` 

a <- model_list <- model_char <- color_ramp <- forecast_list <- obj.name <- NULL
variable <- value <- avgMAPE <- avgRMSE <- NULL
obj.name <- base::deparse(base::substitute(ts.obj))


# Define the model type
for(s in 1:nchar(models)){
  if(!substr(models, s, s) %in% c("a", "w", "e", "n", "t", "b", "h")){
    stop("The 'models' argument is not valide")
  }
}

# Error handling
# Check if xreg argument is valid
if(!base::is.null(xreg.h)){
  if(!"xreg" %in% names(a.arg) &
     !"xreg" %in% names(n.arg) &
     !"xreg" %in% names(h.arg$a.args) &
     !"xreg" %in% names(h.arg$n.args) &
     !"xreg" %in% names(h.arg$s.args)){
    warning("There is no 'xreg' argument in any of the models arguments,", 
            "'xreg.h' will be ignored")
  } else {
    if(base::nrow(xreg.h) != h){
      stop("The length of the 'xreg.h' argument is not equal to the forecast horizon")
    }
  }
}

# Check the xreg in a.arg is valid (if exists)
if("xreg" %in% names(a.arg)){
  xreg.arima <- NULL
  xreg.arima <- a.arg$xreg
  if(base::nrow(xreg.arima) != base::length(ts.obj)){
    stop("The length of the 'xreg' in the 'a.arg' argument is not equal to the series length")
  }
}  

if("xreg" %in% names(n.arg)){
  xreg.nnetar <- NULL
  xreg.nnetar <- n.arg$xreg
  if(base::nrow(xreg.nnetar) != base::length(ts.obj)){
    stop("The length of the 'xreg' in the 'n.arg' argument is not equal to the series length")
  }
}  

if("xreg" %in% names(h.arg$a.args)){
  xreg.hybrid.arima <- NULL
  xreg.hybrid.arima <- h.arg$a.args$xreg
  if(base::nrow(xreg.hybrid.arima) != base::length(ts.obj)){
    stop("The length of the 'xreg' of the auto.arima model in the 'h.arg' argument is not equal to the series length")
  }
}  

if("xreg" %in% names(h.arg$n.args)){
  xreg.hybrid.nnetar <- NULL
  xreg.hybrid.nnetar <- h.arg$n.args$xreg
  if(base::nrow(xreg.hybrid.nnetar) != base::length(ts.obj)){
    stop("The length of the 'xreg' of the nnetar model in the 'h.arg' argument is not equal to the series length")
  }
}  

if("xreg" %in% names(h.arg$s.args)){
  xreg.hybrid.stlm <- NULL
  xreg.hybrid.stlm <- h.arg$s.args$xreg
  if(base::nrow(xreg.hybrid.stlm) != base::length(ts.obj)){
    stop("The length of the 'xreg' of the stlm model in the 'h.arg' argument is not equal to the series length")
  }
}  

if(!base::is.numeric(periods) | periods != base::round(periods) | periods <= 0){
  stop("The value of the 'periods' parameters is no valid")
} 

if(!base::is.numeric(window_test) | window_test != base::round(window_test) | window_test <= 0){
  stop("The value of the 'window_test' parameters is no valid")
} else {
  if((base::length(ts.obj) - window_space * (periods - 1) - window_test) < 2 * stats::frequency(ts.obj)){
    stop("The length of the series is long enough to create a forecast")
  }
}

if (stats::is.ts(ts.obj)) {
  if (stats::is.mts(ts.obj)) {
    warning("The 'ts.obj' has multiple columns, only the first column will be plot")
    ts.obj <- ts.obj[, 1]
  }
}else {
  stop("The 'ts.obj' is not a 'ts' class")
}

if(!error %in% c("MAPE", "RMSE")){
  warning("The value of the 'error' parameter is invalid, using the default setting - 'MAPE'")
  error <- "MAPE"
} 

if(!base::is.logical(plot)){
  warning("The value of the 'plot' parameter is invalid, using default option TRUE")
  plot <- TRUE
}

# Checking the palette argument
brewer_palettes <- row.names(RColorBrewer::brewer.pal.info)
viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")

if(!palette %in% c(brewer_palettes, viridis_palettes)){
  warning("The 'palette' argument is not valid, using default option ('BrBG'")
  palette <- "BrBG"
}


# Setting the bsts arguments
if(is.null(b.arg)){
  b.arg <-  list(linear_trend = TRUE,
                 seasonal = TRUE,
                 niter = 1000,
                 ping = 0,
                 family = "gaussian",
                 seed=1234)
} else{
  
  if("linear_trend" %in% names(b.arg)){
    if(!b.arg$linear_trend %in% c(TRUE, FALSE)){
      warning("The value of the 'linear_trend' argument of the bsts model is invalid, using default (TRUE)")
      b.arg$linear_trend <- TRUE
    }
  } else {
    warning("The 'linear_trend' was not defined, using TRUE as default")
    b.arg$linear_trend <- TRUE
  }
  
  if("seasonal" %in% names(b.arg)){
    if(!b.arg$seasonal %in% c(TRUE, FALSE)){
      warning("The value of the 'seasonal' argument of the bsts model is invalid, using TRUE as default")
      b.arg$seasonal <- TRUE 
    } 
  } else {
    warning("The 'seasonal' argument was not defined, using TRUE as default")
    b.arg$seasonal <- TRUE
  }
  
  if("niter" %in% names(b.arg)){
    if(!base::is.numeric(b.arg$niter)){
      warning("The value of the 'niter' argument of the bsts model is invalid, setting the argument to 1000")
      b.arg$niter <- 1000 
    } else if(b.arg$niter %% 1 != 0){
      warning("The value of the 'niter' argument of the bsts model is not integer, setting the argument to 1000")
      b.arg$niter <- 1000 
    }
  } else {
    warning("The 'niter' argument was not defined, setting the argument to 1000")
    b.arg$niter <- 1000
  }
  
  if("ping" %in% names(b.arg)){
    if(!base::is.numeric(b.arg$ping)){
      warning("The value of the 'ping' argument of the bsts model is invalid, setting the argument to 100")
      b.arg$ping <- 100 
    } else if(b.arg$ping %% 1 != 0){
      warning("The value of the 'ping' argument of the bsts model is not integer, setting the argument to 100")
      b.arg$ping <- 1000 
    }
  } else {
    warning("The 'ping' argument was not defined, setting the argument to 100")
    b.arg$ping <- 100
  }
  
  if("seed" %in% names(b.arg)){
    if(!base::is.numeric(b.arg$seed)){
      warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
      b.arg$seed <- 1234 
    } else if(b.arg$seed %% 1 != 0){
      warning("The value of the 'seed' argument of the bsts model is not integer, setting the argument to 1234")
      b.arg$seed <- 1234 
    }
  } else {
    warning("The 'seed' argument was not defined, setting the argument to 1234")
    b.arg$seed <- 1234
  }
  
  
  if("family" %in% names(b.arg)){
    if(!b.arg$family %in% c("gaussian", "logit", "poisson", "student")){
      warning("The value of the 'family' argument of the bsts model is invalid, using 'gaussian' as default")
      b.arg$family <- "gaussian"
    }
  } else{
    warning("The value of the 'family' argument is missing, using 'gaussian' as default")
    b.arg$family <- "gaussian"
  }
  
  
}

if(!base::is.character(window_type)){
  warning("The value of the 'window_type' argument is not valid, setting it to 'expending'")
  window_type <- "expending"
} else if(!window_type %in% c("both", "sliding", "expending")){
  warning("The value of the 'window_type' argument is not valid, setting it to 'expending'")
  window_type <- "expending"
}

if(window_type == "both"){
  w_type <- c("sliding", "expending")
} else{
  w_type <- window_type
}

if(base::is.null(top)){
  top <-  base::nchar(models) * length(w_type)
} else if(!base::is.null(top)){
  if(!base::is.numeric(top)){
    warning("The 'top' argument is not valid, will show all models")
    top <- base::nchar(models) * length(w_type)
  } else if(top %% 1 != 0 || top < 1){
    warning("The 'top' argument is not valid, will show all models")
    top <- base::nchar(models) * length(w_type)
  }
}


#### Testing ####


models_map <- base::data.frame(model_abb = c("a", "b", "e", "h", "n", "t", "w"),
                               model = c("auto.arima", "bsts", "est", "hybridModel", 
                                         "nnetar", "tbats", "HoltWinters"),
                               stringsAsFactors = FALSE)


s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w_end <- seq(from = s, by = window_space, to = e)

if(window_type == "both" | window_type == "sliding"){
  if(base::is.null(window_length)){
    stop("The 'window_length' argument is not set")
  } else if(!base::is.numeric(window_length)){
    stop("The 'window_length' argument is not set")
  } else if(window_length > s){
    stop("The 'window_legnth' argument is not proportional to the length of the series")
  }
}



periods_map <- base::data.frame(w_end = w_end,
                                period = 1:periods)

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


model_output <- base::list()
periods_list <- paste("period_", 1:periods, sep ="")

# Defining the window type
for(i in w_type){
  periods_map1 <- NULL
  model_output[[i]] <- base::list()
  periods_map1 <- periods_map
  if(i == "sliding"){
    periods_map1$window_test <- window_test
    periods_map1$window_length <- window_length
    periods_map1$w_start <- periods_map1$w_end - periods_map1$window_length - periods_map1$window_test + 1
  } else if(i == "expending"){
    periods_map1$w_start <- 1
  }
  for(l in base::seq_along(periods_list)){
    ts_sub <- train <- test <- NULL
    
    ts_sub <- stats::window(ts.obj, 
                            start = stats::time(ts.obj)[periods_map1$w_start[l]],
                            end = stats::time(ts.obj)[periods_map1$w_end[l]])
    
    ts_partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
    train <- ts_partition$train
    test <- ts_partition$test
    
    model_output[[i]][[periods_list[l]]] <- base::list(train = train, test = test)
  }
}


if(parallel){
future::plan(future::multiprocess, workers = n_cores) 
backtesting_train <- future.apply::future_lapply(1:base::nrow(grid_df), function(i){
  train <- test <- ts_partition <- output <- NULL
  
  train <- model_output[[grid_df$w_type[i]]][[paste("period_", grid_df$period[i], sep = "")]]$train
  test <- model_output[[grid_df$w_type[i]]][[paste("period_", grid_df$period[i], sep = "")]]$test
  
  
  if(grid_df$model_abb[i] == "a"){
    md <- fc <- RMSE <- MAPE <- NULL
    if("xreg" %in% names(a.arg)){
      a.xreg.train <- xreg.arima[1:length(train),]
      a.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_test),]
      a.arg.xreg <- a.arg
      a.arg.xreg$xreg <- a.xreg.train
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = a.xreg.test)
    } else {
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
      fc <- forecast::forecast(md, h = window_test)
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
  } else if(grid_df$model_abb[i] == "b"){
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
  } else if(grid_df$model_abb[i] == "e"){
    md <- fc <- MAPE <- RMSE <- output <- NULL
    md <- base::do.call(forecast::ets, c(list(train), e.arg))
    fc <- forecast::forecast(train, h = window_test)
    MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
    
    output <- base::list(model_name = "ets",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "h"){
    md <- fc <- MAPE <- RMSE <- output <-  NULL
    
    if("xreg" %in% names(h.arg$a.args) ||
       "xreg" %in% names(h.arg$n.args) ||
       "xreg" %in% names(h.arg$s.args)){
      h.arg.xreg <- h.test <-  NULL
      h.arg.xreg <- h.arg
      if("xreg" %in% names(h.arg$a.args)){
        h.arg.xreg$a.args$xreg <- xreg.hybrid.arima[1:length(train),]
        h.test <- xreg.hybrid.arima[(length(train) + 1):(length(train) + window_test),]
      }
      
      if("xreg" %in% names(h.arg$n.args)){
        h.arg.xreg$n.args$xreg <- xreg.hybrid.nnetar[1:length(train),]
        h.test <- xreg.hybrid.nnetar[(length(train) + 1):(length(train) + window_test),]
      }
      
      if("xreg" %in% names(h.arg$s.args)){
        h.arg.xreg$s.args$xreg <- xreg.hybrid.stlm[1:length(train),]
        h.test <- xreg.hybrid.stlm[(length(train) + 1):(length(train) + window_test),]
      }
      
      md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = base::as.data.frame(h.test))
    } else {
      md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
      fc <- forecast::forecast(md, h = window_test)
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
  } else if(grid_df$model_abb[i] == "n"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    if("xreg" %in% names(n.arg)){
      n.xreg.train <- xreg.arima[1:length(train),]
      n.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_test),]
      n.arg.xreg <- n.arg
      n.arg.xreg$xreg <- n.xreg.train
      md <- base::do.call(forecast::nnetar, c(list(train), n.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = n.xreg.test)
    } else {
      md <- base::do.call(forecast::nnetar, c(list(train), n.arg))
      fc <- forecast::forecast(md, h = window_test)
    }
    
    
    MAPE <-  base::round(forecast::accuracy(fc, test)[10],2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4],2)
    output <- base::list(model_name = "nnetar",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "t"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    md <- base::do.call(forecast::tbats, c(list(train), t.arg))
    fc <- forecast::forecast(md, h = window_test)
    MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
    output <- base::list(model_name = "tbats",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "w"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    md <- base::do.call(stats::HoltWinters, c(list(train), w.arg))
    fc <- forecast::forecast(md, h = window_test)
    MAPE <- base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <- base::round(forecast::accuracy(fc, test)[4], 2)
    output <- base::list(model_name = "HoltWinters",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  }
  
  
  return(output)
  
})  

grid_forecast_df <- base::expand.grid(model_list, w_type, stringsAsFactors = FALSE)

names(grid_forecast_df) <- c("model_abb",  "w_type")

grid_forecast_df$w_start <- ifelse(grid_forecast_df$w_type == "sliding",
                                   base::length(ts.obj) -  window_length + 1,
                                   1)

grid_forecast_df <- grid_forecast_df %>% dplyr::left_join(models_map)
grid_forecast_df$model_name <- base::paste(grid_forecast_df$model, 
                                           "_",
                                           base::substr(grid_forecast_df$w_type,1,1),
                                           sep = "")

final_forecast <- future.apply::future_lapply(1:base::nrow(grid_forecast_df), 
                                              function(i){
  output <- train <- NULL
  output <- base::list()
  
  # Setting the training partition
  if(grid_forecast_df$w_type[i] == "expending"){
    train <- ts.obj
  } else if(grid_forecast_df$w_type[i] == "sliding"){
    train <- ts_sub <- stats::window(ts.obj, 
                                     start = stats::time(ts.obj)[grid_forecast_df$w_start[i]])
  }
  
  
  if(grid_forecast_df$model_abb[i] == "a"){
    md <- fc <- NULL
    a.arg$parallel <- FALSE # Disabling the parallel option when running future
    md <- base::do.call(forecast::auto.arima, 
                        c(base::list(train), 
                          a.arg))
    
    if("xreg" %in% base::names(a.arg)){
      fc <- forecast::forecast(md, h = h, xreg = xreg.h)
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "w"){
    md <- fc <- NULL
    md <- base::do.call(stats::HoltWinters, c(base::list(train), w.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "e"){
    md <- fc <- NULL
    md <- base::do.call(forecast::ets, c(base::list(train), e.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "n"){
    md <- fc <- NULL
    md <- base::do.call(forecast::nnetar, c(base::list(train), n.arg))
    if("xreg" %in% base::names(n.arg)){
      fc <- forecast::forecast(md, h = h, xreg = xreg.h)
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    output <- base::list(model = md, forecast = fc)
  } else if(grid_forecast_df$model_abb[i] == "t"){
    md <- fc <- NULL
    t.arg$use.parallel <- FALSE # Disabling the parallel option when running future
    md <- base::do.call(forecast::tbats, c(list(train), t.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  }else if(grid_forecast_df$model_abb[i] == "b"){
    
    # Check if the bsts arguments are valid
    if(is.null(b.arg)){
      b.arg <-  list(linear_trend = TRUE,
                     seasonal = TRUE,
                     niter = 1000,
                     ping = 0,
                     family = "gaussian",
                     seed=1234)
    } else{
      
      if("linear_trend" %in% names(b.arg)){
        if(!b.arg$linear_trend %in% c(TRUE, FALSE)){
          warning("The value of the 'linear_trend' argument of the bsts model is invalid, using default (TRUE)")
          b.arg$linear_trend <- TRUE
        }
      } else {
        warning("The 'linear_trend' was not defined, using TRUE as default")
        b.arg$linear_trend <- TRUE
      }
      
      if("seasonal" %in% names(b.arg)){
        if(!b.arg$seasonal %in% c(TRUE, FALSE)){
          warning("The value of the 'seasonal' argument of the bsts model is invalid, using TRUE as default")
          b.arg$seasonal <- TRUE 
        } 
      } else {
        warning("The 'seasonal' argument was not defined, using TRUE as default")
        b.arg$seasonal <- TRUE
      }
      
      if("niter" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$niter)){
          warning("The value of the 'niter' argument of the bsts model is invalid, setting the argument to 1000")
          b.arg$niter <- 1000 
        } else if(b.arg$niter %% 1 != 0){
          warning("The value of the 'niter' argument of the bsts model is not integer, setting the argument to 1000")
          b.arg$niter <- 1000 
        }
      } else {
        warning("The 'niter' argument was not defined, setting the argument to 1000")
        b.arg$niter <- 1000
      }
      
      if("ping" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$ping)){
          warning("The value of the 'ping' argument of the bsts model is invalid, setting the argument to 100")
          b.arg$ping <- 100 
        } else if(b.arg$ping %% 1 != 0){
          warning("The value of the 'ping' argument of the bsts model is not integer, setting the argument to 100")
          b.arg$ping <- 1000 
        }
      } else {
        warning("The 'ping' argument was not defined, setting the argument to 100")
        b.arg$ping <- 100
      }
      
      if("seed" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$seed)){
          warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
          b.arg$seed <- 1234 
        } else if(b.arg$seed %% 1 != 0){
          warning("The value of the 'seed' argument of the bsts model is not integer, setting the argument to 1234")
          b.arg$seed <- 1234 
        }
      } else {
        warning("The 'seed' argument was not defined, setting the argument to 1234")
        b.arg$seed <- 1234
      }
      
      
      if("family" %in% names(b.arg)){
        if(!b.arg$family %in% c("gaussian", "logit", "poisson", "student")){
          warning("The value of the 'family' argument of the bsts model is invalid, using 'gaussian' as default")
          b.arg$family <- "gaussian"
        }
      } else{
        warning("The value of the 'family' argument is missing, using 'gaussian' as default")
        b.arg$family <- "gaussian"
      }
      
      
    }
    
    md <- fc <- ss <- fit.bsts <- burn <-  NULL
    ss <- list()
    if(b.arg$linear_trend){
      ss <- bsts::AddLocalLinearTrend(ss, train) 
    }
    if(b.arg$seasonal){
      ss <- bsts::AddSeasonal(ss, train, 
                              nseasons = stats::frequency(train))
    }
    
    md <- bsts::bsts(train, 
                     state.specification = ss, 
                     niter = b.arg$niter, 
                     ping= b.arg$ping, 
                     seed= b.arg$seed,
                     family = b.arg$family)
    fc <- stats::predict(md, horizon = h, quantiles = c(.025, .975))
    output <- base::list(model = md, forecast = fc)
  }
  
  if(grid_forecast_df$model_abb[i] == "h"){
    md <- fc <- NULL
    h.arg$parallel <- FALSE # Disabling the parallel option when running future
    md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
    
    
    if("xreg" %in% names(h.arg$a.args) ||
       "xreg" %in% names(h.arg$n.args) ||
       "xreg" %in% names(h.arg$s.args)){
      fc <- forecast::forecast(md, h = h, xreg = base::as.data.frame(xreg.h))
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    
    output <- base::list(model = md, forecast = fc)
  }
  
  
  return(output)
  
}) %>%
  stats::setNames(grid_forecast_df$model_name)


} else {
backtesting_train <- base::lapply(1:base::nrow(grid_df), function(i){
  train <- test <- ts_partition <- output <- NULL
  
  train <- model_output[[grid_df$w_type[i]]][[paste("period_", grid_df$period[i], sep = "")]]$train
  test <- model_output[[grid_df$w_type[i]]][[paste("period_", grid_df$period[i], sep = "")]]$test

  
  if(grid_df$model_abb[i] == "a"){
    md <- fc <- RMSE <- MAPE <- NULL
    if("xreg" %in% names(a.arg)){
      a.xreg.train <- xreg.arima[1:length(train),]
      a.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_test),]
      a.arg.xreg <- a.arg
      a.arg.xreg$xreg <- a.xreg.train
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = a.xreg.test)
    } else {
      md <- base::do.call(forecast::auto.arima, c(list(train), a.arg))
      fc <- forecast::forecast(md, h = window_test)
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
  } else if(grid_df$model_abb[i] == "b"){
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
  } else if(grid_df$model_abb[i] == "e"){
    md <- fc <- MAPE <- RMSE <- output <- NULL
    md <- base::do.call(forecast::ets, c(list(train), e.arg))
    fc <- forecast::forecast(train, h = window_test)
    MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
    
    output <- base::list(model_name = "ets",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "h"){
    md <- fc <- MAPE <- RMSE <- output <-  NULL
    
    if("xreg" %in% names(h.arg$a.args) ||
       "xreg" %in% names(h.arg$n.args) ||
       "xreg" %in% names(h.arg$s.args)){
      h.arg.xreg <- h.test <-  NULL
      h.arg.xreg <- h.arg
      if("xreg" %in% names(h.arg$a.args)){
        h.arg.xreg$a.args$xreg <- xreg.hybrid.arima[1:length(train),]
        h.test <- xreg.hybrid.arima[(length(train) + 1):(length(train) + window_test),]
      }
      
      if("xreg" %in% names(h.arg$n.args)){
        h.arg.xreg$n.args$xreg <- xreg.hybrid.nnetar[1:length(train),]
        h.test <- xreg.hybrid.nnetar[(length(train) + 1):(length(train) + window_test),]
      }
      
      if("xreg" %in% names(h.arg$s.args)){
        h.arg.xreg$s.args$xreg <- xreg.hybrid.stlm[1:length(train),]
        h.test <- xreg.hybrid.stlm[(length(train) + 1):(length(train) + window_test),]
      }
      
      md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = base::as.data.frame(h.test))
    } else {
      md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
      fc <- forecast::forecast(md, h = window_test)
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
  } else if(grid_df$model_abb[i] == "n"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    if("xreg" %in% names(n.arg)){
      n.xreg.train <- xreg.arima[1:length(train),]
      n.xreg.test <- xreg.arima[(length(train) + 1):(length(train) + window_test),]
      n.arg.xreg <- n.arg
      n.arg.xreg$xreg <- n.xreg.train
      md <- base::do.call(forecast::nnetar, c(list(train), n.arg.xreg))
      fc <- forecast::forecast(md, h = window_test, xreg = n.xreg.test)
    } else {
      md <- base::do.call(forecast::nnetar, c(list(train), n.arg))
      fc <- forecast::forecast(md, h = window_test)
    }
    
    
    MAPE <-  base::round(forecast::accuracy(fc, test)[10],2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4],2)
    output <- base::list(model_name = "nnetar",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "t"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    md <- base::do.call(forecast::tbats, c(list(train), t.arg))
    fc <- forecast::forecast(md, h = window_test)
    MAPE <-  base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <-  base::round(forecast::accuracy(fc, test)[4], 2)
    output <- base::list(model_name = "tbats",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  } else if(grid_df$model_abb[i] == "w"){
    md <- fc <- RMSE <- MAPE <- output <- NULL
    md <- base::do.call(stats::HoltWinters, c(list(train), w.arg))
    fc <- forecast::forecast(md, h = window_test)
    MAPE <- base::round(forecast::accuracy(fc, test)[10], 2)
    RMSE <- base::round(forecast::accuracy(fc, test)[4], 2)
    output <- base::list(model_name = "HoltWinters",
                         model = md,
                         forecast = fc,
                         period = grid_df$period[i],
                         window_type = grid_df$w_type[i],
                         MAPE = MAPE,
                         RMSE = RMSE)
  }
  
  
  return(output)
  
})  

grid_forecast_df <- base::expand.grid(model_list, w_type, stringsAsFactors = FALSE)

names(grid_forecast_df) <- c("model_abb",  "w_type")

grid_forecast_df$w_start <- ifelse(grid_forecast_df$w_type == "sliding",
                                   base::length(ts.obj) -  window_length + 1,
                                   1)

grid_forecast_df <- grid_forecast_df %>% dplyr::left_join(models_map)
grid_forecast_df$model_name <- base::paste(grid_forecast_df$model, 
                                           "_",
                                           base::substr(grid_forecast_df$w_type,1,1),
                                           sep = "")

final_forecast <- base::lapply(1:base::nrow(grid_forecast_df), function(i){
  output <- train <- NULL
  output <- base::list()
  
  # Setting the training partition
  if(grid_forecast_df$w_type[i] == "expending"){
    train <- ts.obj
  } else if(grid_forecast_df$w_type[i] == "sliding"){
    train <- ts_sub <- stats::window(ts.obj, 
                                     start = stats::time(ts.obj)[grid_forecast_df$w_start[i]])
  }
  
  
  if(grid_forecast_df$model_abb[i] == "a"){
    md <- fc <- NULL
    a.arg$parallel <- parallel
    md <- base::do.call(forecast::auto.arima, 
                        c(base::list(train), 
                          a.arg))
    
    if("xreg" %in% base::names(a.arg)){
      fc <- forecast::forecast(md, h = h, xreg = xreg.h)
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "w"){
    md <- fc <- NULL
    md <- base::do.call(stats::HoltWinters, c(base::list(train), w.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "e"){
    md <- fc <- NULL
    md <- base::do.call(forecast::ets, c(base::list(train), e.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  } else if(grid_forecast_df$model_abb[i] == "n"){
    md <- fc <- NULL
    md <- base::do.call(forecast::nnetar, c(base::list(train), n.arg))
    if("xreg" %in% base::names(n.arg)){
      fc <- forecast::forecast(md, h = h, xreg = xreg.h)
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    output <- base::list(model = md, forecast = fc)
  } else if(grid_forecast_df$model_abb[i] == "t"){
    md <- fc <- NULL
    t.arg$use.parallel <- parallel
    md <- base::do.call(forecast::tbats, c(list(train), t.arg))
    fc <- forecast::forecast(md, h = h)
    output <- base::list(model = md, forecast = fc)
    
  }else if(grid_forecast_df$model_abb[i] == "b"){
    
    # Check if the bsts arguments are valid
    if(is.null(b.arg)){
      b.arg <-  list(linear_trend = TRUE,
                     seasonal = TRUE,
                     niter = 1000,
                     ping = 0,
                     family = "gaussian",
                     seed=1234)
    } else{
      
      if("linear_trend" %in% names(b.arg)){
        if(!b.arg$linear_trend %in% c(TRUE, FALSE)){
          warning("The value of the 'linear_trend' argument of the bsts model is invalid, using default (TRUE)")
          b.arg$linear_trend <- TRUE
        }
      } else {
        warning("The 'linear_trend' was not defined, using TRUE as default")
        b.arg$linear_trend <- TRUE
      }
      
      if("seasonal" %in% names(b.arg)){
        if(!b.arg$seasonal %in% c(TRUE, FALSE)){
          warning("The value of the 'seasonal' argument of the bsts model is invalid, using TRUE as default")
          b.arg$seasonal <- TRUE 
        } 
      } else {
        warning("The 'seasonal' argument was not defined, using TRUE as default")
        b.arg$seasonal <- TRUE
      }
      
      if("niter" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$niter)){
          warning("The value of the 'niter' argument of the bsts model is invalid, setting the argument to 1000")
          b.arg$niter <- 1000 
        } else if(b.arg$niter %% 1 != 0){
          warning("The value of the 'niter' argument of the bsts model is not integer, setting the argument to 1000")
          b.arg$niter <- 1000 
        }
      } else {
        warning("The 'niter' argument was not defined, setting the argument to 1000")
        b.arg$niter <- 1000
      }
      
      if("ping" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$ping)){
          warning("The value of the 'ping' argument of the bsts model is invalid, setting the argument to 100")
          b.arg$ping <- 100 
        } else if(b.arg$ping %% 1 != 0){
          warning("The value of the 'ping' argument of the bsts model is not integer, setting the argument to 100")
          b.arg$ping <- 1000 
        }
      } else {
        warning("The 'ping' argument was not defined, setting the argument to 100")
        b.arg$ping <- 100
      }
      
      if("seed" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$seed)){
          warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
          b.arg$seed <- 1234 
        } else if(b.arg$seed %% 1 != 0){
          warning("The value of the 'seed' argument of the bsts model is not integer, setting the argument to 1234")
          b.arg$seed <- 1234 
        }
      } else {
        warning("The 'seed' argument was not defined, setting the argument to 1234")
        b.arg$seed <- 1234
      }
      
      
      if("family" %in% names(b.arg)){
        if(!b.arg$family %in% c("gaussian", "logit", "poisson", "student")){
          warning("The value of the 'family' argument of the bsts model is invalid, using 'gaussian' as default")
          b.arg$family <- "gaussian"
        }
      } else{
        warning("The value of the 'family' argument is missing, using 'gaussian' as default")
        b.arg$family <- "gaussian"
      }
      
      
    }
    
    md <- fc <- ss <- fit.bsts <- burn <-  NULL
    ss <- list()
    if(b.arg$linear_trend){
      ss <- bsts::AddLocalLinearTrend(ss, train) 
    }
    if(b.arg$seasonal){
      ss <- bsts::AddSeasonal(ss, train, 
                              nseasons = stats::frequency(train))
    }
    
    md <- bsts::bsts(train, 
                     state.specification = ss, 
                     niter = b.arg$niter, 
                     ping= b.arg$ping, 
                     seed= b.arg$seed,
                     family = b.arg$family)
    fc <- stats::predict(md, horizon = h, quantiles = c(.025, .975))
    output <- base::list(model = md, forecast = fc)
  }
  
  if(grid_forecast_df$model_abb[i] == "h"){
    md <- fc <- NULL
    h.arg$parallel <- parallel
    md <- base::do.call(forecastHybrid::hybridModel, c(list(train), h.arg))
    
    
    if("xreg" %in% names(h.arg$a.args) ||
       "xreg" %in% names(h.arg$n.args) ||
       "xreg" %in% names(h.arg$s.args)){
      fc <- forecast::forecast(md, h = h, xreg = base::as.data.frame(xreg.h))
    } else{
      fc <- forecast::forecast(md, h = h)
    }
    
    output <- base::list(model = md, forecast = fc)
  }
  
  
  return(output)
  
}) %>%
  stats::setNames(grid_forecast_df$model_name)
}




# Parsing the forecast outputs 
model_output$ts.obj <- ts.obj
model_output$forecast <- final_forecast
model_output[["results"]] <- base::data.frame(model = purrr::map_chr(.x = backtesting_train, ~.x[["model_name"]]),
                                              window_type = purrr::map_chr(.x = backtesting_train, ~.x[["window_type"]]),
                                              mape = purrr::map_dbl(.x = backtesting_train, ~.x[["MAPE"]]),
                                              rmse = purrr::map_dbl(.x = backtesting_train, ~.x[["RMSE"]]),
                                              period = purrr::map_dbl(.x = backtesting_train, ~.x[["period"]]),
                                              stringsAsFactors = FALSE)


if(error == "MAPE"){
   model_output[["leaderboard"]]  <-  model_output$results %>% 
        dplyr::group_by(model, window_type) %>%
        dplyr::summarise(avgMAPE = base::mean(mape, na.rm = TRUE),
                         sdMAPE = stats::sd(mape, na.rm = TRUE),
                         avgRMSE = base::mean(rmse, na.rm = TRUE),
                         sdRMSE = stats::sd(rmse, na.rm = TRUE),
                         num_success_models = sum(!is.na(mape))) %>%
        dplyr::arrange(avgMAPE)
   
} else if(error == "RMSE"){
  model_output[["leaderboard"]]  <-  model_output$results %>% 
    dplyr::group_by(model, window_type) %>%
    dplyr::summarise(avgMAPE = base::mean(mape, na.rm = TRUE),
                     sdMAPE = stats::sd(mape, na.rm = TRUE),
                     avgRMSE = base::mean(rmse, na.rm = TRUE),
                     sdRMSE = stats::sd(rmse, na.rm = TRUE),
                     num_success_models = sum(!is.na(rmse))) %>%
    dplyr::arrange(avgRMSE)
}

top_models <- model_output$leaderboard[1:top,] %>% 
  dplyr::select(model, window_type) %>% 
  dplyr::mutate(flag = 1,
                model_name = paste(model, " (",
                                   base::substr(window_type, 1, 1),
                                   ")", sep = ""))

for(i in 1:base::length(backtesting_train)){
  
  base::eval(base::parse(text = base::paste("model_output$", 
              backtesting_train[[i]]$window_type,
              "$period_", 
              backtesting_train[[i]]$period,
              "$", backtesting_train[[i]]$model_name,
              " <-  base::list(model = backtesting_train[[i]]$model,",
              "forecast = backtesting_train[[i]]$forecast)",
              sep = "")))
}

# plotting the object 

  results_df <- model_output$results %>% dplyr::left_join(top_models) %>% 
    dplyr::filter(flag == 1)



results_df$model_name <- factor(results_df$model_name, 
                                levels = top_models$model_name,
                                ordered = TRUE)


if(palette %in% viridis_palettes){
color_ramp <- viridis::viridis_pal(option = base::eval(palette))(top)
} else if(palette %in% brewer_palettes){
  n_colors <- NULL
  n_colors <- RColorBrewer::brewer.pal.info$maxcolors[row.names(RColorBrewer::brewer.pal.info)  == palette]
  color_ramp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(top)
}

m <- base::levels(results_df$model_name)
p1 <- plotly::plot_ly()
p2 <- plotly::plot_ly()

if(error == "MAPE"){
for(i in seq_along(m)){
  p_df <- NULL
  p_df <- results_df %>% dplyr::filter(model_name == m[i])
  p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                 y = p_df$mape, 
                                 name = p_df$model_name, 
                                 legendgroup = p_df$model_name, 
                                 line = list(color = color_ramp[i])) %>% 
    plotly::layout(yaxis = list(title = "MAPE"),
                   xaxis = list(title = "Period"),
                   annotations = list(
                     text = "Error Dist by Model/Testing Period",
                     xref = "paper",
                     yref = "paper",
                     yanchor = "bottom",
                     xanchor = "center",
                     align = "center",
                     x = 0.5,
                     y = 1,
                     showarrow = FALSE
                   ))
  
  p2 <- p2 %>% plotly::add_trace(y = p_df$mape,
                                 type = "box",
                                 boxpoints = "all",
                                 jitter = 0.3,
                                 pointpos = -1.8, 
                                 name = p_df$model_name, 
                                 legendgroup = p_df$model_name,
                                 line = list(color = color_ramp[i]),
                                 marker = list(color = color_ramp[i]),
                                 showlegend=F) %>% 
    plotly::layout(title = "Backtesting Models Error Rate (MAPE)",
                   yaxis = list(title = "MAPE"),
                   xaxis = list(title = "Model",
                                tickangle = 45,
                                tickfont = list(size = 8)),
                   annotations = list(
                     text = "Error Dist by Model",
                     xref = "paper",
                     yref = "paper",
                     yanchor = "bottom",
                     xanchor = "center",
                     align = "center",
                     x = 0.5,
                     y = 1,
                     showarrow = FALSE
                   ))
}
} else if(error == "RMSE"){
  for(i in seq_along(m)){
    p_df <- NULL
    p_df <- results_df %>% dplyr::filter(model_name == m[i])
    p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                   y = p_df$rmse, 
                                   name = p_df$model_name, 
                                   legendgroup = p_df$model_name, 
                                   line = list(color = color_ramp[i])) %>% 
      plotly::layout(title = "Backtesting Models Error Rate (RMSE)",
                     yaxis = list(title = "RMSE"),
                     xaxis = list(title = "Period"),
                     annotations = list(
                       text = "Error Dist by Model/Testing Period",
                       xref = "paper",
                       yref = "paper",
                       yanchor = "bottom",
                       xanchor = "center",
                       align = "center",
                       x = 0.5,
                       y = 1,
                       showarrow = FALSE
                     ))
    
    p2 <- p2 %>% plotly::add_trace(y = p_df$rmse,
                                   type = "box",
                                   boxpoints = "all",
                                   jitter = 0.3,
                                   pointpos = -1.8, 
                                   name = p_df$model_name, 
                                   legendgroup = p_df$model_name,
                                   line = list(color = color_ramp[i]),
                                   marker = list(color = color_ramp[i]),
                                   showlegend=F) %>% 
      plotly::layout(title = "Backtesting Models Error Rate (RMSE)",
                     yaxis = list(title = "RMSE"),
                     xaxis = list(title = "Model",
                                  tickangle = 45,
                                  tickfont = list(size = 10)),
                     annotations = list(
                       text = "Error Dist by Model",
                       xref = "paper",
                       yref = "paper",
                       yanchor = "bottom",
                       xanchor = "center",
                       align = "center",
                       x = 0.5,
                       y = 1,
                       showarrow = FALSE
                     ))
  }
}
model_output$plot1 <- p1 
model_output$plot2 <- p2 

p3 <- TSstudio::plot_forecast(model_output$forecast[[base::paste(model_output$leaderboard$model[1], 
                                                                                  base::substr(model_output$leaderboard$window_type[1], 1, 1),
                                                                                  sep = "_" )]][["forecast"]]) %>%
  plotly::layout(annotations = list(
    text = paste(obj.name, " Best Forecast by ", error, " - ", 
                 model_output$leaderboard$model[1]," with ", 
                 base::toupper(base::substr(model_output$leaderboard$window_type[1], 1, 1)), 
                 base::substr(model_output$leaderboard$window_type[1], 2, 
                              base::nchar(model_output$leaderboard$window_type[1])),
                 " Window",  sep = ""),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  ))
model_output$plot3 <- p3 
model_output$summary_plot <- plotly::subplot(plotly::subplot(p1, p2, 
                                                             shareY = TRUE, 
                                                             titleX = TRUE, 
                                                             titleY = TRUE, 
                                                             nrows = 1),
                                             titleY = TRUE,
                                             p3, nrows = 2, margin = 0.1)
return(model_output)
}

brewer_palettes <- row.names(RColorBrewer::brewer.pal.info)
viridis_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")
x <- ts_test(ts.obj = USgas,
             window_length = 36,
             palette = "Set1",
             window_type = "both",
             h = 12)
x$plot1
x$plot2
x$summary_plot

  results_df
TSstudio::plot_forecast(x$forecast[[base::paste(x$leaderboard$model[1], 
                                                base::substr(x$leaderboard$window_type[1], 1, 1), sep = "_" )]][["forecast"]])

plotly::subplot(subplot(x$plot1, x$plot2), x$plot3, nrows = 2, margin = 0.1)
plotly::subplot(subplot(p1, p2), x$plot3, nrows = 2, margin = 0.1)

x$leaderboard

