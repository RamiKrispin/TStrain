library(TSstudio)
data("USgas")
ts.obj <- USgas
models = "abehntw"
periods = 6
window_space = 6
window_test = 12
window_length = 48
window_type = "both"
error = "MAPE"
top = "all"
#window_size = 3,
h = 3
plot = TRUE
a.arg = NULL
b.arg = NULL
e.arg = NULL
h.arg = NULL
n.arg = NULL
t.arg = NULL
w.arg = NULL
xreg.h = NULL
parallel = FALSE
ts_backtesting <- function(ts.obj, 
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
                           a.arg = NULL,
                           b.arg = NULL,
                           e.arg = NULL,
                           h.arg = NULL,
                           n.arg = NULL,
                           t.arg = NULL,
                           w.arg = NULL,
                           xreg.h = NULL,
                           parallel = FALSE){}

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


#### Testing ####


models_map <- base::data.frame(model_abb = c("a","b", "e", "h", "n", "t", "w", "p"),
                               model = c("auto.arima", "bsts", "est", "hybridModel", 
                                         "nnetar", "tbats", "HoltWinters", "prophet"),
                               stringsAsFactors = FALSE)


s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w_end <- seq(from = s, by = window_space, to = e)

periods_map <- base::data.frame(w_end = w_end,
                                period = 1:periods)

model_list <- base::strsplit(models, "") %>% base::unlist()
if(window_type == "both"){
  w_type <- c("sliding", "expending")
} else{
  w_type <- window_type
}



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



backtesting_lapply <- base::lapply(1:base::nrow(grid_df), function(i){
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


# Parsing the forecast outputs 
model_output$ts.obj <- ts.obj
model_output[["results"]] <- base::data.frame(model = purrr::map_chr(.x = backtesting_lapply, ~.x[["model_name"]]),
                                              window_type = purrr::map_chr(.x = backtesting_lapply, ~.x[["window_type"]]),
                                              mape = purrr::map_dbl(.x = backtesting_lapply, ~.x[["MAPE"]]),
                                              rmse = purrr::map_dbl(.x = backtesting_lapply, ~.x[["RMSE"]]),
                                              period = purrr::map_dbl(.x = backtesting_lapply, ~.x[["period"]]),
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

top_models <- model_output$leaderboard[1:top,] %>% dplyr::select(model, window_type) %>% 
  dplyr::mutate(flag = 1)

for(i in 1:base::length(backtesting_lapply)){
  
  base::eval(base::parse(text = base::paste("model_output$", 
              backtesting_lapply[[i]]$window_type,
              "$period_", 
              backtesting_lapply[[i]]$period,
              "$", backtesting_lapply[[i]]$model_name,
              " <-  base::list(model = backtesting_lapply[[i]]$model,",
              "forecast = backtesting_lapply[[i]]$forecast)",
              sep = "")))
}

# plotting the object 
if(top != "all"){
  results_df <- model_output$results %>% dplyr::left_join(top_models) 
} else{
  results_df <- model_output$results 
}

if(window_type == "both"){
  results_df$model_name <- paste(results_df$model, " (",
                                 base::substr(results_df$window_type, 1, 1),
                                 ")", sep = "")
} else {
  results_df$model_name <- results_df$model
}

p1 <- plotly::plot_ly()

# Define the plot colors
color_ramp <- viridis::viridis(base::length(base::unique(results_df$model_name)), 
                               alpha = 1, 
                               direction = 1, 
                               begin = 0, 
                               end = 0.9)

style <- base::list()

for(i in base::seq_along(base::unique(results_df$model_name))){
  style[[i]] <- base::list(target = base::unique(results_df$model_name)[i], 
                           value = list(line =list(color = color_ramp[i])))
}

p <- plotly::plot_ly(
  type = 'scatter',
  x = results_df$period,
  y = results_df$mape,
  text = paste("Model: ", results_df$model_name,
               "<br>Period: ", results_df$period,
               "<br>MAPE: ", results_df$mape,
               "<br>RMSE: ", results_df$rmse),
  hoverinfo = 'text',
  mode = 'lines',
  transforms = list(
    list(
      type = 'groupby',
      groups = results_df$model_name,
      styles = style
    )
  )
)
p


error_df <- results_df %>% 
            dplyr::select(model_name, period, mape) %>%
            tidyr::spread(key = model_name, value = mape)

error_df
p2 <- plotly::plot_ly()
for(r2 in 2:base::ncol(error_df)){
  p2 <- base::suppressWarnings(p2 %>% plotly::add_trace(y = error_df[, r2], 
                                                        type = "box", 
                                                        boxpoints = "all", 
                                                        jitter = 0.3,
                                                        pointpos = -1.8, 
                                                        name =  names(error_df)[r2], 
                                                        marker = list(color = color_ramp[(r2 -1)]),
                                                        line = list(color = color_ramp[(r2 -1)]),
                                                        showlegend=F
  ))
}
p2
