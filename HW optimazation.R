#' Tuning Time Series Forecasting Models Parameters with Grid Search 
#' @export ts_grid
#' @param ts.obj A univariate time series object of a class "ts"
#' @param model A string, defines the model
#' @param periods A string, set the number backtesting periods
#' @param window_length An integer, defines the length of the backtesting training window.
#' If set to NULL (default) will use an expending window starting the from the first observation,
#'  otherwise will use a sliding window.
#' @param window_space An integer, set the space length between each of the backtesting training partition 
#' @param window_test An integer, set the length of the backtesting testing partition
#' @param hyper_params A list, defines the tuning parameters and their range
#' @param parallel Logical, if TRUE use multiple cores in parallel
#' @param n.cores Set the number of cores to use if the parallel argument is set to TRUE. 
#' @description Tuning time series models with grid serach approach using backtesting method.
#'  If set to "auto" (default), will use all available cores in the system minus 1
#'  @return A list

ts_grid <- function(ts.obj, 
                    model, 
                    periods,
                    window_length = NULL, 
                    window_space,
                    window_test,
                    hyper_params,
                    parallel = TRUE,
                    n.cores = "auto"){
  
  mape <- period <- start_time <-  NULL
  
  `%>%` <- magrittr::`%>%` 
  
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
    n.cores <- base::as.numeric(future::availableCores() - 1)
  }
  
  if(!base::exists("model")){
    stop("The 'model' argument is missing")
  } else if(!model %in% c("HoltWinters")){
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
    if(!base::all(base::names(hyper_params) %in% hw_par)){
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
  if(!parallel){
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
      dplyr::bind_rows() %>%
      tidyr::spread(key = period, value = mape)
  } else if(parallel){
    future::plan(future::multiprocess, workers = n.cores)  
    start_time <- Sys.time()
    grid_output <- future.apply::future_lapply(1:periods, function(n){
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
      dplyr::bind_rows() %>%
      tidyr::spread(key = period, value = mape)
  }
  
  col_mean <- base::which(!base::names(grid_output)  %in% base::names(hyper_params) )
  grid_output$mean <- base::rowMeans(grid_output[, col_mean])
  grid_output <- grid_output %>% dplyr::arrange(mean)
  
  final_output <- list(grid_df = grid_output)
  
  for(i in base::names(hyper_params)){
    final_output[[i]] <- grid_output[1, i]
  }
  final_output[["parameters"]] <- list(series = ts.obj, 
                                       model = model, 
                                       periods = periods,
                                       window_length = window_length, 
                                       window_space = window_space,
                                       window_test = window_test,
                                       hyper_params = hyper_params,
                                       parallel = parallel,
                                       n.cores = n.cores)
  
  base::class(final_output) <- "ts_grid" 
  return(final_output)
}


# Example

data(USgas, package = "TSstudio")

USgas_grid <- ts_grid(USgas,
                      model = "HoltWinters",
                      periods = 6,
                      window_length = NULL,
                      window_space = 6,
                      window_test = 12,
                      parallel = TRUE,
                      n.cores = 8,
                      hyper_params = list(alpha = seq(0, 1, 0.1),
                                          beta = seq(0, 1, 0.1),
                                          gamma = seq(0, 1, 0.1)))
View(USgas_grid$grid_df)

md <- HoltWinters(USgas, 
                  alpha = USgas_grid$grid_df$alpha[1],
                  beta = USgas_grid$grid_df$beta[1],
                  gamma = USgas_grid$grid_df$gamma[1])
fc <- forecast::forecast(md, h = 60)
TSstudio::plot_forecast(fc)

class(USgas_grid)
plot_grid <- function(grid.obj, top = NULL, highlight = 0.1){
  
  high_light <- NULL
  # Error handle
  if(class(grid.obj) != "ts_grid"){
    stop("The input object is not a 'ts_grid' class")
  }
  
  if(base::is.null(top)){
    if(!base::is.numeric(top) || top %% 1 != 0){
      warning("The value of the 'top' argument is not valid, using default option (top 100 models)")
      top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
    }
    if(top > base::nrow(grid.obj$grid_df)){
      warning("The value of the 'top' argument exceeding the number of models, using default option (top 100 models)")
      top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
    }
  } else { 
    top <- ifelse(base::nrow(grid.obj$grid_df) > 100, 100, base::nrow(grid.obj$grid_df))
    }
  
  if(!base::is.numeric(highlight) || highlight <= 0 || highlight > 1){
    warning("The value of the 'highlight' argument is not valid, using default (0.1)")
    highlight <- 0.1
  }
  
  
  high_light <- 1:base::ceiling(top * highlight)
  
 hw_dim <- NULL
 hw_dim <- base::list()
 
  if(grid.obj$parameters$model == "HoltWinters"){
    if(base::length(base::names(grid.obj$parameters$hyper_params)) < 2){
      stop("Cannot create a parallel coordinates plot for a single hyper parameter")
    }
         for(i in base::seq_along(base::names(grid.obj$parameters$hyper_params))){
          hw_dim[[i]] <-  base::eval(base::parse(text = base::paste("list(range = c(0,1),
                constraintrange = c(min(grid.obj$grid_df[1:20, i]),
                                    max(grid.obj$grid_df[1:20,i])),
                  label = i, values = ~", 
                                base::names(grid.obj$parameters$hyper_params)[i],
                                ")",
                                sep = "")
          ))
         }
 
    grid.obj$grid_df[1:top,] %>%
      plotly::plot_ly(type = 'parcoords',
                      line = list(color = ~ mean,
                                  colorscale = 'Jet',
                                  showscale = TRUE,
                                  reversescale = F,
                                  cmin = base::min(x$grid_df$mean),
                                  cmax = base::min(x$grid_df$mean) * 1.5),
                      dimensions = hw_dim
      )
    
  }
}

