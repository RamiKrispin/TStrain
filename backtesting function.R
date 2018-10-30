backtesting <- function(ts.obj, 
                        periods, 
                        window_type = "both", 
                        window_space,
                        window_length, 
                        window_test, 
                        models = "abehntwp",
                        xreg = NULL,
                        a.arg = NULL,
                        b.arg = NULL,
                        e.arg = NULL,
                        h.arg = NULL,
                        n.arg = NULL,
                        t.arg = NULL,
                        w.arg = NULL,
                        p.arg = NULL,
                        trend = TRUE,
                        seasonal = TRUE,
                        start_date = lubridate::ymd("2014-10-01"),
                        seed = 1234,
                        h){
  
  
  
}

ts.obj <- USgas
periods <- 6
window_type <- "both"
window_space <- 6
window_length <- 36
window_test <- 12
models <- "abehntwp"
xreg <- NULL
a.arg <- NULL
b.arg <- NULL
e.arg <- NULL
h.arg <- NULL
n.arg <- NULL
t.arg <- NULL
w.arg <- NULL
p.arg <- NULL
trend <- TRUE
seasonal <- TRUE
seed <- 1234
h <- 12

s <- e <- w <- NULL

s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w <- seq(from = s, by = window_space, to = e)


partition_s <- 1
partition_e <- w[i]
ts.sub <- stats::window(ts.obj, 
                        start = stats::time(ts.obj)[partition_s], 
                        end = stats::time(ts.obj)[partition_e])
ts_partition <- TSstudio::ts_split(ts.obj = ts.sub, sample.out = window_test)

train <- ts_partition$train
test <- ts_partition$test

forecast_function <- function(models = "abehntwp", ts.input, h, seed, bsts.trend, bsts.seasonal){
  model_char <-  base::unlist(base::strsplit(models, split = ""))
  output <- list()
  if("a" %in% model_char){
    md_a <- fc_a <- NULL
    md_a <- base::do.call(forecast::auto.arima, c(list(ts.input),a.arg))
    fc_a <- forecast::forecast(md_a, h)
    output$auto.arima <- list(model = md_a, forecast = fc_a)
  }
  
  if("b" %in% model_char){
    md_b <- fc_b <- ss <-  NULL
    if(base::is.null(b.arg)){
      b.arg <- list(niter = 1000, 
                    ping = 0, 
                    family = "gaussian", 
                    seed = seed,
                    burn = 0.1)
    } else if(!base::is.null(b.arg)){
      if("niter" %in% base::names(b.arg)){
        if(!base::is.numeric(b.arg$niter) || b.arg$niter < 0 || b.arg$niter %% 1 != 0){
          warning("The value of the 'niter' argument is not valid, using the default option (niter = 1000)")
          b.arg$niter == 1000
        }
      } else {
        warning("The value of the 'niter' argument is missing, setting it to 1000")
        b.arg$niter == 1000
      }
      
      if("ping" %in% base::names(b.arg)){
        if(!base::is.numeric(b.arg$ping)){
          warning("The value of the 'ping' argument is in valid, setting it to 0")
          b.arg$ping <- 0
        } else{
          warning("The value of the 'ping' argument is missing, setting it to 0")
          b.arg$ping == 0
        }
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
      
      if("seed" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$seed)){
          warning("The value of the 'seed' argument of the bsts model is invalid, setting the argument to 1234")
          b.arg$seed <- 1234 
        } else if(b.arg$seed %% 1 != 0){
          warning("The value of the 'seed' argument of the bsts model is not an integer, setting the argument to 1234")
          b.arg$seed <- 1234 
        }
      } else {
        warning("The 'seed' argument was not defined, setting the argument to 1234")
        b.arg$seed <- 1234
      }
      
      if("burn" %in% names(b.arg)){
        if(!base::is.numeric(b.arg$burn) || (b.arg$burn < 0 | b.arg$burn > 1)){
          warning("The value of the 'burn' argument is invalid, setting it to 0.1")
          b.arg$burn <- 0.1
        }
      } else {
        warning("The value of the 'burn' argument is missing, setting it to 0.1")
        b.arg$burn <- 0.1
      }
      
    }
    
    
    
    
    ss <- list()
    if(seasonal){
      ss <- bsts::AddSeasonal(ss, ts.input, nseasons = stats::frequency(ts.input))
    }
    if(trend){
      ss <- bsts::AddLocalLinearTrend(ss, ts.input)
    }
    
    md_b <- bsts::bsts(ts.input, 
                       state.specification = ss, 
                       niter = b.arg$niter,
                       ping = b.arg$ping)
    burn <- bsts::SuggestBurn(0.1, md_b)
    fc_b <- bsts::predict.bsts(md_b, burn = burn, horizon = h)
    output$bsts <- list(model = md_b, forecast = fc_b)
  }
  
  if("e" %in% model_char){
    md_e <- fc_e <- NULL
    md_e <- base::do.call(forecast::ets, c(list(ts.input),e.arg))
    fc_e <- forecast::forecast(md_e, h)
    output$ets <- list(model = md_e, forecast = fc_e)
  }
  
  if("h" %in% model_char){
    md_h <- fc_h <- NULL
    md_h <- base::do.call(forecastHybrid::hybridModel, c(list(ts.input),h.arg))
    fc_h <- forecast::forecast(md_h, h)
    output$hybrid <- list(model = md_h, forecast = fc_h)
  }
  
  if("n" %in% model_char){
    md_n <- fc_n <- NULL
    md_n <- base::do.call(forecast::nnetar, c(list(ts.input),n.arg))
    fc_n <- forecast::forecast(md_n, h)
    output$nnetar <- list(model = md_a, forecast = fc_a)
  }
  
  if("t" %in% model_char){
    if(!"formula" %in% names(t.arg)){
      if(!seasonal & !trend){
        warning("Cannot define a 'tslm' model when both the trend and seasonal ",
                "components are set to FALSE. ",
                "Setting a 'tslm' model with trend and seasonal components")
        t.arg$formula <- formula <-  stats::as.formula("ts.input ~ trend + season")
      } else if(trend & !seasonal){
        t.arg$formula <- formula <-  stats::as.formula("ts.input ~ trend")
      } else if(!trend & seasonal){
        t.arg$formula <- formula <-  stats::as.formula("ts.input ~ season")
      } else if(trend & seasonal){
        t.arg$formula <- formula <-  stats::as.formula("ts.input ~ trend + season")
      }
    }
    md_t <- fc_t <- NULL
    md_t <- base::do.call(forecast::tslm, c(list(ts.input),t.arg))
    fc_t <- forecast::forecast(md_t, h)
    output$tslm <- list(model = md_t, forecast = fc_t)
  }
  
  if("w" %in% model_char){
    md_w <- fc_w <- NULL
    md_w <- base::do.call(stats::HoltWinters, c(list(ts.input),w.arg))
    fc_w <- forecast::forecast(md_w, h)
    output$HoltWinters <- list(model = md_w, forecast = fc_w)
  }
  
  if("p" %in% model_char){
    md_p <- fc_p <- freq <- prophet_df <- future_df <- NULL
    prophet_df <- ts_to_prophet(ts.input, start = start_date) 
    md_p <- base::do.call(prophet::prophet, c(list(ts.input), p.arg))
    
    if(stats::frequency(ts.input) == 365 | stats::frequency(ts.input) == 365.25){
      freq = "day"
    } else if(stats::frequency(ts.input) == 52 | 
              stats::frequency(ts.input) == 365 / 7 | 
              stats::frequency(ts.input) == 365.25 / 7 ){
      freq = "week"
    } else if(stats::frequency(ts.input) == 12){
      freq = "month"
    } else if(stats::frequency(ts.input) == 4){
      freq = "quarter"
    } else if(stats::frequency(ts.input) == 1){
      freq = "year"
    }
    future_df <- prophet::make_future_dataframe(md_p, periods = h, freq = freq)
    fc_p <- stats::predict(md_p, future_df)
  }
}


