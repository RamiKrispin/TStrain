# plumber.R

#-------------------------------------------------------
# To call the API use
# pr <- plumber::plumb("Tuning ARIMA with plumber.R")
# pr$run()
# Open the swagger UI on your browser
#-------------------------------------------------------

#' Forecast with ARIMA model the AirPassenger dataset
#' @param p:int The order of the autoregressive model
#' @param d:int The degree of differencing
#' @param q:int The order of the moving-average model
#' @param P:int The order of the seasonal autoregressive model
#' @param D:int The degree of seasonal differencing
#' @param Q:int The order of the seasonal moving-average model
#' @param h:int Set forecast horizon
#' @get /plot
#' @png
function(p = 0, d = 0, q = 0, P = 0, D = 0 ,Q = 0, h = 12){
  data(AirPassengers)
  p <- as.numeric(p)
  d <- as.numeric(d)
  q <- as.numeric(q)
  P <- as.numeric(P)
  D <- as.numeric(D)
  Q <- as.numeric(Q)
  h <- as.numeric(h)
  
  md <- arima(AirPassengers, order = c(p,d,q), seasonal = list(order = c(P,D,Q)))
  fc <- forecast::forecast(md, h = h)
  p <- plot(fc)

}


pr <- plumber::plumb("Tuning ARIMA with plumber.R")
pr$run()
