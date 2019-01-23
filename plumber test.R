# plumber.R

#' Forecast with ARIMA model the AirPassenger dataset
#' @param p:int
#' @param d:int
#' @param q:int
#' @param P:int
#' @param D:int
#' @param Q:int
#' @param h:int
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