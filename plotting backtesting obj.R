plot_backtesting <- function(backtesting.obj,
                 type = "summary",
                 by = "MAPE",
                 top = NULL){
  
  # error handling
  if(class(backtesting.obj) != "ts_backtesting"){
    stop("The input object is not a'ts_backtesting' class")
  }
  
  
}