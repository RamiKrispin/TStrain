library(TSstudio)
library(tsibble)


data("USgas")


input <- as_tsibble(USgas)
tsibble::is_regular(input)
freq <- tsibble::interval(input)


input <- USgas

ts_reg <- function(input, 
                y, 
                x = NULL, 
                seasonal = FALSE, 
                trend = FALSE, 
                ar = NULL, 
                method = "lm",
                method_arg = list(step = FALSE),
                scale = NULL){
  
  freq <- NULL
  
  
  
  if(any(class(input) == "tbl_ts")){
    df <- input
  } else if(any(class(input) == "ts")){
    df <- as_tsibble(input) %>% setNames(c("index", "y"))
    y <- "y"
    if(!base::is.null(x)){
      warning("The 'x' argument cannot be used when input is a 'ts' class")
    }
    x <- NULL

  }
  
  freq <- base::names(base::which(purrr::map(tsibble::interval(df), ~.x) == 1))
  if(seasonal){
    if("month" %in% freq){
      df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
      x <- c(x, "month")
    } else if("quarter" %in% freq){
      df$quarter <- lubridate::quarter(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
      x <- c(x, "quarter")
    } else if("week" %in% freq){
      df$quarter <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
      x <- c(x, "week")
    } 
  }
  
  if(trend){
    df$trend <- 1:nrow(df)
    x <- c(x, "trend")
  }
  
  if(!base::is.null(ar)){
    if(!base::is.numeric(ar)){
      warning("The 'ar' argument is not valid and will be ignored")
    } else if(base::any((ar %% 1) > 0 )){
      warning("The 'ar' argument is not valid and will be ignored")
    } else {
    
      for(lag in ar){
        df[paste("lag", lag, sep = "")] <- dplyr::lag(df$y, n = lag)
        
        x <- c(x, paste("lag", lag, sep = ""))
      }
    }
      
      
    }
  }
  
  
  
  
}

