rm(list  = ls())
library(TSstudio)
library(tsibble)


data("USgas")


input <- as_tsibble(USgas)
tsibble::is_regular(input)

# Frequency option
# Quarterly series -> quarter
# Monthly series -> month, quarter
# Weekly series -> week, month, and quarter
# Daily series -> day of the year, day of the week, week, month, and quarter
# Hourly series -> hour, day of the week, day of the year, day of the week, week, month, and quarter


input <- USgas

ts_reg <- function(input, 
                y, 
                x = NULL, 
                seasonal = NULL, 
                trend = TRUE, 
                ar = NULL, 
                method =  "lm", 
                method_arg = list(step = FALSE),
                scale = NULL){
  
  freq <- NULL
  freq <- base::names(base::which(purrr::map(tsibble::interval(df), ~.x) == 1))
  
  # Setting the input table
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
  
  # Setting the frequency component
  if(!base::is.null(seasonal)){
    if(freq == "quarter"){
      if(base::length(seasonal) == 1 & seasonal == "quarter"){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "quarter")
      } else if(base::length(seasonal) > 1 & "quarter" %in% seasonal){
        warning("Only quarter seasonal component can be used with quarterly frequency")
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "quarter")
      } else {
        stop("The seasonal component is not valid")
      }
    } else if(freq == "month"){
      if(base::length(seasonal) == 1 && seasonal == "month"){
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        x <- c(x, "month")
      } else if(all(seasonal %in% c("month", "quarter"))){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        x <- c(x, "month", "quarter")
      } else if(any(seasonal %in% c("month", "quarter"))){
        if("month" %in% seasonal){
          df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "month")
        } 
        
        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "quarter")
        }
        
        warning("For monthly frequency only 'month' or 'quarter' seasonal component could be used")
      }
    }
  }
      
  
  f <- stats::as.formula(paste("y ~ ", paste0(x, collapse = " + ")))
  md <- stats::lm(f, data = df)
  
  
  }
  
  
  
  
}

