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


periods_list <- base::paste0("period_", 1:periods)
partition_list <- list()
s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w_end <- seq(from = s, by = window_space, to = e)
w_end
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
grid_df <- base::expand.grid(model_list, w_end, w_type)
names(grid_df) <- c("model", "w_end", "w_type")
head(grid_df)
tail(grid_df)

grid_df$w_start <- ifelse(grid_df$w_type == "sliding",
                          grid_df$w_end - window_test - window_length + 1,
                          1)

head(grid_df)


test
backtesting_lapply <- base::lapply(1:base::nrow(grid_df), function(i){
  ts_sub <- train <- test <- ts_partition <- NULL
  
  ts_sub <- stats::window(ts.obj, 
                          start = stats::time(ts.obj)[grid_df$w_start[i]],
                          end = stats::time(ts.obj)[grid_df$w_end[i]])
  
  ts_partition <- TSstudio::ts_split(ts_sub, sample.out = window_test)
  train <- ts_partition$train
  test <- ts_partition$test
  
  output <- base::list()
  
})

length(backtesting_lapply)
backtesting_lapply[[1]]
