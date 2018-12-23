ts.obj <- USgas
periods <- 6 # Number of testing periods
window_space <- 6 # Space between each testing partition
window_length <- 36 # Length of training window for sliding backtesting
window_test <- 12 # Length of the testing partition
models <- "abehntwp"  # Type of models
window_type <- "both" # Type of backtesting window 


# Creating a grid data frame
s <- length(ts.obj) - window_space * (periods - 1)
e <- length(ts.obj) 
w <- seq(from = s, by = window_space, to = e)

if(window_type == "both"){
  w_type <- c("sliding", "expanding")
} else {
  w_type <- window_type
}
model_list <- base::strsplit(models, "") %>% base::unlist()
grid_df <- base::expand.grid(model_list, w, w_type)
names(grid_df) <- c("model", "cut_point", "w_type")
head(grid_df)
tail(grid_df)

grid_df$start <- ifelse(grid_df$w_type == "sliding",
                        grid_df$cut_point - window_test - window_length + 1,
                        1)
