library(TSstudio)
library(plotly)
library(forecast)
data(USgas)
data("USVSales")
ts_plot(USgas)

USgas <- USVSales

ts_decompose(USgas)

ts_seasonal(USgas, type = "all")

ts_lags(USgas)

ts_lags(USgas, lags = c(12, 24, 36, 48, 60))

df <- data.frame(y = as.numeric(USgas), 
                 year = floor(time(USgas)), 
                 month = as.factor(cycle(USgas)),
                 lag12 = dplyr::lag(as.numeric(USgas), n = 12),
                 lag24 = dplyr::lag(as.numeric(USgas), n = 24))

df$index <- 1:nrow(df)

head(df, 13)


h <- 12
USgas_split <- ts_split(USgas, sample.out = h)
train_ts <- USgas_split$train
test_ts <- USgas_split$test

train <- df[1:(nrow(df) - h),]
test <- df[(nrow(df) - h + 1):nrow(df),]


md1 <- lm(y ~ index, data = train)

df$yhat1 <- predict(md1, newdata = df)

plot_ly(data = df) %>%
  add_lines(x = ~ index, y = ~ y)  %>%
  add_lines(x = ~ index, y = ~ yhat1)  

test$yhat1 <- predict(md1, newdata = test)

mean(abs(test$y - test$yhat1) / test$y)

md2 <- lm(y ~ index + month, data = train)

df$yhat2 <- predict(md2, newdata = df)
plot_ly(data = df) %>%
  add_lines(x = ~ index, y = ~ y)  %>%
  add_lines(x = ~ index, y = ~ yhat2)


test$yhat2 <- predict(md2, newdata = test)

mean(abs(test$y - test$yhat2) / test$y)


md3 <- lm(y ~ index + month + lag12 + lag24, data = train)

df$yhat3 <- predict(md3, newdata = df)
plot_ly(data = df) %>%
  add_lines(x = ~ index, y = ~ y)  %>%
  add_lines(x = ~ index, y = ~ yhat3)


test$yhat3 <- predict(md3, newdata = test)

mean(abs(test$y - test$yhat3) / test$y)


md4 <- auto.arima(train_ts, stepwise = FALSE, approximation = FALSE)
fc4 <- forecast(md4, h = h)
accuracy(fc4, test_ts)
test_forecast(actual = USgas, forecast.obj = fc4, test = test_ts)

md5 <- tslm(train_ts ~ season + trend)
fc5 <- forecast(md5, h = h)
accuracy(fc5, test_ts)

md6 <- ets(train_ts)
fc6 <- forecast(md6, h = h)
accuracy(fc6, test_ts)

md7 <- HoltWinters(train_ts)
fc7 <- forecast(md7, h = h)
accuracy(fc7, test_ts)


library(h2o)
h2o.init(max_mem_size = "16g")

train_h <- as.h2o(train)
test_h <- as.h2o(test)

x <- c("index", "month", "lag12", "lag24")
y <- "y"

hyper_params <- list(max_depth = seq(1, 29, 2))

rf1 <- h2o.grid("randomForest",
                grid_id="depth_grid",
                search_criteria = list(strategy = "Cartesian"),
                hyper_params = hyper_params,
                ntrees = 1000,
                sample_rate = 0.8,
                seed = 1234,
                stopping_rounds = 5,
                stopping_tolerance = 0.0001,
                stopping_metric = "RMSE",
                training_frame = train_h,
                y = y,
                x = x, 
                nfolds = 5
                )


sortedGrid <- h2o.getGrid("depth_grid", sort_by="rmse", decreasing = FALSE)    
sortedGrid



## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth

rf1a <- h2o.getModel(sortedGrid1@model_ids[[1]])
test_h$yhat_rf1 <-  as.numeric(h2o.predict(rf1a, test_h))
test_h2o <- as.data.frame(test_h)
plot(test_h2o$y, type = "l")
lines(test_h2o$yhat_rf1, col = "red")

mean(abs(test_h2o$y - test_h2o$yhat_rf1) / test_h2o$y)
hyper_params <- list(max_depth = seq(minDepth, maxDepth, 1),
                     ntrees = c(50, 100),
                     sample_rate = seq(0.75, 0.9, 0.01)
                     )


rf2 <- h2o.grid("randomForest",
                grid_id="grid1",
                search_criteria = list(strategy = "Cartesian"),
                hyper_params = hyper_params,
                seed = 1234,
                stopping_rounds = 10,
                stopping_tolerance = 0.00005,
                stopping_metric = "RMSE",
                training_frame = train_h,
                y = y,
                x = x, 
                nfolds = 5
)

sortedGrid2 <- h2o.getGrid("grid1", sort_by="rmse", decreasing = FALSE)    
sortedGrid2
rf2a <- h2o.getModel(sortedGrid2@model_ids[[1]])
test_h$yhat_rf2 <-  as.numeric(h2o.predict(rf2a, test_h))
test_h2o <- as.data.frame(test_h)
plot(test_h2o$y, type = "l")
lines(test_h2o$yhat_rf1, col = "red")
lines(test_h2o$yhat_rf2, col = "green")

mean(abs(test_h2o$y - test_h2o$yhat_rf2) / test_h2o$y)

# GBM

hyper_params = list( max_depth = seq(1,29,2) )

gbm1 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_gbm",
  x = x, 
  y = y, 
  training_frame = train_h, 
  nfolds = 5,
  ntrees = 10000,                                                            
  learn_rate = 0.02,                                                         
  learn_rate_annealing = 0.99,                                               
  sample_rate = 0.8,              
  col_sample_rate = 0.8, 
  seed = 1234,                                                             

  stopping_rounds = 5,
  stopping_tolerance = 0.00005,
  stopping_metric = "RMSE", 
  score_tree_interval = 10                                                
)

sortedGrid <- h2o.getGrid("depth_gbm", sort_by="rmse", decreasing = FALSE)    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth
gbm1a <- h2o.getModel(sortedGrid1@model_ids[[1]])
test_h$yhat_gbm1 <-  as.numeric(h2o.predict(gbm1a, test_h))
test_h2o <- as.data.frame(test_h)
plot(test_h2o$y, type = "l")
lines(test_h2o$yhat_rf1, col = "red")
lines(test_h2o$yhat_gbm1, col = "green")
mean(abs(test_h2o$y - test_h2o$yhat_gbm1) / test_h2o$y)


hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.05),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.05),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.05),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train_h))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4)
)

search_criteria = list(
  strategy = "RandomDiscrete",      
  max_runtime_secs = 3600,         
    seed = 1234,                        
  stopping_rounds = 5,                
  stopping_metric = "RMSE",
  stopping_tolerance = 1e-4
)


gbm2 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm = "gbm",
  grid_id = "gbm_grid2", 
  x = x, 
  y = y, 
  training_frame = train_h, 
  nfolds = 5,
  ntrees = 10000,                                                            
  learn_rate = 0.02,                                                         
  learn_rate_annealing = 0.99,                                            
  score_tree_interval = 10,
  seed = 1234                                                             
)

## Sort the grid models by AUC
sortedGrid2 <- h2o.getGrid("gbm_grid2", sort_by = "rmse", decreasing = FALSE)    
sortedGrid2

gbm2a <- h2o.getModel(sortedGrid2@model_ids[[1]])
test_h$yhat_gbm2 <-  as.numeric(h2o.predict(gbm2a, test_h))
test_h2o <- as.data.frame(test_h)
plot(test_h2o$y, type = "l")
lines(test_h2o$yhat_rf1, col = "red")
lines(test_h2o$yhat_gbm1, col = "green")
lines(test_h2o$yhat_gbm2, col = "yellow")

mean(abs(test_h2o$y - test_h2o$yhat_gbm1) / test_h2o$y)

