plot_backtesting <- function(backtesting.obj,
                 type = "summary",
                 by = "MAPE",
                 top = NULL){
  
  top_models <- NULL
  
  # error handling
  if(class(backtesting.obj) != "ts_backtesting"){
    stop("The input object is not a'ts_backtesting' class")
  }
  
  #top
  if(base::is.null(top)){
    top <- nrow(backtesting.obj$leaderboard)
  }
  
  #by
  #type
  
  
  if(type == "summary"){
    
    top_models <- model_output$leaderboard[1:top,] %>% 
      dplyr::select(model, window_type) %>% 
      dplyr::mutate(flag = 1,
                    model_name = paste(model, " (",
                                       base::substr(window_type, 1, 1),
                                       ")", sep = ""))
    
    
    
    p1 <- plotly::plot_ly()
    p2 <- plotly::plot_ly()
    
    if(by == "MAPE"){
      leaderboard <- backtesting.obj$leaderboard %>% dplyr::arrange(avgMAPE)
      results_df <- backtesting.obj$results
      m <- leaderboard$model
      for(i in seq_along(m)){
        p_df <- NULL
        p_df <- results_df %>% dplyr::filter(model_name == m[i])
        p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                       y = p_df[,error_type], 
                                       name = p_df$model_name, 
                                       legendgroup = p_df$model_name, 
                                       line = list(color = color_ramp[i])) %>% 
          plotly::layout(yaxis = list(title = "MAPE"),
                         xaxis = list(title = "Period"))
        
        p2 <- p2 %>% plotly::add_trace(y = p_df$mape,
                                       type = "box",
                                       boxpoints = "all",
                                       jitter = 0.3,
                                       pointpos = -1.8, 
                                       name = p_df$model_name, 
                                       legendgroup = p_df$model_name,
                                       line = list(color = color_ramp[i]),
                                       marker = list(color = color_ramp[i]),
                                       showlegend=F) %>% 
          plotly::layout(title = "Backtesting Models Error Rate (MAPE)",
                         yaxis = list(title = "MAPE"),
                         xaxis = list(title = "Model",
                                      tickangle = 45,
                                      tickfont = list(size = 8)))
      }
    } else if(by == "RMSE"){
      for(i in seq_along(m)){
        p_df <- NULL
        p_df <- results_df %>% dplyr::filter(model_name == m[i])
        p1 <- p1 %>% plotly::add_lines(x = p_df$period, 
                                       y = p_df$rmse, 
                                       name = p_df$model_name, 
                                       legendgroup = p_df$model_name, 
                                       line = list(color = color_ramp[i])) %>% 
          plotly::layout(title = "Backtesting Models Error Rate (RMSE)",
                         yaxis = list(title = "RMSE"),
                         xaxis = list(title = "Period"))
        
        p2 <- p2 %>% plotly::add_trace(y = p_df$rmse,
                                       type = "box",
                                       boxpoints = "all",
                                       jitter = 0.3,
                                       pointpos = -1.8, 
                                       name = p_df$model_name, 
                                       legendgroup = p_df$model_name,
                                       line = list(color = color_ramp[i]),
                                       marker = list(color = color_ramp[i]),
                                       showlegend=F) %>% 
          plotly::layout(title = "Backtesting Models Error Rate (RMSE)",
                         yaxis = list(title = "RMSE"),
                         xaxis = list(title = "Model",
                                      tickangle = 45,
                                      tickfont = list(size = 10)))
      }
    }
    
    
    
    
  }
  
}