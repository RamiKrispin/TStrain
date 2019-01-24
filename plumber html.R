# dygraph example

#' Plot out data from the iris dataset
#' @get /hello
#* @serializer htmlwidget
function(){
  library(dygraphs)
  lungDeaths <- cbind(mdeaths, fdeaths)
  out <- dygraph(lungDeaths)
  print(out)
}