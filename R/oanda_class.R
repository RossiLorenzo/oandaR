#' @export
setClass("oanda_result", representation(type = "character", url = "character", result = "data.frame"))


#' @export
setGeneric("plot")
setMethod("plot", "oanda_result", function(x, type = c("Ask", "Bid")) {
  library(ggplot2)
  
  df = x@result
  df$next_time = c(df$time[-1], df$time[nrow(df)] + mean(diff(df$time)))
  df$middle_point = (df$next_time - df$time)/2 + df$time
  df$pos_neg = ifelse((df$closeBid - df$openBid) >= 0, "A", "B")
  if(x@type == "historical")
    G = oanda_candle(df)
  if(x@type == "inst_list")
})

#' @export
setGeneric("print")
setMethod("print", "oanda_result", function(x, n = 10) {
  df = x@result
  cat("Printing the first", n, "rows of", nrow(df), "\n")
  print.data.frame(df[1:n, ], row.names = FALSE)
  return(NULL)
})
