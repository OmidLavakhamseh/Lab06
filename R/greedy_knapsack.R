greedy_knapsack <-
function(x,W){
  stopifnot( is.data.frame(x),
             is.numeric(W),
             W>0,
             is.numeric(x$w),
             is.numeric(x$v),
             x$w >=0,
             x$v >= 0)
  x[,'Ratio'] <- x[,2]/x[,1]

  new_df <- x[order(-x$Ratio),]

  capacity =W
  weight = 0
  value = 0
  counter = 1
  elements = c()

  while( weight <= capacity){
    value = value + new_df[counter,2]
    row<- attr(new_df[counter,], "row.names")
    # row <- as.integer(rownames(new_df[counter,]))
    elements <-c(elements,row)
    counter = counter +1
    weight <- weight + new_df[counter, 1]
  }

  result = list(
    value = round(value),
    elements = elements
  )

  return(result)
}
