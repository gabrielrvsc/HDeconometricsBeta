
aggregate.ts.matrix=function (matrix, aggregation = c("month", "week"), FUN = sum, 
          dates = NULL) 
{
  if (length(dates) == 0) {
    dates = as.Date(row.names(matrix))
  }
  if (is.zoo(matrix) == FALSE) {
    matrix = zoo(matrix, dates)
  }
  if (aggregation == "month") {
    month = function(x) format(x, "%Y.%m")
    tsm = aggregate(matrix, by = month, FUN = function(x) FUN(x,na.rm=TRUE))#, na.action = na.pass)
  }
  else {
    week <- function(x) format(x, "%Y.%W")
    tsm = aggregate(matrix, by = week, FUN = function(x) FUN(x,na.rm=TRUE))#, na.action = na.pass)
  }
  return(tsm)
}
