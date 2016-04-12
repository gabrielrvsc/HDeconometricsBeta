
aggregate.ts.matrix=function(matrix,aggregation=c("month","week"),FUN=sum,dates=NULL){

  if(length(dates)==0){
    dates=as.Date(row.names(final.table))
  }
  if(is.zoo(tsm)==FALSE){
    matrix=zoo(matrix,dates)
  }
  if(aggregation=="month"){
    month = function(x)format(x, '%Y.%m')
    tsm=aggregate(tsm, by=month, FUN=FUN)
  }else{
    week <- function(x)format(x, '%Y.%W')
    tsm=aggregate(tsm, by=week, FUN=FUN)
  }
  return(tsm)
}
