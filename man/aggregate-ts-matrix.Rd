\name{aggregate.ts.matrix}
\alias{aggregate.ts.matrix}
\title{Aggregates a time-series matrix from daily to monthly or weekly.}

\usage{
aggregate.ts.matrix(matrix,aggregation=c("month","week"),FUN=sum,dates=NULL)
}

\arguments{
\item{matrix}{A T x N matrix of daily data.}
\item{aggregation}{type of aggregation}
\item{FUN}{Funtion used to aggregate. Standard is sum. One may also use mean, sd, var and more.}
\item{dates}{Ignore this argument if is.zoo(matrix)==TRUE. If rownames(matrix) is of date type you may also ignore this argument. 
If neither is the case, this argument is a vector of daily dates to be used on the time-series matrix.}
}
\value{
This function returns a time-series matrix with the desired aggregation.
}

