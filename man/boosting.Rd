\name{boosting}
\alias{boosting}
\title{Implementation of the Bai & Ng (2009) component-wise boosting for dynamic models.}
\usage{
boosting(y,X,v=0.2,minIt=ncol(X)/2,maxIt=10*ncol(X),display=TRUE,crit.break=TRUE)
}

\description{
This is an implementation for time-series component-wise boosting using the results from Bai and Ng (2009). The algorithm has its own way of determining when to stop. Keep crit.break=TRUE if you want to use the standard stop criterion. 

Note that the information criteria automaticaly adjusts the degrees of freedom of the model considering that the boosting may select the same variable more than once.
}

\arguments{
  \item{y}{T-dimensional vector with the dependent variable.}
  
  \item{x}{T x N matrix with all candidate variables.}
  
  \item{v}{Algorithm step size.}
  
  \item{minIt}{Minimum number of iterations. Effective only if crit.break=TRUE.}
  
  \item{maxIt}{Maximum number of iterations.}
  
  \item{display}{Show iteration counter.}
  
  \item{crit.break}{If TRUE, algorithm breaks when the minimum information criteria is likely to be found. If FALSE the algorithm stops only when maxIt is reached.}
}

\value{
This function returns a list with several items. 

  \item{coef}{Boosting coefficients for the model with the smallest information criteria.}
  \item{fitted.values}{In sample fitted-values.}
  \item{best.crit}{Smallest information criteria.}
  \item{all.crit}{Information criteria in each iteration.}
  \item{df}{Degrees of freedom of the best model. }

}

\details{
Nothin to add.


}


\references{
Bai, Jushan, and Serena Ng. "Boosting diffusion indices." Journal of Applied Econometrics 24.4 (2009): 607-629.


}

\examples{
## Import data on the Brazilian inflation and some determinant factors
data(brinf)
y=brinf[,1]
X=brinf[,-1]

boostest=boosting(y,X,v=0.2)

# Shows the information criteria for all interations
plot(boostest$all.crit,type="l")

# fitted values and real values
plot(y,type="l")
lines(boostest$fitted,col="red")

}


