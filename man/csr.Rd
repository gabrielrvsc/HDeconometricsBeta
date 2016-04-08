\name{biclasso}
\alias{biclasso}
\title{Fits LASSO, Ridge and Elastic-net using glmnet and selectes the best model using the BIC.}
\usage{
biclasso(x,y,alpha=1,display=FALSE,penalty.factor=rep(1,ncol(x)))
}
\description{
The glmnet function estimates the desired model for several values of the regularization parameter lambda. This function uses the results from the glmnet to select the best version of the desired model using the BIC to decide. The glmet is programmed to cover the entire relevant space of lambda, which starts (for LASSO and Elastic-net) at the smaller lambda that excludes all variables and stops when all variables are included or, in case N>>T, it stops when the number of selected variable, p=T.
}

\arguments{
  \item{x}{T x N matrix with all candidate variables.}

  \item{y}{T-dimensional vector with the dependent variable.}
  
  \item{alpha}{1 for LASSO, 0 for Ridge and for any number between 0 and 1 for Elastic-net. See the glmnet documentation for more information.}
  \item{display}{If TRUE the function returns some plots regarding model adjustment.}
  \item{penalty.factor}{A N-dimensional vector used in cases when it is desired to give different weights for each variable. For example, in the adaptive LASSO these weights are calculated from some first step model.}
 
}

\value{
This function returns a list with several items. 

  \item{coef}{Coefficients of the best model selected using the BIC.}
  \item{lambda}{Best regularization parameter.}
  \item{bic}{Best model BIC.}
  \item{nvar}{Number of selected variables}
  \item{r2}{R-squared of the best model.}
  \item{adjr2}{Adjusted R-squared of the best model.}
  \item{glmnet-model}{Object with the entire output of the glmnet.}

}

\details{
Nothin to add.


}


\references{
Friedman, Jerome, Trevor Hastie, and Robert Tibshirani. "glmnet: Lasso and elastic-net regularized generalized linear models. R package version 1.9–5." (2013).


}

\examples{
# LASSO exaple
# 300 variable, 10 relevant, 100 observations
set.seed(123456)

N=300
p=10
T=101 #one more because of the autorregressive
betas=runif(p-1,-1,1)

data=dgp(N=N,p=p,T=T,betas=betas)
y=data$y
X=data$X

X=cbind(embed(y,2)[,-1],X[-1,])
y=y[-1]

lasso=biclasso(X,y,display=TRUE)

lasso$nvar #more variables than the true model

# adaptiveLASSO with tau=1
tau=1
weight=(abs(lasso$coef[-1])+1/sqrt(T-1))^(-tau)

adalasso=biclasso(X,y,display = TRUE,penalty.factor = weight)

# Results closer to the real model
adalasso$nvar
adalasso$coef[2:(p+1)]
betas

}


