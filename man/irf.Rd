\name{irf}
\alias{irf}
\title{Fit Vector Autorregressive models by OLS and regularization including LASSO and adaLASSO.}
\usage{
irf(model,ident,h,unity.shock=TRUE)
}

\description{

This function fits VAR models using OLS and shrinkage such as LASSO and adaLASSO. 

}


\arguments{

  \item{Y}{T x k matrix with the endogenous variables in the VAR}

  \item{p}{Order of the VAR. Recall that for OLS estimation T-p must be bigger than k*p}
  
  \item{type}{"ols" and "LASSO" implemented so far. LASSO stands for the l1 penalty and it allows weights unsing the argument penalty.factor}
  
  \item{penalty.factor}{k X k*p Matrix  where each line contains the weights for the coefficients of one equation of the VAR}
  
}

\value{
This function returns a list with several items. 

  \item{coef.by.equation}{k X k*p +1 matrix where the first columns is the intersect and the other columns are the coefficients. Each line is an equation and the coefficients are ordered by the lag and the column order of Y.}
  \item{coef.by.block}{A list where the first element is a vector with the intersect of each equation and the other elements are the VAR coefficient matrix for each lag.}
  \item{fitted}{VAR fitted values.}
  \item{residuals}{VAR residuals.}
  \item{N}{Number of variables in each VAR equation.}
  \item{p}{VAR order.}
  \item{covmat}{residuals covariance matrix.}
  \item{Y}{Supplied Y matrix.}
  \item{type}{Supplied type.}
}

\details{

See ?identificatio ?irf and ?boot.irf for other VAR functions.

}


\examples{

## = generate data = ##
dados=dgp(100,20,200)
Y=dados$X[,1:4]
p=3 # var order
model=fitvar(Y,p,type="ols")
}
