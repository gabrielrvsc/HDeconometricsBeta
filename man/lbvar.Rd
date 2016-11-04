\name{lbvar}
\alias{lbvar}
\title{Fits the Large Bayesian VAR from Bańbura et al. (2008)}
\usage{
lbvar (Y, p = 1, delta = 0, lambda = 0.05, xreg = NULL) 
}
\description{
This function fits the Large Bayesian VAR from Bańbura et al (2008). This method allows the user to fit very large VAR models using priors for regularization very fast.
}

\arguments{
  \item{Y}{T x N with all endogenous variables in the VAR.}

  \item{p}{VAR order.}
  
  \item{delta}{N dimensional vector with the mean priors for the first order autorregressive component of each endogenous variable in the system.}
  \item{lambda}{A value that controls balances the importance of the pior and the data. If lambda = 0 the posterior equals the prior. For large lambdas the estimates are the same as in an OLS VAR.}
  \item{xreg}{Matrix of exogenous controls.}
 
}

\value{
This function returns a list with several items. 

  \item{coef.by.equation}{A matrix where each line contains the coefficients for one equation of the VAR}
  \item{coef.by.block}{A list of matrixes where the first element contains the constant for all equations, the next p elements are N x N matrixes of each lag of the VAR and the last element is the exogenous controls coefficients. }
  \item{fitted}{Fitted values for all equations}
  \item{Y}{Original data.}
  \item{covmat}{Residuals covariance matrix }
  \item{xreg}{exogenous controls}
  

}



\references{
Banbura, Marta, Domenico Giannone, and Lucrezia Reichlin. "Large Bayesian VARs." (2008).

}

\examples{

library(vars)
data(Canada)
model=lbvar(Canada,p=4,delta=0,lambda=0.5)

}
