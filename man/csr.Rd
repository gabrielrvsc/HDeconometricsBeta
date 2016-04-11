\name{csr}
\alias{csr}
\title{Estimates the Complete Subset Regressions of Elliott et. al (2013)}
\usage{
csr(y,X,K=20,k=4,fixed.controls=NULL)
}

\description{
This function allows the user to pre-test which variables are to be used in the CSR. The CSR tends to be computationaly slow when the total number of variables or the number of variables in each subset grow very big. 

The pre-testing is activated automatically in the total number of variables is bigger than K. If the user chooses K=number of variables then the procedure is exactly the one presented in Elliott et. al (2013). 

Additionaly, fixed controls are allowed. If the user choosed to have fixed controls in the model, the procedure is almost the same. However, the variables selected to be fixed will be used as controls in the pre-testing and in the subset of regressions. In this case, the K in the pre-testing becomes the number of variables to be selected ignoring those which are fixed controls. 

}


\arguments{

  \item{y}{T-dimensional vector with the dependent variable.}

  \item{x}{T x N matrix with all candidate variables.}
  
  \item{K}{Number of variables to be selected after the pre-testing. Make "K=total number of variables" if you wish to skip the pre-testing and use all variables in the CSR.}
  
  \item{k}{Number of variables in each subset.}
  
  \item{fixed.controls}{A vector indexing variables that must be used as common controls in the individual pre-testing and also in the regressions in the subset.}
  
  
}

\value{
This function returns a list with several items. 

  \item{coefficients}{A matrix with the coefficients estimated in each regression of the subset. The number of rows if the coeficcients matrix is combn(K,k) and the number of columns is nvar+1. The first column is for the constant.}
  \item{type}{Object determining the type of model to be used in other functions.}

}

\details{

The pre-testing procedure is just to fit individual regressions for each variables and select the K variables with bigger t-statistics. 

Fitted values are not calculated to save computer time. If you wish to see the fitted values use the function predict.HDeconometrics with the in-sample observations.

}


\references{
Elliott, Graham, Antonio Gargano, and Allan Timmermann. "Complete subset regressions." Journal of Econometrics 177.2 (2013): 357-373.
}

\examples{
##########complete subset regression
data(brinf)
K=25
k=4
y=brinf[,1]
X=brinf[,-1]

##- csr - no fixed controls
csr.test=csr(y,X)

##- csr - first four variables as fixed controls
csr1.test=csr(y,X,k=4,K=20,fixed.controls=c(1:4))

## Compare fitted values with a plot
plot(y,type="l")

fitted=predict.HDeconometrics(csr.test,X)
fitted1=predict.HDeconometrics(csr1.test,X)

lines(fitted,col="red")
lines(fitted1,col="blue")

}


