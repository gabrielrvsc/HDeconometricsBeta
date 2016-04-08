\name{baggit}
\alias{baggit}
\title{A pre-defined function to use as the pre-testing for the bagging proceedure on method 2.}
\usage{
baggit(mat, pre.testing=c("joint","individual"), fixed.controls=NULL, t.stat=1.96)
}
\description{
This function calculates the pre-testing of the bagging proceedure in two different ways.

  1- Fits a regression with all variable and select those which are significant. Then fits an other regression only with the selected variable and saves the coefficients.
  
  2- Fits one individual regression for each candidate variable and select those which are individually significant to use on the second regression.

}

\arguments{
  \item{mat}{T x N+1 where the first column is the dependent variable and the other columns are for the independent variables.}

  \item{pre.testing}{If "joint", the all variables are tested in one single model before going to the final model. Note that this option is only feasible for T>N. If "Individual" then each variable is tested individualy before going to the final model. This method is feasible even when N>T if the procedure does not end with more variables than observation after the pre-testing.}
  
  \item{fixed.controls}{only for pre.testing="individual". A vector indexing variables that must be used as common controls in the individual pre-testing and also in the final model.}
  \item{t.stat}{Value of the t-statistic to define which variables remain after the pre-testing.}
 
}

\value{
This function returns a vector with the estimated coefficients. The excluded variables are zero. 

}

\details{
Nothin to add.


}



\examples{
## Generates data
set.seed(12345)
N=20
p=10
T=101 
betas=runif(p-1,-1,1)

data=dgp(N=N,p=p,T=T,betas=betas)
y=data$y
X=data$X

X=cbind(embed(y,2)[,-1],X[-1,])
y=y[-1]

# All variables together
test1=baggit(cbind(y,X),pre.testing="joint")
# Individual tests
test2=baggit(cbind(y,X),pre.testing="individual",fixed.controls = 1)

}


