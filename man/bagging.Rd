\name{bagging}
\alias{bagging}
\title{Estimates bagging coefficients for a given pre-testing procedure.}
\usage{
# Method 1:
bagging(y,X,R=100,l=5,sim="fixed",pre.testing=c("joint","individual"),fixed.controls=NULL,...)

# Method 2:
bagging(y,X,fn,R=100,l=5,sim="fixed",pre.testing="personal",...)

}
\description{
This function returns the pre-testing coefficients for all bootstrap samples. This coefficients may then be used to calculate forecasts. 
}

\arguments{
  \item{y}{T-dimensional vector with the dependent variable.}

  \item{x}{T x N matrix with all candidate variables.}
  
  \item{R}{Number of bootstrap samples to be used.}
  
  \item{l}{length of the blocks for blockbootstrap.}
 
  \item{sim}{How the blockboostrap is computed. See ?tsboot for details.}
  
  \item{pre.testing}{For method 1 this argument defines which pre-testing of the function baggit to be used. For method 2 this argument must be set to "personal", indicating that some other user defined pre-testing is to be used. }
  
  \item{fixed.controls}{Only for method 1. Vector of indexes to determine which variables are fixed if pre.testing="individual". See ?baggit for details.}
  
  \item{fn}{User define pre-testing for method 2.}
  
  \item{...}{Aditional arguments for the tsboot function and the user defined pre-testing in method 2.}
}

\value{
This function returns a list with several items. 

  \item{boot.coef}{R x N matrix. Each line has the coefficients for one bootstrap sample.}
  \item{orig.coef}{Coefficients for the pre-testing applied on the original sample. }
  \item{fitted.values}{In-sample fitted values.}

}

\details{
Nothin to add.


}



\examples{
## Generates data
set.seed(12345)
N=50
p=20
T=101 
betas=runif(p-1,-1,1)

data=dgp(N=N,p=p,T=T,betas=betas)
y=data$y
X=data$X

X=cbind(embed(y,2)[,-1],X[-1,])
y=y[-1]


set.seed(123)
#Method 1
test1=bagging(y,X,R=50,l=5,pre.testing = "individual")

Method 2
test2=bagging(y,X,fn=baggit,R=50,l=5,pre.testing = "personal")

}


