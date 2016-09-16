\name{boot.irf}
\alias{boot.irf}
\title{Calculates Impulse Response coefficents for an identified VAR with bootstrap intervals.}
\usage{
irf(model,ident,h,M.unity.shock=TRUE,type=c("ols","lasso","adalasso"))
}

\description{

This function calculates impulse response functions for an identified VAR fitted using the function fitvar and identified using the function indentification.
It also calculates bootstrap densities for the impulse responses by resampling the structural shocks and rebuild the series. 

}


\arguments{

  \item{model}{VAR model estimated using the function fitvar.}

  \item{ident}{Object created by the function identification.}
  
  \item{h}{Horizon for the impulse response.}
  
  \item{M}{Number of bootstrap replications.}
  
  \item{unity.shock}{If TRUE, the irf is calculated for unity variation on structural shocks. If FALSE the irf is calculated for one standar deviation variation on the structural shocks.  }
  
  \item{type}{Model to be used to calculate the bootstrapped irfs. Implemented for "ols","lasso", "adalasso".}
  
}

\value{
This function returns a list where each element represents one impulse variable of the VAR. Inside these lists are other lists containing the responses on each variable, which is a h+1 x M matrix.

\details{

See ?fitvar ?identification and ?irf for other VAR functions.

}


\examples{

## = generate data = ##
dados=dgp(100,20,200)
Y=dados$X[,1:4]
p=3 # var order

# estimation and identification #
model=fitvar(Y,p,type="ols")
ident=identification(model)

h=48
ir=boot.irf(model,ident,h,200,type="ols")
plot.irf(ir,1,2)
}
