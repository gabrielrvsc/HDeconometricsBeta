\name{irf}
\alias{irf}
\title{Calculates Impulse Response coefficents for an identified VAR.}
\usage{
irf(model,ident,h,unity.shock=TRUE)
}

\description{

This function calculates impulse response functions for an identified VAR fitted using the function fitvar and identified using the function indentification.

}


\arguments{

  \item{model}{VAR model estimated using the function fitvar.}

  \item{ident}{Object created by the function identification.}
  
  \item{h}{Horizon for the impulse response.}
  
  \item{unity.shock}{If TRUE, the irf is calculated for unity variation on structural shocks. If FALSE the irf is calculated for one standar deviation variation on the structural shocks.  }
  
}

\value{
This function returns a list where each element represents one variable of the VAR. Each element contains the irf results 
for an impulse on the variable tha names the object in the list. Inside each object is an h+1 x k matrix where each column is the response of each VAR variable and each row is one horizon.
}

\details{

See ?fitvar ?identification and ?boot.irf for other VAR functions.

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
ir=irf(model,ident,h,unity.shock = TRUE)
plot.irf(ir,1,2)
}
