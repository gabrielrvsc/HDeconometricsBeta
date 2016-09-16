\name{identification}
\alias{identificatiob}
\title{Recursive VAR identification using Cholesky decomposition.}
\usage{
identification(model){
}

\description{

This function uses recursive identification to calculate the contemporaneous matrix of a structural VAR system using Cholesky decomposition on the covariance matrix of the residuals.
Variables should be ordered from the most exogenous to the most endogenous.


}


\arguments{

  \item{model}{object created from the fitvar function.}
}

\value{
This function returns a list with several items. 

  \item{A}{k x k matrix of contemporaneous impact from the structural VAR.}
  \item{sigma2u}{k x k structural shocks covariance matrix.}
}

\details{

See ?fitvar ?irf and ?boot.irf for other VAR functions.

}


\examples{

## = generate data = ##
dados=dgp(100,20,200)
Y=dados$X[,1:4]
p=3 # var order

# = fit the var = #
model=fitvar(Y,p,type="ols")

# = identification = #
ident=identification(model)
ident$A
ident$sigma2u

}
