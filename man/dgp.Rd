\name{dgp}
\alias{dgp}
\title{Generates Data from Medeiros and Mendes (2016) ARDL process.}
\usage{
dgp(N,p,T,rho=0.5,betas=rep(1,p-1),innovations=c("garch","simple"),dfg=5,g.par=c(5e-4,0.9,0.05),
             var.error=c("student.t","gaussian"),dfv=5,size.a.blk=5,a1.par=0.15,a4.par=-0.1)
}
\description{
Generates data from the ARDL described in the Monte Carlo experiment from Medeiros and Mendes (2016).
}

\arguments{
  \item{N}{Number of variables in the system.}

  \item{p}{Number smaller than N, which accounts for the number of variables that are relevant to the process y.}
  
  \item{T}{length of the time-series.}
  \item{rho}{Value of the autorregressive parameter of the process y.}
  \item{betas}{A (p-1) dimensional vector with the parameters of the other relevant variables except the autorregressive.}
  \item{innovations}{If equals "garch" the y error will be a garch(1,1) process with standardized t distribution. If equals "simple" the errors are gaussian.}
  \item{dfg}{For "inovations="garch"" this variable defines the degree of freedom of the standardized t. For "inovations="garch"" dfg is the standard deviation of the gaussian distribution. }
  \item{g.par}{Vector with the three parameters of the GARCH(1,1) process.}
  \item{var.error}{Distribution of the VAR errors.}
  \item{dfv}{Degrees of freedom if "var.error="student.t"" and standard deviation if "var.error="gaussian"". }
  \item{size.a.blk}{Size of the blocks in the block-diagonal A1 and A4 matrix.}
  \item{a1.par}{Value of the nonzero elements in the A1 matrix}
  \item{a2.par}{Value of the nonzero elements in the A2 matrix}
}

\value{
A list of two objects.

  \item{y}{Dependent variable in the ARDL.}
  \item{X}{Txp-1 Matrix with all independent variables. Note that the autorregressive term is not on matrix X. X has only exogenous variables.}

}

\details{
This function generates data from:

y_t=rho * y_{t-1} + beta' * x_t + e

The error e may be gaussian or have the following process:

e=sqrt(h_t) * v_t , where v_t is student-t 

h_t=a + b1 * h_{t-1} + b2 * e^2_{t-1}


x is generated from a VAR process:

x_t = A1*x_{t-1} + A4*x_{t-4} + w, w is gaussian or t.

A1 and A4 are block diagonal.

}


\references{
Medeiros, M. C., & Mendes, E. F. (2016). ℓ1-regularization of high-dimensional time-series models with non-Gaussian and heteroskedastic errors. Journal of Econometrics, 191(1), 255-271.
}

\examples{
test=dgp(N=100,p=20,T=200)
plot(test$y,type="l")
matplot(test$X,type="l")
}


