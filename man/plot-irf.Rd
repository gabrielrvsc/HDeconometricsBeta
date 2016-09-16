\name{plot.irf}
\alias{plot.irf}
\title{Plots the impulse response from a irf or boot.irf object.}
\usage{
plot.irf(ir,impulse,response,ci=c(0.025,0.975),ylab=NULL,ylim=c(-1,1),center=c("median","mean","real"))
}


\arguments{

  \item{ir}{irf or boot.irf object.}

  \item{impulse}{Impulse variable position on the VAR.}
  
  \item{response}{Response variable position on the VAR.}
  
  \item{ci}{Confidence interval.}
  
  \item{center}{The central line of the plotted irf. If median or mean, these statistics are calculated from the bootstrap results. If real, the irf over the real data is used.}
  
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
