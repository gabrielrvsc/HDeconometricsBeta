
#require(metRology)
#require(Matrix)

dgp=function(N,p,T,rho=0.5,betas=rep(1,p-1),innovations="garch",dfg=5,g.par=c(5e-4,0.9,0.05),
             var.error="student.t",dfv=5,size.a.blk=5,a1.par=0.15,a4.par=-0.1){

  if(innovations=="garch"){
    error.garch=rt.scaled(T+100,df=dfg)
    h=rep(0,(T+100))
    for(t in 2:(T+100)){
      h[t]=g.par[1]+g.par[2]*h[t-1]+g.par[3]*h[t-1]*error.garch[t-1]^2
    }
    error.y=sqrt(h)*error.garch
  }else{
    error.y=rnorm(T+100,0,dfg)
  }

  if(var.error=="student.t"){
    erro.var=matrix(rt.scaled((T+100)*(N-1),df=dfv),T+100,N-1)
  }else{
    erro.var=matrix(rnorm((T+100)*(N-1),0,dfv),T+100,N-1)
  }


  aux=ceiling(N/size.a.blk)
  A=matrix(1,size.a.blk,size.a.blk)
  aux1=list()
  for(i in 1:aux){
    aux1[[i]]=A
  }
  A=as.matrix(bdiag(aux1))
  A1=a1.par*A[1:(N-1),1:(N-1)]
  A4=a4.par*A[1:(N-1),1:(N-1)]


  X=matrix(0,T+100,N-1)
  for(i in 5:(T+100)){
    X[i,]=A1%*%X[i-1,]+A4%*%X[i-4,]+erro.var[i,]
  }

  y=rep(0,T+100)
  for(i in 2:(T+100)){
    y[i]=rho*y[i-1]+t(betas)%*%X[i,1:(p-1)]+error.y[i]
  }

  y=y[-c(1:100)]
  X=X[-c(1:100),]
  return(list("y"=y,"X"=X))
}
