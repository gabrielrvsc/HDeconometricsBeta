
lbvar=function(Y,p=1,delta=0,lambda=0.05){
  aux=embed(Y,p+1)
  N=ncol(Y)
  
  Yreg=aux[,1:N]
  Xreg=aux[,-c(1:N)]
  
  Sig=cov(Yreg)
  sig=sqrt(diag(Sig))
  
  # ====== build Yd e Xd =======#
  
  # Yd
  aux1=delta*diag(sig,N)/lambda
  aux2=matrix(0,N*(p-1),N)
  aux3=diag(sig,N)
  aux4=rep(0,N)
  Yd=rbind(aux1,aux2,aux3,aux4)
  
  # Xd
  aux1=diag(1:p,p)
  aux2=diag(sig,N)/lambda
  aux3=kronecker(aux1,aux2)
  
  aux4=matrix(0,N,N*p)
  aux5=rep(0,N*p)
  
  aux6=rbind(aux3,aux4,aux5)
  Xd=cbind(c(rep(0,nrow(aux6)-1),0.1),aux6)
  
  # === Estimation === #
  Ystar=rbind(Yd,Yreg)
  Xstar=rbind(Xd,cbind(1,Xreg))
  
  betas=solve(t(Xstar)%*%Xstar)%*%t(Xstar)%*%Ystar
  
  fitted=cbind(1,Xreg)%*%betas
  
  sigmae=(1/nrow(Ystar))*t(Ystar-cbind(Xstar)%*%betas)%*%(Ystar-cbind(Xstar)%*%betas)
  
  #plot(Yreg[,1],type="l")
  #lines(fitted[,1],col=2)
  return(list("betas"=betas,"fitted"=fitted,"Y"=Y,"p"=p,"covmat"=sigmae,"type"="lbvar"))
}

