
irf=function(model,ident,h,unity.shock=TRUE){
  p=model$p
  k=ncol(model$fitted)
  J=matrix(0,k,k*p)
  diag(J[1:k,1:k])=1
  
  aux=diag(k)
  if(unity.shock==FALSE){
    aux=aux%*%sqrt(ident$sigma2u)
  }
  phi0=solve(ident$A)%*%aux
  
  A=unlist(model$coef.by.block[2:(p+1)])
  
  companion <- matrix(0, nrow = k * p, ncol = k * p)
  companion[1:k, 1:(k * p)] = A
  if (p > 1) {
    j <- 0
    for (i in (k + 1):(k * p)) {
      j <- j + 1
      companion[i, j] <- 1
    }
  }
  store.phi=list()
  store.phi[[1]]=diag(k)
  aux=companion
  for(i in 2:(h+1)){
    store.phi[[i]]=J%*%aux%*%t(J)
    aux=companion%*%aux
  }
  irmat=lapply(store.phi,function(x) x %*% phi0)
  
  aux=matrix(NA,h+1,k)
  colnames(aux)=colnames(model$Y)
  ir=list()
  for(i in 1:k){
    ir[[i]]=aux
  }
  
  for(i in 1:k){
    for(j in 1:length(irmat)){
      ir[[i]][j,]=irmat[[j]][,i]
    }
  }
  names(ir)=colnames(model$Y)
  return(ir)
}
