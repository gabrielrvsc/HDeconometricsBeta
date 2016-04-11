boosting=function(y,X,v=0.2,minIt=ncol(X)/2,maxIt=10*ncol(X),display=TRUE,crit.break=TRUE){
  
  phi=rep(mean(y),length(y))
  B=rep(0,ncol(X))
  BB=(1/nrow(X))*matrix(1,nrow(X),nrow(X))
  df.final=Inf
  M=maxIt
  save.B=matrix(NA,ncol(X),maxIt)
  save.crit=rep(Inf,M)
  
  for(m in 1:M){
    
    u=y-phi
    b=rep(NA,ncol(X))
    e=matrix(NA,nrow(X),ncol(X))
    ssr=rep(NA,ncol(X))
    for(i in 1:ncol(X)){
      b[i]=solve(t(X[,i])%*%X[,i])%*%(t(X[,i])%*%u)
      mod=lm(u~-1+X[,i])
      b[i]=coef(mod)
      e[,i]=u-X[,i]%*%matrix(b[i])
      ssr[i]=t(e[,i])%*%e[,i]
    }
    best.ssr=which(ssr==min(ssr))
    best.coef=b[best.ssr]
    
    phi=phi+v*X[,best.ssr]*best.coef
    aux=rep(0,ncol(X))
    aux[best.ssr]=best.coef
    B=B+v*aux
    
    BB=BB+v*X[,best.ssr]%*%solve(t(X[,best.ssr])
                                 %*%X[,best.ssr])%*%t(X[,best.ssr])%*%(diag(1,nrow(X))-BB)
    df=sum(diag(BB))
    crit=log(sum((y-phi)^2)) + (log(nrow(X))*df)/nrow(X)
    
    if(crit<min(save.crit,na.rm = TRUE)){
      df.final=df
    }
    
    save.crit[m]=crit
    
    save.B[,m]=B
    if(crit.break==TRUE){
      if(m>=minIt){
        if(min(save.crit[1:(m/2)])<crit){
          break
        }
      }
    }
    if(display==TRUE){
      cat(m,"\n")
    }
  }
  
  coef.final=save.B[,which(save.crit==min(save.crit,na.rm=TRUE))] 
  names(coef.final)=colnames(X)
  fitted=mean(y)+X%*%coef.final
  save.crit=save.crit[1:m]
  return(list("coef"=coef.final,"fitted.values"=fitted,"best.crit"=min(save.crit,na.rm=TRUE),"all.crit"=save.crit,"df"=df.final,"type"="boosting","y"=y))
}