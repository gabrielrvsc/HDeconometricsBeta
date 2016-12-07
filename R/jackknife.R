
jackknife=function(X,y,lag=4,fixed.controls=NULL){
  
  if(length(fixed.controls)!=0){
    fixed=Reduce("cbind",lapply(X,function(x)x[,fixed.controls]))
    fixed=as.matrix(fixed)
    X=lapply(X,function(x)x[,-fixed.controls])
  }
  
  N=ncol(X[[1]])
  T=nrow(X[[1]])
  errosl1o=matrix(NA,T,N)
  
  aux=matrix(NA,lag,N)
  Xaux=lapply(X,function(x)rbind(aux,x,aux))
  yaux=c(rep(NA,lag),y,rep(NA,lag))
  fixed.aux=rbind(matrix(NA,lag,ncol(fixed)),fixed,matrix(NA,lag,ncol(fixed)))
  for(j in 1:T){
    
    X1=lapply(Xaux,function(x)x[-c(j:(j+2*lag)),]); X2=lapply(Xaux,function(x)x[j+lag,])
    y1=yaux[-c(j:(j+2*lag))]; y2=yaux[j+lag]
    
    if(length(fixed.controls)!=0){
      f1=fixed.aux[-c(j:(j+2*lag)),]; f2=fixed.aux[j+lag,]
    }
    #estimando regressoes
    
    residos=matrix(NA,length(y1),N)
    erroprev=rep(NA,N)
    for(i in 1:N){
      aux=Reduce("cbind",lapply(X1,function(x)x[,i]))
      aux2=Reduce("c",lapply(X2,function(x)x[i]))
      if(length(fixed.controls)!=0){
        model=lm(y1 ~ f1 + aux)
        residos[,i]=y1-cbind(1,f1,aux)%*%coef(model)
        jprev=t(c(1,f2,aux2))%*%coef(model)
      }else{
        model=lm(y1 ~ aux)
        residos[,i]=y1-cbind(1,aux)%*%coef(model)
        jprev=t(c(1,aux2))%*%coef(model)
      }

      erroprev[i]=y2-jprev
    }
    errosl1o[j,]=erroprev
  }
  
  
  #otimizando
  amat=rbind(rep(1,N),rep(1,N))
  bvec=c(1,1)
  dvec=rep(0,N)
  dmat=t(errosl1o)%*%errosl1o
  
  optm=solve.QP(Dmat=dmat,dvec=dvec,Amat=t(amat),bvec=bvec)
  
  if(length(fixed.controls)!=0){
    save.coef=matrix(NA,N,lag+1+ncol(fixed)) 
  }else{
    save.coef=matrix(NA,N,lag+1)  
  }
  
  for(i in 1:N){
    aux=Reduce("cbind",lapply(Xaux,function(x)x[,i]))
    if(length(fixed.controls)!=0){
      model=lm(yaux ~ fixed.aux + aux)
    }else{
      model=lm(yaux ~ aux)
    }
    save.coef[i,]=coef(model)
  }
  
  
  return(list("coef"=save.coef,"weights"=optm$solution,"type"="jackknife","fixed.controls"=fixed.controls,"lag"=lag,"N"=N))
}
