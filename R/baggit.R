
baggit=function(mat,pre.testing="joint",fixed.controls=NULL){
  
  y=mat[,1]
  X=mat[,-1]
  
  if(pre.testing=="joint"){
    
    if(nrow(X)<ncol(X)){
      stop("Error: Type = joint is only for data with more observations than variables")
    }
    
    m1=lm(y~X)
    t1=summary(m1)$coefficients[-1,3]
    s1=which(abs(t1)>1.95)
    if(length(s1)==0){
      stop("Error: The pre-testing excluded all variables","/n")
    }
    
  }
  
  
  if(pre.testing=="individual"){
    
    if(length(fixed.controls)>0){
      w=X[,fixed.controls]
      nonw=setdiff(1:ncol(X),fixed.controls)
    }else{
      w=rep(0,nrow(X))
      nonw=1:ncol(X)
    }
    
    store.t=rep(NA,ncol(X))
    store.t[fixed.controls]=Inf
    
    for(i in nonw){
      m1=lm(y~X[,i]+w)
      t1=summary(m1)$coefficients[2,3]
      store.t[i]=t1
    }
    
    s1=which(abs(store.t)>1.96)
    
    
  }
  
  if(length(s1)>nrow(X)){
    stop("Error: The pre-testing was not able to reduce the dimension to N<T")
  }
  
  m2=lm(y~X[,s1])
  
  final.coef=rep(0,ncol(X))
  final.coef[s1]=coef(m2)[-1]
  names(final.coef)=colnames(X)
  final.coef=c(coef(m2)[1],final.coef)
  return(final.coef)
}
