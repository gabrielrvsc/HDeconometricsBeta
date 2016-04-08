
csr=function(y,X,K=20,k=4,fixed.controls=NULL){
  
  save.stat=matrix(NA,ncol(X),3)
  for(i in 1:ncol(X)){
    save.stat[i,]=c(abs(summary(lm(y~ X[,i]))$coefficients[2,c(3,4)]),i)
  }
  
  t.ord=order(save.stat[,1],decreasing=TRUE)
  save.stat=save.stat[t.ord,]
  selected=save.stat[1:K,3]

  aux=combn(selected,k)

  m=ncol(aux)
  final.coef=matrix(0,m,ncol(X))
  final.const=rep(0,m)
  for(i in 1:m){
    model=coef(lm(y~X[,aux[,i]]))
    final.const[i]=model[1]
    final.coef[i,aux[,i]]=model[-1]
  }
  colnames(final.coef)=colnames(X)
  final.coef=cbind("intersect"=final.const,final.coef)
  return(list("coefficients"=final.coef,"type"="csr"))
}

