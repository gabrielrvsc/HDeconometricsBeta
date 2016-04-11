#require(glmnet)

biclasso=function(x,y,alpha=1,display=FALSE,penalty.factor=rep(1,ncol(x))){
  
  if(is.vector(y)==FALSE){
    y=as.vector(y)
  }
  
  inf.lasso=glmnet(x=x,y=y,family="gaussian",alpha=alpha,penalty.factor=penalty.factor)
  coef=as.matrix(coef(inf.lasso))
  lambda=inf.lasso$lambda
  ncoef=inf.lasso$df
  
  yhat=matrix(NA,length(y),ncol(coef))
  for(i in 1:ncol(coef)){
    yhat[,i]=(cbind(1,x)%*%coef[,i]) 
  }
  
  residual=(yhat-y)
  mse=colMeans(residual^2)
  sse=colSums(residual^2)
  Likelihood=mse+lambda*(colSums(abs(coef)))
  nvar=ncoef+1
  
  bic=nrow(x)+nrow(x)*log(2*pi)+nrow(x)*log(mse)+log(nrow(x))*(nvar+1)
  
  sst=sum(((y-mean(y))^2))
  r2=1-(sse/sst)
  adjr2=(1-(1-r2)*(nrow(x)-1)/(nrow(x)-nvar-1))
  
  best.model=which(bic==min(bic))
  
  if(display==TRUE){
    plot(inf.lasso,xvar="lambda",label=FALSE)
    plot(log(lambda),bic,xlab="log(Lambda)",ylab="BIC")
    abline(v=log(lambda[best.model]),lty=2)
    axis(3,at=log(lambda),labels=nvar)
  }
  
  return(list("coef"=coef[,best.model],"lambda"=lambda[best.model],"bic"=bic[best.model],
              "nvar"=nvar[best.model],"r2"=r2[best.model],"adjr2"=adjr2[best.model],"glmnet-model"=inf.lasso,
              "type"="LASSO"))
  
}