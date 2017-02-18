predict.HDeconometrics=function (model, newdata=NULL, h=1) 
{
 if (model$type == "var") {
    
    if(is.vector(newdata)){
      newdata=matrix(newdata,nrow=1)
    }
    
    p = model$p
    b = t(model$coef.by.equation)
    aux = embed(model$Y, p)
    aux = aux[nrow(aux), ]
    N = ncol(model$Y)
    prev.store = matrix(NA, h, N)
    exog=length(model$xreg)
    
    for (i in 1:h) {
      if(exog>0){
        prev = c(1, aux,newdata[i,]) %*% b
      }else{
        prev = c(1, aux) %*% b
      }
      prev.store[i, ] = prev
      aux = c(prev, head(aux, length(aux) - N))
    }
    final.prediction = prev.store
    colnames(final.prediction) = colnames(model$Y)
  }
  
  
  if (model$type == "csr") {
    parameters = model$coefficients
    if (is.vector(newdata)) {
      individual.prediction = c(1, newdata) %*% t(parameters)
      final.prediction = mean(individual.prediction)
    }
    else {
      individual.prediction = cbind(1, newdata) %*% t(parameters)
      final.prediction = rowMeans(individual.prediction)
    }
  }
  if (model$type == "LASSO") {
    parameters = model$coef
    if (is.vector(newdata)) {
      final.prediction = c(1, newdata) %*% parameters
    }
    else {
      final.prediction = cbind(1, newdata) %*% parameters
    }
  }
  if (model$type == "bagging") {
    parameters = model$boot.coef
    parameters[is.na(parameters)]=0
    if (is.vector(newdata)) {
      individual.prediction = c(1, newdata) %*% t(parameters)
      final.prediction = mean(individual.prediction)
    }
    else {
      individual.prediction = cbind(1, newdata) %*% t(parameters)
      final.prediction = rowMeans(individual.prediction)
    }
  }
  if (model$type == "boosting") {
    parameters = model$coef
    ybar = mean(model$y)
    if (is.vector(newdata)) {
      final.prediction = ybar + newdata %*% parameters
    }
    else {
      final.prediction = ybar + newdata %*% parameters
    }
  }
 
 if(model$type=="jackknife"){
  
  if(is.matrix(newdata[[1]])==FALSE){
    newdata=lapply(newdata,function(x)matrix(x,nrow=1))
  }
  
  w=model$weights; coef=model$coef; fixed.controls=model$fixed.controls
  lag=model$lag; N=model$N
  pred=matrix(NA,nrow(newdata[[1]]),N)
  if(length(fixed.controls)==0){
    
    for(i in 1:N){
      aux=Reduce("cbind",lapply(newdata,function(x)x[,i]))
      pred[,i]=(cbind(1,aux)%*%coef[i,])*w[i]
    }
  }else{

    fixed=Reduce("cbind",lapply(newdata,function(x)x[,fixed.controls]))
     fixed=as.vector(fixed)
  fixed = t(as.matrix(fixed))
    newdata=lapply(newdata,function(x)x[,-fixed.controls])
    
    if(is.matrix(newdata[[1]])==FALSE){
      newdata=lapply(newdata,function(x)matrix(x,nrow=1))
    }
    
    for(i in 1:N){
      aux=Reduce("cbind",lapply(newdata,function(x)x[,i]))
      pred[,i]=(cbind(1,fixed,aux)%*%coef[i,])*w[i]
    }
  }
  final.prediction=rowSums(pred)
  
  return(final.prediction)
}

  return(final.prediction)
}
