predict.HDeconometrics=function(model,newdata){
  if(model$type=="csr"){
    parameters=model$coefficients
    if(is.vector(newdata)){
      individual.prediction=c(1,newdata)%*%t(parameters)
      final.prediction=mean(individual.prediction)
    }else{
      individual.prediction=cbind(1,newdata)%*%t(parameters)
      final.prediction=rowMeans(individual.prediction)
    }
  }  

  if(model$type=="LASSO"){
    parameters=model$coef
    if(is.vector(newdata)){
      final.prediction=c(1,newdata)%*%parameters
    }else{
      final.prediction=cbind(1,newdata)%*%parameters
    }
  }  

  if(model$type=="bagging"){
    parameters=model$boot.coef
    if(is.vector(newdata)){
      individual.prediction=c(1,newdata)%*%t(parameters)
      final.prediction=mean(individual.prediction)
    }else{
      individual.prediction=cbind(1,newdata)%*%t(parameters)
      final.prediction=rowMeans(individual.prediction)
    }
  }  
  
  if(model$type=="boosting"){
    parameters=model$coef
    ybar=mean(model$y)
    if(is.vector(newdata)){
      final.prediction=ybar+newdata%*%parameters
    }else{
      final.prediction=ybar+newdata%*%parameters
    }
  }  
    
  return(final.prediction)
}