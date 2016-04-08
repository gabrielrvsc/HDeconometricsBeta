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
  
  
  
  return(final.prediction)
}