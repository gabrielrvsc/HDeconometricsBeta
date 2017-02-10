fitvar=function(Y,p,m.type="ols",penalty.factor=matrix(1,ncol(Y),ncol(Y)*p+ max(ncol(xreg),0)),xreg=NULL){
  
  
  if(length(colnames(Y))<ncol(Y)){
    aux=seq(1:ncol(Y))
    aux=paste("V",aux,sep="")
    colnames(Y)=aux
    cat("Variables were supplied with no colnames. Automatic names were supplied.")
  }
  Y1=embed(as.matrix(Y),p+1)
  colnames(Y1)=rep(colnames(Y),p+1)
  if(length(xreg)!=0){
    xreg=tail(xreg,nrow(Y1))
  }
  save.coef=matrix(NA,ncol(Y),ncol(Y)*p+1+max(ncol(xreg),0))
  
  if(m.type=="ols"){
    for(i in 1:ncol(Y)){
      
      ols=lm(Y1[,i]~Y1[,-c(1:ncol(Y))]+xreg)
      save.coef[i,]=coef(ols)
      
    }
  }
  if(m.type=="lasso"){
    for(i in 1:ncol(Y)){
      
      lasso=biclasso(y=Y1[,i], x=cbind(Y1[,-c(1:ncol(Y))],xreg), display=FALSE,penalty.factor = penalty.factor[i,])
      save.coef[i,]=lasso$coef
      
    }
  }
  colnames(save.coef)=c("intersect",colnames(Y1)[-c(1:ncol(Y))],colnames(xreg))
  
  coef.by.equation=save.coef
  coef.by.block=list("intersect"=save.coef[,1])
  save.coef=save.coef[,-1]
  for(i in 1:p){
    coef.by.block[[i+1]]=save.coef[,(ncol(Y)*i-ncol(Y)+1):(ncol(Y)*i)]
  }
  
  if (length(xreg) != 0) {
    aux = ncol(coef.by.equation)
    coef.by.block[[length(coef.by.block) + 1]] = coef.by.equation[, 
                                                                  (aux - ncol(xreg) + 1):aux]
    fitted=cbind(1,Y1[,-c(1:ncol(Y))],xreg)%*%t(coef.by.equation)
  }else{
    fitted=cbind(1,Y1[,-c(1:ncol(Y))])%*%t(coef.by.equation)
  }
  

  
  residuals=Y1[,c(1:ncol(Y))]-fitted
  
  
  covmatrix=(t(residuals)%*%residuals)/(nrow(Y1)-ncol(Y1))
  
  return(list("coef.by.equation"=coef.by.equation,"coef.by.block"=coef.by.block,"fitted"=fitted,
              "residuals"=residuals,"N"=nrow(Y1),"p"=p,"covmat"=covmatrix,"Y"=Y,"m.type"=m.type,type="var","xreg"=xreg))
}
