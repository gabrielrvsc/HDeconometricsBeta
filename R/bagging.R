
bagging=function(y,X,fn=NULL,R=100,l=5,sim="fixed",pre.testing="joint",fixed.controls=NULL,...){
  
  tsd=cbind(y,X)
  if(pre.testing=="personal"){
    bag=tsboot(tseries=tsd,statistic=fn,R=R,l=l
               ,sim=sim, ...)
  }else{
    bag=tsboot(tseries=tsd,statistic=baggit,R=R,l=l
               ,sim=sim, pre.testing=pre.testing, fixed.controls=fixed.controls,... )
  }
  
  original=bag$t0
  bootres=bag$t
  colnames(bootres)=names(original)
  
  yhat=rowMeans(cbind(1,X)%*%t(bootres),na.rm=TRUE)
  
  
  return(list("boot.coef"=bootres,"orig.coef"=original,"fitted.values"=yhat))
}