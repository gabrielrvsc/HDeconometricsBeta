plot.irf=function(ir,impulse,response,ci=c(0.025,0.975),ylab=NULL,ylim=c(-1,1),center="median"){
  if(length(ir$density)==0){
    irp=ir
    if(length(ylab)==0){
      ylab=paste("Response on",names(irp)[impulse]," from impulse on",names(irp)[response],sep=" ")
    }
    real.ir=irp[[impulse]][,response]
    plot(real.ir,type="l",ylim=ylim,xlab="Horizon",ylab=ylab)
    abline(h=0,col="red",lty=2)
    grid()
  }else{
    irb=ir$density
    irp=ir$point.irf
    M=ncol(irb[[1]][[1]])
    I=round(quantile(1:M,probs=ci))
    med=round(quantile(1:M,probs=0.5))
    
    median.ir=irb[[impulse]][[response]][,med]
    mean.ir=rowMeans(irb[[impulse]][[response]])
    real.ir=irp[[impulse]][,response]
    interval.ir=irb[[impulse]][[response]][,I]
    
    if(length(ylab)==0){
      ylab=paste("Response on",names(irb)[impulse]," from impulse on",names(irb)[response],sep=" ")
    }
    
    if(center=="median"){
      plot(median.ir,type="l",ylim=ylim,xlab="Horizon",ylab=ylab)
      lines(interval.ir[,1],col=4,lty=2)
      lines(interval.ir[,2],col=4,lty=2)
      abline(h=0,col="red",lty=2)
      grid()
    }
    
    if(center=="mean"){
      plot(mean.ir,type="l",ylim=ylim,xlab="Horizon",ylab=ylab)
      lines(interval.ir[,1],col=4,lty=2)
      lines(interval.ir[,2],col=4,lty=2)
      abline(h=0,col="red",lty=2)
      grid()
    }
    
    if(center=="real"){
      plot(real.ir,type="l",ylim=ylim,xlab="Horizon",ylab=ylab)
      lines(interval.ir[,1],col=4,lty=2)
      lines(interval.ir[,2],col=4,lty=2)
      abline(h=0,col="red",lty=2)
      grid()
    }
  }
}
