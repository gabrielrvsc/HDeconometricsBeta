identification=function(model){
  covmat_e=model$covmat
  choles=chol(covmat_e)
  covmat_u_sqrt=diag(diag(choles))
  
  A=solve(t(solve(covmat_u_sqrt)%*%choles))
  sigma2u=covmat_u_sqrt^2
  colnames(A)=colnames(sigma2u)=rownames(sigma2u)=rownames(A)=rownames(covmat_e)
  return(list("A"=A,"sigma2u"=sigma2u))
}
