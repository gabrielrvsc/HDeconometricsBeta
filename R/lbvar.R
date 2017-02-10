lbvar=function (Y, p = 1, delta = 0, lambda = 0.05, xreg = NULL) 
{
  if (!is.matrix(Y)) {
    Y = as.matrix(Y)
  }
  if (length(xreg) != 0) {
    if (!is.matrix(xreg)) {
      xreg = as.matrix(xreg)
    }
  }
  aux = embed(Y, p + 1)
  N = ncol(Y)
  Yreg = aux[, 1:N]
  Xreg = aux[, -c(1:N)]
  Sig = cov(Yreg)
  sig = sqrt(diag(Sig))
  aux1 = delta * diag(sig, N)/lambda
  aux2 = matrix(0, N * (p - 1), N)
  aux3 = diag(sig, N)
  aux4 = rep(0, N)
  Yd = rbind(aux1, aux2, aux3, aux4)
  aux1 = diag(1:p, p)
  aux2 = diag(sig, N)/lambda
  aux3 = kronecker(aux1, aux2)
  aux4 = matrix(0, N, N * p)
  aux5 = rep(0, N * p)
  aux6 = rbind(aux3, aux4, aux5)
  if (length(xreg) > 0) {
    aux7 = matrix(0, nrow(aux6), ncol(xreg))
    aux6 = cbind(aux6, aux7)
    Xreg = cbind(Xreg, tail(xreg, nrow(Xreg)))
  }
  Xd = cbind(c(rep(0, nrow(aux6) - 1), 0.1), aux6)
  Ystar = rbind(Yd, Yreg)
  Xstar = rbind(Xd, cbind(1, Xreg))
  #betas = solve(t(Xstar) %*% Xstar) %*% t(Xstar) %*% Ystar
  betas=coef(lm(Ystar~-1+Xstar))
  fitted = cbind(1, Xreg) %*% betas
  sigmae = (1/nrow(Ystar)) * t(Ystar - cbind(Xstar) %*% betas) %*% 
    (Ystar - cbind(Xstar) %*% betas)
  coef.by.equation = t(betas)
  coef.by.block = list(intersect = coef.by.equation[, 1])
  coef.by.equation = coef.by.equation[, -1]
  for (i in 1:p) {
    coef.by.block[[i + 1]] = coef.by.equation[, (ncol(Y) * 
                                                   i - ncol(Y) + 1):(ncol(Y) * i)]
  }
  if (length(xreg) != 0) {
    aux = ncol(coef.by.equation)
    coef.by.block[[length(coef.by.block) + 1]] = coef.by.equation[, 
                                                                  (aux - ncol(xreg) + 1):aux]
  }
  
  residuals=tail(Y,nrow(fitted))-fitted
  
  return(list(coef.by.equation = t(betas), coef.by.block = coef.by.block, 
              fitted = fitted,residuals=residuals, Y = Y, p = p, covmat = sigmae, type = "var", 
              xreg = xreg, Ts = c(T = nrow(Xreg), Td = nrow(Xd), Tstar = nrow(Xstar))))
}
