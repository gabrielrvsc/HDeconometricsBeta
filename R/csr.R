
csr=function (y, X, K = 20, k = 4, fixed.controls = NULL) 
{
  if (length(fixed.controls) > 0) {
    w = X[, fixed.controls]
    nonw = setdiff(1:ncol(X), fixed.controls)
  }
  else {
    w = rep(0, nrow(X))
    nonw = 1:ncol(X)
  }
  save.stat = matrix(NA, ncol(X), 3)
  save.stat[fixed.controls, 3] = fixed.controls
  save.stat[fixed.controls, -3] = 0
  for (i in nonw) {
    save.stat[i, ] = c(abs(summary(lm(y ~ X[, i] + w))$coefficients[2, 
                                                                    c(3, 4)]), i)
  }
  t.ord = order(save.stat[, 1], decreasing = TRUE)
  save.stat = save.stat[t.ord, ]
  aux = setdiff(1:ncol(X), fixed.controls)
  if (K > length(aux)) {
    stop("K bigger than the number of possible variables. Choose a value for K that is equal \n         smaller than ncol(X)-length(fixed.controls)")
  }
  selected = save.stat[1:K, 3]
  aux = combn(selected, k)
  m = ncol(aux)
  final.coef = matrix(0, m, ncol(X))
  final.const = rep(0, m)
  if (length(fixed.controls) != 0) {
    for (i in 1:m) {
      model = coef(lm(y ~ w + X[, aux[, i]]))
      final.const[i] = model[1]
      model=model[-1]
      final.coef[i, fixed.controls] = model[1:length(fixed.controls)]
      final.coef[i, aux[, i]] = model[-c(1:length(fixed.controls ))]
    }
  }
  else {
    for (i in 1:m) {
      model = coef(lm(y ~ X[, aux[, i]]))
      final.const[i] = model[1]
      final.coef[i, aux[, i]] = model[-1]
    }
  }
  colnames(final.coef) = colnames(X)
  final.coef = cbind(intersect = final.const, final.coef)
  return(list(coefficients = final.coef, type = "csr"))
}
