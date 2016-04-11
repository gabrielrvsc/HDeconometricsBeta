\name{predict.HDeconometrics}
\alias{predict.HDeconometrics}
\title{Predict method for functions in the HDeconometrics package.}
\usage{
predict.HDeconometrics(model,newdata)
}


\arguments{
  \item{model}{model object from HDeconometrics package.}

  \item{newdata}{Data used for prediction. If the data is the same used to fit the model, the result is the fitted value.}
}


\references{
Friedman, Jerome, Trevor Hastie, and Robert Tibshirani. "glmnet: Lasso and elastic-net regularized generalized linear models. R package version 1.9–5." (2013).


}

\examples{

data(brinf)
y=brinf[,1]
X=brinf[,-1]

boostest=boosting(y,X,v=0.2)
plot(y,type="l")
lines(predict.HDeconometrics(boostest,X),col="red")

}


