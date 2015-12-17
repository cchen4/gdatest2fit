\name{ncv.lasso}
\alias{ncv.lasso}
\title{
A nested cross validation procedure for binomial responese variable and select m genes/covariates for cross validation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function requires on specifying the number folds inner loop and outer loop
it also need to includes number of m covariates in the variable selection \\
It would generate the plots of ROC curve and AUC statistics
}

\usage{
ncv.lasso(gex, k1, k2, m)
}
\arguments{
  \item{gex}{a data matrix or frame that used to fit the lasso model with more than 2 covariates}
  \item{k1}{a numerical value that is greater than 0 but less than # rows of whole data}
  \item{k2}{a numerical value that is greater than 0 but less than # rows of test data}
  \item{m}{a numerical value that is greater than 0 but less than max mumber of genes/covariates}
}
\value{
  A data matrix of response and predicted values will be returned to be used for futher analysis, such as calculating the error rate/mse. If a response is bernoulli and predicted values will be probability for logistic model. A list of non zero fitted covariates under lasso models will be printed out.
}

\author{Chongshu Chen}
\examples{
  data(subBRCA)
  status <-  c(rep(1, 785), rep(0, 230))
  testdat <- data.frame(log2(t(subBRCA)+1), status)
  cv.result <- ncv.lasso (gex=testdat, k1=3, k2=5, m=5)
}

\keyword{crossvalidation}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{glm}
\keyword{lasso}
\keyword{nested}
