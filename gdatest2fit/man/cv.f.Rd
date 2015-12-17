\name{cv.f}
\alias{cv.f}
\title{
A cross validation procedure for binomial responese variable and select m genes/covariates for cross validation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function requires on specifying the number of k folds and m covariates \\
It would generate the plots of ROC curve and AUC statistics
}

\usage{
cv.f(dat, K, m)
}
\arguments{
  \item{dat}{a data matrix or frame that used to fit the glm/logit model with more than 2 covariates}
  \item{K}{a numerical value that is greater than 0 but less than # rows of data}
  \item{m}{a numerical value that is greater than 0 but can not be too large otherwise perfect seperation may occur}
}
\value{
  A data matrix of response and predicted values will be returned to be used for futher analysis, such as calculating the error rate/mse. If a response is bernoulli and predicted values will be probability for logistic model. A index of m fitted covariates will be returned and thus we can use the index to fit the model with a whole data.
}
\author{Chongshu Chen}
\examples{
  subBRCA1 <- matrix(rnorm(101500,50,5), ncol=1015)
  status <-  c(rep(1, 785), rep(0, 230))
  testdat <- data.frame(log2(t(subBRCA1)+1), status)
  cv.result <- cv.f (dat=testdat, K=5, m=50)
}
\keyword{crossvalidation}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{glm}
