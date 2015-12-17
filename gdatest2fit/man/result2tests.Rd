\name{result2tests}
\alias{result2tests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform row wilcoxcon test and row t test and then plot the p-values via histogram and scatter plot
}
\description{
This function performs wilcoxcon rank sum tests and ttest for each row of data matrix/frame \\
This function returns adjusted p values with FDR method and create tables of proportions of lower p-values
}
\usage{
result2tests(dmat, index)
}
\arguments{
  \item{dmat}{a class of data frame or data matrix}
  \item{index}{a vector of indicator of zero and one to distinguish the samples for column vectors. It should be consisted value of 0 and 1}
}
\value{
  a set of p-values will be returned from wilcoxcon tests and ttest\\
  the table of proportion that p values are less than .005, .05, .5 from both tests
}
\author{Chongshu Chen}
\examples{
  testdat <- matrix (rnorm(1000, 5, 2), ncol=100)
  id <- rep(c(1,0), 50)
  out <- result2tests (dmat=testdat, index=id)
}
\keyword{rowWilcox}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ttest}
\keyword{p-values}
\keyword{plot}
\keyword{histogram}

