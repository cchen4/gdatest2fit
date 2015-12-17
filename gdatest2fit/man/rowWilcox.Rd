\name{rowWilcox}
\alias{rowWilcox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform row wilcoxon test to return a set of p-values
}
\description{
This function performs wilcoxcon rank sum test for each row of data matrix/frame and return a p value
}
\usage{
rowWilcox(mat, ind)
}
\arguments{
  \item{mat}{a class of data frame or data matrix}
  \item{ind}{a vector of indicator of zero and one to distinguish the samples for column vectors. It should be consisted value of 0 and 1}
}
\value{
  a set of p-values will be returned from wilcoxcon tests
}
\author{Chongshu Chen}
\examples{
  testdat <- matrix (rnorm(1000, 5, 2), ncol=100)
  id <- rep(c(1,0), 50)
  rowWilcox (mat=testdat, ind=id)
}
\keyword{rowWilcox}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{wilcoxon}
\keyword{non parametric}% __ONLY ONE__ keyword per line


