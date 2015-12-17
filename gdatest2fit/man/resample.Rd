\name{resample}
\alias{resample}
\title{
random labeling sample id and use to create subsamples from original data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function requires on specifying the number of k samples with parameter K\\
It would generate random seeds and assign subsample id for each row of data\\
It can be used for k fold crossvalidation or random sampling from origianl data without replacement
}

\usage{
resample(data, K )
}
\arguments{
  \item{data}{a data matrix or frame that used to create subsamples}
  \item{K}{a numerical value that is greater than 0 but less than # rows of data}
}
\value{
  A vector of sample id (labels) will be created. Each row of data is belonged to each subsample. we can build a subsample of data from original data. Thus we can use the sample labels to build the dataset for cross vailidataion.
}
\author{Chongshu Chen}
\examples{
  testdat <- data.frame(matrix (rnorm(1000, 5, 2), ncol=10))
  sampleid <- resample (data=testdat, K=10)
}
\keyword{sample}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{random}
