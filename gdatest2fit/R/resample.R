### label the ID for each row of data.frame
resample <- function (data, K)
{
  call <- match.call()
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- nrow(data)
  if ((K > n) || (K <= 1))
    stop("'K' outside allowable range")
  K.o <- K
  K <- round(K)
  kvals <- unique(round(n/(1L:floor(n/2))))
  temp <- abs(kvals - K)
  if (!any(temp == 0))
    K <- kvals[temp == min(temp)][1L]
  if (K != K.o)
    warning(gettextf("'K' has been set to %f", K), domain = NA)
  f <- ceiling(n/K)
  s <- sample(rep(1L:K, f), n)
  return(s)
 }
