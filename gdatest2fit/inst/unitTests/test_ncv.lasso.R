test_ncv.lasso <- function(){

  status <- c(rep(1, 785), rep(0, 230))
  subBRCA1 <- matrix(rnorm(101500,50,5), ncol=1015)
  data.glm <- data.frame(log2(t(subBRCA1)+1), status)
  ### ttest and wilcoxcon test result
  tst <- ncv.lasso(gex=data.glm, k1=3, k2=5, m=10)
  checkTrue(is.list(tst))

}
