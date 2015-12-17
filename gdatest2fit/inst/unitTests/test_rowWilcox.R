test_rowWilcox <- function (){

  index01 <- c(rep(1, 785), rep(0, 230))
  data(subBRCA)
  ## wilcoxcon test result
  tst <- rowWilcox(mat=log2(subBRCA+1), ind=index01)
  tst <- as.vector(tst)
  checkTrue(is.vector(tst))
}

