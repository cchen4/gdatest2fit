test_result2tests <- function (){

  index01 <- c(rep(1, 785), rep(0, 230))
  data(subBRCA)
  ### ttest and wilcoxcon test result
  tst <- result2tests(dmat=log2(subBRCA+1), index=index01)
  checkTrue(is.list(tst))

  }
