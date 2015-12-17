test_resample<- function (){

  testdat <- data.frame(matrix (rnorm(1000, 5, 2), ncol=10))
  ### ttest and wilcoxcon test result
  tst <- resample(data=testdat, K=10)

  checkEquals(max(tst), 10)

}


