test_cv.f<- function ()
  {
  status <- c(rep(1, 785), rep(0, 230))
  data(subBRCA)
  data.glm <- data.frame(log2(t(subBRCA)+1), status)
  ### ttest and wilcoxcon test result
  tst <- cv.f(dat=data.glm, K=5, m=20)

  checkTrue(is.list(tst))
}
