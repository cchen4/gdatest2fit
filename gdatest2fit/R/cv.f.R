### nested gene selections wilcoxcon test in cross validation procedure
cv.f <- function(dat, K, m){
  ### assigning the id to each row of data to be used for create subsamples
  sid <- resample (data=dat, K=K) ### 10 folds CV
  c <- ncol(dat)
  data.glm <- dat
  res<-array(,dim=c(1,2))[-1,]
  for (i in 1:K){
    train.dat <- data.glm[sid!=i,]
    test.dat <- data.glm[sid==i,]
    status1<- ind <- train.dat[,c]
    dmat <- t(train.dat[,1:(c-1)])
    stat.pval <- rowWilcox(dmat, ind)
    index.cv<-order(stat.pval, decreasing=F)[1:m]
    #Select the 50 genes with the strongest statistical evidence of differential expression.
    out.dat<- train.dat[,index.cv]
    # create a dataset with containing only 50 genes
    data.cv <- data.frame (out.dat, status1)
    status2 <-test.dat[,c]
    fit.logit <- glm ( status1 ~ ., family = binomial(link=logit), data=data.cv)
    eta <- predict(fit.logit, newdata=test.dat[,index.cv], type=("response"))
    out <- cbind(status2, eta)
    res <- rbind.data.frame(res, out)
  }

  res <- data.frame(res)
  roc(res$status2, res$eta, plot=T, smooth=T, main=paste("ROC for fitting with", m,"genes", sep=" " ))
  AUC <- roc.area(res$status2, res$eta)


  return(list(res, index.cv, AUC))
}

