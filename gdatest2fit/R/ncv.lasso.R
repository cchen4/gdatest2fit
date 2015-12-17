ncv.lasso <- function (gex, k1, k2, m){
  data.glm <-gex
  gname <-(colnames(data.glm[,-max(ncol(data.glm))]))
  ### assigning the id to each row of data to be used for create subsamples
  sid <- resample (data = gex  , K=k1) ### 10 folds CV
  c <- ncol(data.glm)

  res<-array(,dim=c(1,2))[-1,]

  for (i in 1:k1){
    ### create test data and trainning data
    train.dat <- data.glm[sid!=i,]
    test.dat <- data.glm[sid==i,]

    status1<- ind <- train.dat[,c]
    dmat <- t(train.dat[,1:(c-1)])

    stat.pval <- rowWilcox(dmat, ind)
    index.cv<-order(stat.pval, decreasing=F)[1:m]

    #Select the 50 genes with the strongest statistical evidence of differential expression.
    x.lasso<- as.matrix(train.dat[,index.cv])

    # create a dataset with containing only m genes
    status2 <-test.dat[,c]
    x.test <- as.matrix(test.dat[,index.cv])

    # inner loop to determine the optimal lambda value for lasso regression
    fit.lasso <- cv.glmnet(x.lasso, status1,  nfolds= k2, family="binomial", type.measure = "auc")

    # predict the test data with optimal lambda value
    eta <- predict(fit.lasso, newx=x.test, s= "lambda.min", type="response")

    out <- cbind(status2, eta)
    res <- rbind.data.frame(res, out)

    ##b.name<-gname[which(coef(fit.lasso, s="lambda.min")!=0)]
    ## Parameter estimates for genes
    ##b.coef<-coef(fit.lasso)[which(coef(fit.lasso, s="lambda.min")!=0)]
    ##names(b.coef) <- b.name
    ##(b.coef)
    ##cat("\n the coeficient of beta not equal 0 \n")
    ##print(b.coef)

  }

  roc(res$status2, res[,2], plot=T, smooth=T, main=paste("ROC for fitting with", m,"genes", sep=" " ))
  #roc.plot(res$status, res[,2], main="ROC Curve with CV", xlab="False Positive", ylab="True Positive")
  AUC <- roc.area(res$status2, res[,2])

  return(list(res, index.cv, AUC))
}
