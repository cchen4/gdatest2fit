result2tests<-function(dmat, index){
  # equal variances is the default method for two sample rowttests
  ### control FDR=.05
  dmat <- as.matrix(dmat)
  ttest1<-rowttests(dmat, fac=as.factor(index))
  wtest1<-rowWilcox(dmat, ind=index)

  pval.t<-ttest1[,3]
  pval.w<-wtest1

  ### unadjusted P-values
  plot(log(pval.t), log(pval.w), xlab="p-value ttest", ylab="p-value wilcoxon", main="log scale")
  abline(a=0,b=1,col=4,lwd=3)

  hist(pval.t, nclass=50)
  hist(pval.w, nclass=50)

  ### ajusted pval
  pval.t <- p.adjust(pval.t, method = "fdr", n = length(pval.t))
  pval.w <- p.adjust(pval.w, method= "fdr", n =length(pval.w))


  prop1<-mean(I(pval.t<=.005), na.rm=T)
  prop2<-mean(I(pval.t<=.05), na.rm=T)
  prop3<-mean(I(pval.t<=.5), na.rm=T)
  #cat("\n\nthe table of p-value at t test\n\n")
  table1<-cbind(prop1, prop2, prop3)
  colnames(table1)<-c("<=.005", "<=.05", "<=.5")

  wprop1<-mean(I(pval.w<=.005), na.rm=T)
  wprop2<-mean(I(pval.w<=.05),na.rm=T)
  wprop3<-mean(I(pval.w<=.5),na.rm=T)
  table2<-cbind(wprop1, wprop2, wprop3)
  colnames(table2)<-c("<=.005", "<=.05", "<=.5")
  #cat("\n\nthe table of p-value of wilcoxon test\n\n")

  return(list(pval.t, pval.w, table1, table2))
}

