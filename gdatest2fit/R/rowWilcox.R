rowWilcox <- function(mat, ind){
  pval <- rep("NA", dim(mat)[1])
  datamat <- t(mat)
     for (i in 1: dim(mat)[1]) {
     outtest<-wilcox.test(datamat[,i]~ind, alternative = c("two.sided"), paired = FALSE, exact = NULL)
     pval[i] <- outtest$p.value
  }
    pval <- as.numeric(pval)
    return(pval)
}

