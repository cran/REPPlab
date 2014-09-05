plot.epplabOutlier <- function(x,col=c("white","black"),outlier=TRUE, ...) 
{
  # If only outlier are requested, then the outlier matrix is limitied to those, where
  # the rowsum is larger than 0.
  ifelse(outlier, X <- x$outlier[apply(x$outlier,1,sum)>0,,drop=FALSE] ,  X <- x$outlier)
 
  p <- ncol(X)
  n <- nrow(X)
  if (n==0) stop('Option "outlier=TRUE" not meaningful since no outliers were detected')
  X2 <- t(X[n:1,, drop=FALSE])

  image(1:p,1:n,X2, col=col, axes=FALSE, xlab="", ylab="",...)
  box()
  axis(2, labels=colnames(X2), at=1:n,...)
  axis(1, labels=rownames(X2), at=1:p,...)    
}
