`fitted.epplab` <- function(object,which=1,...){
  which <- which[which<=length(object$PPindexVal)]
  
  PPscores <- object$x %*% object$PPdir[,which,drop=FALSE]

  PPscores
} 
