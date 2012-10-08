`predict.epplab` <- function(object,which=1, data=NULL,...){
  
  which <- which[which<=length(object$PPindexVal)]
  if(is.null(data)==T) data <- object$x

  # Center the data with the mean vector of the object:
  data <- as.matrix(sweep(data,2,object$center,"-"))

  PPscores <- data %*% object$PPdir[,which]

  PPscores
} 
