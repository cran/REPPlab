`coef.epplab` <- function(object,which=1:ncol(object$PPdir),...){
 object$PPdir[,which,drop=FALSE]
} 
