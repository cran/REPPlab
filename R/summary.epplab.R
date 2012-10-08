summary.epplab <- function(object, which=1:10, ...){
  which <- which[which<=length(object$PPindexVal)]
  cat("REPPlab Summary\n")
  cat("---------------\n")
  cat("Index name       :",object$PPindex,"\n")
  cat("Index values     :",object$PPindexVal[which],"\n")
  cat("Algorithm used   :",object$PPalg,"\n")
  cat("Sphered          :",object$sphered,"\n")
  invisible(object)
} 
