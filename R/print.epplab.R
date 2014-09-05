`print.epplab` <- function(x,...){
  X <- list()
  X$PPindex <- x$PPindex
  X$PPindexVal <- x$PPindexVal[1]
  X$PPalg <- x$PPalg
  X$PPdir <- x$PPdir[,1]
  X$PPiter <- x$PPiter[1]

  print(X,...)
} 
