`pairs.epplab` <- function(x,which=1:10,...){
  
  which <- which[which<=length(x$PPindexVal)]
  x.fitted <- fitted(x,which=which)
  
  pairs(x.fitted,...)

  invisible()
} 
