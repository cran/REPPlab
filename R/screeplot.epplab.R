`screeplot.epplab` <- function(x,type="lines",which=1:10,main="",ylab="Objective criterion",xlab="Simulation run",...){
  which <- which[which<=length(x$PPindexVal)]
  temp <- x$PPindexVal[which]
  names(temp) <- colnames(x$PPdir)[which]


  type<-match.arg(type,c("barplot","lines"))
   
  if (type=="barplot")
  {
    barplot(temp,ylab=ylab,xlab=xlab,main=main,...)
  } else{
    plot(temp,type="b",ylab=ylab,xlab=xlab,main=main,...)
  }
  invisible()
} 
