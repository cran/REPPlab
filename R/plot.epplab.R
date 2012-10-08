`plot.epplab` <- function(x, type="kernel", angles="radiants", kernel="biweight", which=1:10, as.table=TRUE, ...){
  
  which <- which[which<=length(x$PPindexVal)]
  x.fitted <- fitted(x,which=which)
  plotThis <- stack(as.data.frame(x.fitted))
  colnames(plotThis) <- c("values","ind")

  # Bring the levels into a numerical order
  temp <-levels(plotThis$ind)
  levels(plotThis$ind) <- temp[order(as.numeric(substr(temp,4,50)))]

  type<-match.arg(type,c("kernel","histogram","angles"))
    
    if (type=="kernel")
    {
      print(densityplot(~values|ind, kernel=kernel, data=plotThis, as.table=as.table, ...))
    } else if(type=="histogram"){
      print(histogram(~values|ind, data=plotThis, as.table=as.table, ...))
    } else if (type=="angles"){
      #print(anglesXYPlot(x, which=1:ncol(x$PPdir), angles=angles, ...))
      print(anglesXYPlot(x, which=which, angles=angles, ...))
    }
    invisible()
} 
