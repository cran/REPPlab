# Changes:
# 2013-02-17: Added the seed parts into the subfunction pp

#-----------------------------------------------------------------------------------------------
# Begin Javastuff preparations

.onLoad <- 
  function(libname, pkgname) {
    .jpackage(pkgname, lib.loc = libname)
  }

eppSession<-function(){
	eppSession <- .jnew("epp/EPPLab")
	return(eppSession)
}
# End of Javastuff preparations
#-----------------------------------------------------------------------------------------------


# Internal function that calls the Java Functions

 pp <- function(data,index,alg,nSimulations,sphere,iterations=NULL,individuals=NULL,particles=NULL){
 
  seed <- runif(1)*10^7

  indexInt <- 0

  rows <- nrow(data)
  cols <- ncol(data)

  index<-match.arg(index,c("KurtosisMax","Friedman","Discriminant","FriedmanTukey","KurtosisMin","Indice4","StahelDonoho"))
  alg<-match.arg(alg,c("GA","PSO","Tribe"))
   
  # Possible options for alg are "GA","Tribes" and "PSO" 
  if(alg=="GA")
  {
    ifelse(is.null(iterations),iterations <- 50, iterations <- iterations)
    ifelse(is.null(individuals),tPara <- 20, tPara <- individuals)
  } else if(alg=="PSO"){
    ifelse(is.null(iterations),iterations <- 20, iterations <- iterations)
    ifelse(is.null(particles),tPara <- 50, tPara <- particles)
  } else if(alg=="Tribe"){
    ifelse(is.null(iterations),iterations <- 20, iterations <- iterations)
    ifelse(is.null(particles),tPara <- 50, tPara <- particles)
  } else {
    stop("Unknown algorithm!\n")
  }

  # Possible Indices as named in the Java code. For easing reasons we switch them to numerical values.
   if(index=="KurtosisMax"){
    indexInt <- 1
  } else if(index=="Friedman"){
    indexInt <- 2
  } else if(index=="Discriminant"){
    indexInt <- 3
  } else if(index=="FriedmanTukey"){
    indexInt <- 4
  } else if(index=="KurtosisMin"){
    indexInt <- 5
 # } else if(index=="Indice4"){
 #   indexInt <- 6
 # } else if(index=="StahelDonoho"){
 #   indexInt <- 7
  } else {
    stop("Unknown (or currently disabled) index!\n")
  }

 eppLabString <- .jcall("epp/EPPLab",returnSig="S",method="eppLabRInterface",indexInt,nSimulations,alg,iterations,tPara,as.double(rows),as.double(cols),as.double(as.vector(t(data))),as.double(seed))

 eppLabString

}


EpplabOutputConv <- function(x)
    {
    xList <- strsplit(x,"\n")
    xList <- xList[[1]]
    indI <-grep("<I>",xList)
    indA <-grep("<A>",xList)
    
    Apart <- xList[indA]
    Apart <- gsub("<A> ","",Apart)
    Apart <- gsub(",",".",Apart)
    Apart <- strsplit(Apart," ")
    Apart <- do.call(cbind,Apart)
    Apart <- apply(Apart,2,as.numeric)
    
    Ipart <- xList[indI]
    Ipart <- gsub("<I> ","",Ipart)
    Ipart <- gsub(",",".",Ipart)
    Ipart <- as.numeric(Ipart)
    
    orderI <- order(Ipart, decreasing=TRUE)
    Ipart <- Ipart[orderI]
    Apart <- Apart[,orderI]
    list(PPindex = Ipart, PPdir = Apart)
    }


EPPlab <- function(x, PPindex="KurtosisMax", PPalg="GA", n.simu=20, sphere=FALSE, maxiter=NULL,individuals=NULL,particles=NULL)
        {
	# center first the data:
	MEANS <- colMeans(x)
        x.c <- as.matrix(sweep(x,2,MEANS,"-"))
	
	# Whitening or centring the data
	if(sphere==TRUE)
	{		
	  x <- WhitenSVD(x)
	} else {
	  x <- x.c
	  attr(x,"center") <- MEANS
	  attr(x,"matrix") <- diag(ncol(x))
	  attr(x,"backmatrix") <- diag(ncol(x))
	}

        jepplab <- pp(data=x, index=PPindex , alg=PPalg , nSimulations=n.simu , sphere=sphere , iterations=maxiter,individuals=individuals,particles=particles)
        AI <- EpplabOutputConv(jepplab)
        xNames <- colnames(x)
        
        PPindexVal <- AI$PPindex
	# Give the whitened directions as PPDir
	ifelse(sphere , PPdir <- attributes(x)$transform %*% AI$PPdir, PPdir <- AI$PPdir)
        rownames(PPdir) <- xNames
        colnames(PPdir) <- paste("Run",1:n.simu,sep="")
        
        RES <- list(PPdir=PPdir, PPindexVal=PPindexVal, PPindex=PPindex, PPalg=PPalg, x=as.matrix(x.c), sphered=sphere, whiteMat=attributes(x)$transform,backMat=attributes(x)$backtransform,center=attributes(x)$center)
        class(RES) <- "epplab"
	RES
     }

