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

 pp <- function(data,index,alg,nSimulations,iterations=NULL,individuals=NULL,particles=NULL, step_iter, eps){
 
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
    ifelse(is.null(iterations),iterations <- 200, iterations <- iterations)
    ifelse(is.null(particles),tPara <- 50, tPara <- particles)
  } else if(alg=="Tribe"){
    ifelse(is.null(iterations),iterations <- 200, iterations <- iterations)
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

 eppLabString <- .jcall("epp/EPPLab",returnSig="S",method="eppLabRInterface",indexInt,nSimulations,alg,iterations,tPara,as.double(rows),as.double(cols),as.double(as.vector(t(data))),as.double(seed), as.double(step_iter), as.double(eps))
 eppLabString

}


EpplabOutputConv <- function(x, maxiter)
    {
    xList <- strsplit(x,"\n")
    xList <- xList[[1]]
    indI <-grep("<I>",xList)
    indA <-grep("<A>",xList)
    indNumberIter <- grep("<Number_of_iterations>",xList)
    indHasConverged <- grep("<Has_converged>",xList)    

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

    NumberIterPart <- xList[indNumberIter]
    NumberIterPart <- gsub("<Number_of_iterations> ","",NumberIterPart)
    NumberIterPart <- gsub(" </Number_of_iterations>","",NumberIterPart)
    NumberIterPart <- as.numeric(unlist(strsplit(NumberIterPart," ")))

    HasConvergedPart <- xList[indHasConverged]
    HasConvergedPart <- gsub("<Has_converged> ","",HasConvergedPart)
    HasConvergedPart <- gsub(" </Has_converged>","",HasConvergedPart)
    HasConvergedPart <- unlist(strsplit(HasConvergedPart," "))

    #converged <- NumberIterPart < maxiter
    HasConvergedPart <- ifelse(HasConvergedPart=="true",TRUE,FALSE)
    converged <- HasConvergedPart
    Ipart.c <- Ipart[converged]
    Ipart.nc <- Ipart[!converged] 
    Apart.c <- as.matrix(Apart[,converged])
    Apart.nc <- as.matrix(Apart[,!converged])
    NumberIterPart.c <- NumberIterPart[converged]
    NumberIterPart.nc <- NumberIterPart[!converged] 
    HasConvergedPart.c <- HasConvergedPart[converged]
    HasConvergedPart.nc <- HasConvergedPart[!converged]

    orderI.c <- order(Ipart.c, decreasing=TRUE)
    Ipart.c <- Ipart.c[orderI.c]
    Apart.c <- as.matrix(Apart.c[,orderI.c])
    NumberIterPart.c <- NumberIterPart.c[orderI.c]
    HasConvergedPart.c <- HasConvergedPart.c[orderI.c]

    orderI.nc <- order(Ipart.nc, decreasing=TRUE)
    Ipart.nc <- Ipart.nc[orderI.nc]
    Apart.nc <- as.matrix(Apart.nc[,orderI.nc])
    NumberIterPart.nc <- NumberIterPart.nc[orderI.nc]
    HasConvergedPart.nc <- HasConvergedPart.nc[orderI.nc]

    Ipart <- c(Ipart.c,Ipart.nc)
    Apart <- cbind(Apart.c,Apart.nc)
    NumberIterPart <- c(NumberIterPart.c,NumberIterPart.nc)
    HasConvergedPart <- c(HasConvergedPart.c, HasConvergedPart.nc)

# Uncomment this if the order should be given with respect to value <I> (maybe this could be also an option in the print function?!)
#     orderI <- order(Ipart, decreasing=TRUE)
#     Ipart <- Ipart[orderI]
#     Apart <- Apart[,orderI]
#     NumberIterPart <- NumberIterPart[orderI]

# Order according to convergence
#     orderNumbIter <- order(NumberIterPart, decreasing=FALSE)
#     Ipart <- Ipart[orderNumbIter]
#     Apart <- Apart[,orderNumbIter]
#     NumberIterPart <- NumberIterPart[orderNumbIter]

    if(sum(!converged)>0) {
      if(sum(!converged)==1){ 
        warning("There was ",sum(!converged)," non-converged simulation run!")
      } else {
        warning("There were ",sum(!converged)," non-converged simulation runs!")
      }
    }

    list(PPindex = Ipart, PPdir = Apart, PPiter = NumberIterPart, PPconv= HasConvergedPart)
    }


EPPlab <- function(x, PPindex="KurtosisMax", PPalg="GA", n.simu=20, sphere=FALSE, maxiter=NULL, individuals=NULL, particles=NULL, step_iter=10, eps=10^(-6))
        {
	# Check if row names are given
        if(is.null(rownames(x))) rownames(x) <- 1:nrow(x)

	# center first the data:
	MEANS <- colMeans(x)
        x.c <- as.matrix(sweep(x,2,MEANS,"-"))
	
	# Whitening or centring the data
	if(sphere==TRUE)
	{		
	  x <- WhitenSVD(x)
	} else {
	  x <- x.c
          SDS <- apply(x,2,sd)
          x <- sweep(x,2,SDS, "/")
	  attr(x,"center") <- MEANS
	  attr(x,"transform") <- diag(1/SDS)
	  attr(x,"backtransform") <- diag(SDS)
	}

        jepplab <- pp(data=x, index=PPindex , alg=PPalg , nSimulations=n.simu , iterations=maxiter,individuals=individuals,particles=particles, step_iter=step_iter, eps=eps)
        AI <- EpplabOutputConv(jepplab, maxiter)
        xNames <- colnames(x)
        
        PPindexVal <- AI$PPindex
	# Give the whitened directions as PPDir
	    PPdir <- attributes(x)$transform %*% AI$PPdir
        rownames(PPdir) <- xNames
        colnames(PPdir) <- paste("Run",1:n.simu,sep="")
        
        RES <- list(PPdir=PPdir, PPindexVal=PPindexVal, PPindex=PPindex, PPiter= AI$PPiter,PPconv= AI$PPconv, PPalg=PPalg, x=as.matrix(x.c), sphered=sphere, whiteMat=attributes(x)$transform, backMat=attributes(x)$backtransform, center=attributes(x)$center, maxiter=maxiter)
        class(RES) <- "epplab"
	RES
     }

