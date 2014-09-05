\name{summary.epplabOutlier}
\docType{methods}
\alias{summary.epplabOutlier}
\alias{summary,epplabOutlier-method}

\title{Summarize an epplabOutlier Object}
\description{
  Summarizes and prints an \code{epplabOutlier} object in an informative way.
}
\usage{
 \method{summary}{epplabOutlier}(object, ...)
}

\arguments{
  \item{object}{Object of class \code{epplabOutlier}.}
  \item{...}{Additional parameters}
}

\details{
 The main information provided here is a table with names of the observations which are considered outliers
 and in how many PP directions they are considered outliers. This function is useful if the data has been
 given row names. 
}

\examples{
# creating data with 3 outliers
n <-300 
p <- 10
X <- matrix(rnorm(n*p),ncol=p)
X[1,1] <- 9
X[2,4] <- 7 
X[3,6] <- 8
# giving the data rownames, obs.1, obs.2 and obs.3 are the outliers.
rownames(X) <- paste("obs",1:n,sep=".")

PP<-EPPlab(X,PPalg="PSO",PPindex="KurtosisMax",n.simu=20, maxiter=20)
OUT<-EPPlabOutlier(PP, k = 3, location = median, scale = mad)
summary(OUT)
}


\author{Klaus Nordhausen}

\keyword{methods}
\keyword{print}
