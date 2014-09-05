\name{plot.epplabOutlier}
\docType{methods}
\alias{plot.epplabOutlier}
\alias{plot,epplabOutlier-method}

\title{Plot for an epplabOutlier Object}
\description{
 Visualizes which observations are considered as outliers and how often for an \code{epplabOutlier} object.
}
\usage{
 \method{plot}{epplabOutlier}(x, col=c("white","black"), outlier=TRUE, ...)
}

\arguments{
  \item{x}{Object of class \code{epplabOutlier}.}
  \item{col}{Which colors should be used for non-outliers and outliers. Default is white and black.}
  \item{outlier}{Logical if only observations considered as outliers at least once should be plotted or all. Default is \code{TRUE}.}
  \item{...}{Graphical parameters passed on to \code{\link{image}}.}
}


\seealso{
 \code{\link{EPPlabOutlier}}, \code{\link{image}}
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
plot(OUT)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\author{Daniel Fischer and Klaus Nordhausen}

\keyword{methods}
\keyword{hplot}
