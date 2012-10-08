\name{pairs.epplab}
\docType{methods}
\alias{pairs.epplab}
\alias{pairs-method}
\alias{pairs,epplab-method}

\title{Plots a Scatterplot Matrix for an epplab Object}
\description{
 Plots a scatterplot matrix of fitted scores of an \code{epplab} object.
}
\usage{
 \method{pairs}{epplab}(x,which=1:10,...)
}

\arguments{
  \item{x}{Object of class \code{epplab}.}
  \item{which}{Which simulation runs should be taken into account.}
  \item{...}{Graphical parameters, see also par().}
}

\details{
 The option \code{which} can restrict the output to certain simulation runs. In case of many simulations,
 this might improve the readability. 
}

\examples{
library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
pairs(res)
}

\author{Daniel Fischer}

\keyword{methods}
\keyword{hplot}
