\name{summary.epplab}
\docType{methods}
\alias{summary.epplab}
\alias{summary-method}
\alias{summary,epplab-method}

\title{Summarize an epplab Object}
\description{
  Summarizes and prints an \code{epplab} object in an informative way.
}
\usage{
 \method{summary}{epplab}(object, which = 1:10,...)
}

\arguments{
  \item{object}{Object of class \code{epplab}.}
  \item{which}{Summary for \code{which} simulation runs}
  \item{...}{Additional parameters}
}

\details{
 The option \code{which} can restrict the output to certain simulation runs. In case of many simulations,
 this might improve the readability. 
}

\examples{
library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
summary(res)
}


\author{Daniel Fischer}

\keyword{methods}
\keyword{print}
