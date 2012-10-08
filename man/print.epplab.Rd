\name{print.epplab}
\docType{methods}
\alias{print.epplab}
\alias{print-method}
\alias{print,epplab-method}

\title{Print an epplab Object}
\description{
  Prints an \code{epplab} object.
}
\usage{
 \method{print}{epplab}(x,...)
}

\arguments{
  \item{x}{Object of class \code{epplab}.}
  \item{...}{Additional parameters}
}

\details{
The print function displays the result with the best value in the objective criterion. 
}

\examples{
library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
print(res)
}



\author{Daniel Fischer}

\keyword{methods}
\keyword{print}
