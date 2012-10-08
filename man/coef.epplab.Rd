\name{coef.epplab}
\docType{methods}
\alias{coef.epplab}
\alias{coef-method}
\alias{coef,epplab-method}

\title{Extracts the Directions of an Epplab Object}
\description{
  Extracts the found directions of an \code{epplab} object.
}
\usage{
 \method{coef}{epplab}(object,which=1:ncol(object$PPdir),...)
}

\arguments{
  \item{object}{Object of class \code{epplab}.}
  \item{which}{Specifies which directions are extracted.}
  \item{...}{Additional parameters.}
}

\details{
The coef function extracts the directions found from the EPPlab call.
}

\examples{
library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
coef(res)
}



\author{Daniel Fischer}

\keyword{methods}
\keyword{print}
