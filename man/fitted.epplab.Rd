% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitted.epplab.R
\docType{methods}
\name{fitted.epplab}
\alias{fitted.epplab}
\alias{fitted-method}
\alias{fitted,epplab-method}
\title{Calculates projections of the Data}
\usage{
\method{fitted}{epplab}(object, which = 1, ...)
}
\arguments{
\item{object}{Object of class \code{epplab}.}

\item{which}{Onto which direction should the new data be projected.}

\item{...}{Additional parameters}
}
\value{
A matrix, having in each column the projection onto the direction of
a certain run, and in each row the projected value.
}
\description{
Calculates the projections of the data object onto the directions from an
underlying \code{ebblab} object.
}
\details{
The default projection direction is the direction with the best objective
criterion.
}
\examples{

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)

# Projection to the best direction
fitted(res)

# Projection to the 1,2,5 best directions:
fitted(res,which=c(1,2,5))

}
\author{
Daniel Fischer
}
\keyword{methods}
\keyword{print}
