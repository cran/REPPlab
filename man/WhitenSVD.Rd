% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/whitenSVD.R
\name{WhitenSVD}
\alias{WhitenSVD}
\title{Whitening Data Using Singular Value Decomposition}
\usage{
WhitenSVD(x, tol = 1e-06)
}
\arguments{
\item{x}{A numeric data frame or data matrix with at least two rows.}

\item{tol}{Tolerance value to decide the rank of the data. See details for
further information. If set to \code{NULL} it will be ignored.}
}
\value{
The whitened data.
}
\description{
The function whitens a data matrix using the singular value decomposition.
}
\details{
The function whitens the data so that the result has mean zero and identity
covariance matrix using the function \code{\link{svd}}. The data can have
here less observations than variables and svd will determine the rank of the
data automatically as the number of singular values larger than the largest
singular value times \code{tol}. If \code{tol=NULL} the rank is set to the
number of singular values, which is not advised when one or more singular
values are close to zero.

The output contains among others as attributes the singular values and the
matrix needed to backtransform the whitened data to its original space.
}
\examples{

# more observations than variables
X <- matrix(rnorm(200),ncol=4)
A <- WhitenSVD(X)
round(colMeans(A),4)
round(cov(A),4)
# how to backtransform
summary(sweep(A \%*\% (attr(A,"backtransform")), 2, attr(A,"center"), "+") - X)

# fewer observations than variables
Y <- cbind(rexp(4),runif(4),rnorm(4), runif(4), rnorm(4), rt(4,4))
B <- WhitenSVD(Y)
round(colMeans(B),4)
round(cov(B),4)
# how to backtransform
summary(sweep(B \%*\% (attr(B,"backtransform")), 2, attr(B,"center"), "+") - Y)


}
\seealso{
\code{\link{svd}}, \code{\link{cov}}, \code{\link{colMeans}}
}
\author{
Klaus Nordhausen
}
\keyword{multivariate}
