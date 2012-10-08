\name{EPPlab}
\alias{EPPlab}

\title{
  Function for Explanatory Projection Pursuit.
}

\description{
  REPPlab optimizes a projection pursuit (PP) index using a Genetic Algorithm (GA) or two Particle Swarm
  Optimisation (PSO) algorithms over several runs, implemented in the Java program
  EPP-lab.
  One of the PSO algorithms is a classic one while the other one is
  a parameter-free extension called Tribes. The parameters of the algorithms (maxiter and individuals for GA and maxiter and particles for PSO) can
  be modified by the user.
  The PP indices are the well-known Friedman (accessible via R in a later package version, so far only available via the Java program) and Friedman-Tukey
  indices together with the kurtosis and a so-called discriminant index that is
  devoted to the detection of groups.
  At each run, the function finds a local optimum of the PP index and gives the
  associated projection direction.
  }
\usage{
  EPPlab(x, PPindex="kurtosisMax", PPalg="GA", n.simu=20, sphere=FALSE,
         maxiter=NULL,individuals=NULL,particles=NULL)
}

\arguments{
  \item{x}{Matrix where each row is an observation and each column a dimension.}
  \item{PPindex}{The used index, see details.}
  \item{PPalg}{The used algorithm, see details.}
  \item{n.simu}{Number of simulation runs.}
  \item{sphere}{Logical, sphere the data. Default is \code{FALSE}.}
  \item{maxiter}{Maximum number of iterations.}
  \item{individuals}{Size of the generated population in GA.}
  \item{particles}{Number of generated particles in the standard PSO algorithm.}
}

\details{
  The function always centers the data using \code{\link{colMeans}} and sphering the data is optional. If sphering is requested
  the function \code{\link{WhitenSVD}} is used, which automatically tries to estimate the rank of the data. 

  Currently the function provides the following projection pursuit indices: 
  \code{KurtosisMax}, \code{Discriminant}, \code{FriedmanTukey}, \code{KurtosisMin}.

  The algorithms can be used to find the projection directions. These are a Genetic Algorithm \code{GA} and two Particle Swarm Optimisation algorithms \code{PSO} and \code{Tribe}.

  Since the algorithms might find local optima they are run several times. The function sorts the found directions according to the optimization criterion. 

  The different algorithm have different default settings. It is for GA: maxiter=50 and individuals=20. For PSO: maxiter=20 and particles=50. For Tribe: maxiter=20.

  For GA, the size of the generated population is fixed by the user (individuals). The algorithm is based on a tournament section of three participants. 
  It uses a 2-point crossover with a probability of 0.65 and the mutation operator is applied to all the individuals with a probability of 0.05. The termination criterion corresponds
  to the number of generations and is also fixed by the user (maxiter).

  For PSO, the user can give the number of initial generated particles and also the maximum number of iterations. The other parameters are fixed following Clerc (2006) and using a
  "cosine" neighborhood adapted to PP for the PSO algorithm. For Tribes, only the maximum number of iterations needs to be fixed. The algorithm proposed by Cooren and Clerc (2009)
  and adapted to PP using a "cosine neighborhood" is used.

}
\value{
  A list with class 'epplab' containing the following components:
  \item{PPdir}{Matrix containing the PP directions as columns, see details.}
  \item{PPindexVal}{Vector containing the objective criterion value of each run.}
  \item{PPindex}{Name of the used projection index.}
  \item{PPalg}{Name of the used algorithm.}
  \item{x}{Matrix containing the data (centered!).}
  \item{sphere}{Logical}
  \item{transform}{The transformation matrix from the whitening step.}
  \item{backtransform}{The back-transformation matrix from the whitening step.}
  \item{center}{The column center of the data}
}

\references{
  \cite{Larabi Marie-Sainte, S. (2011). Biologically inspired algorithms for exploratory projection pursuit. PhD thesis, University of Toulouse.}

  \cite{Ruiz-Gazen, A., Larabi M.-S. and Berro, A. (2010). Detecting multivariate outliers using projection 
   pursuit with particle swarm optimization. \emph{COMPSTAT2010}, pp. 89-98.}

  \cite{Clerc, M. (2006). Particle Swarm Optimization. ISTE, Wiley.}

  \cite{Cooren, Y., Clerc, M. and Siarry, P. (2009). Performance evaluation of TRIBES, an adaptive particle swarm optimization algorithm. Swarm Intelligence, 3(2), 149-178.}
}

\author{
  Daniel Fischer, Klaus Nordhausen
}

\examples{
  library(tourr)
  data(olive)
  olivePP <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMax",n.simu=5, maxiter=20)
  summary(olivePP)

  library(amap)
  data(lubisch)
  X <- lubisch[1:70,2:7]
  rownames(X) <- lubisch[1:70,1]
  res <- EPPlab(X,PPalg="PSO",PPindex="FriedmanTukey",n.simu=15, maxiter=20,sphere=TRUE)
  print(res)
  summary(res)
  fitted(res)
  plot(res)
  pairs(res)
  predict(res,data=lubisch[71:74,2:7])
}
\keyword{
  multivariate
}
 
