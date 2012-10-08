pkgname <- "REPPlab"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('REPPlab')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("EPPlab")
### * EPPlab

flush(stderr()); flush(stdout())

### Name: EPPlab
### Title: Main Function of the Package.
### Aliases: EPPlab
### Keywords: multivariate

### ** Examples

  library(tourr)
  data(olive)
  olivePP <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMax",n.simu=5, maxiter=20)
  summary(olivePP)



cleanEx()
nameEx("EPPlabOutlier")
### * EPPlabOutlier

flush(stderr()); flush(stdout())

### Name: EPPlabOutlier
### Title: Function to Find Outliers for an epplab Object
### Aliases: EPPlabOutlier
### Keywords: multivariate

### ** Examples

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
OUT



cleanEx()
nameEx("WhitenSVD")
### * WhitenSVD

flush(stderr()); flush(stdout())

### Name: WhitenSVD
### Title: Whitening Data Using Singular Value Decomposition
### Aliases: WhitenSVD
### Keywords: multivariate

### ** Examples

# more observations than variables
X <- matrix(rnorm(200),ncol=4)
A <- WhitenSVD(X)
round(colMeans(A),4)
round(cov(A),4)
# how to backtransform
summary(sweep(A %*% (attr(A,"backtransform")), 2, attr(A,"center"), "+") - X)

# fewer observations than variables
Y <- cbind(rexp(4),runif(4),rnorm(4), runif(4), rnorm(4), rt(4,4))
B <- WhitenSVD(Y)
round(colMeans(B),4)
round(cov(B),4)
# how to backtransform
summary(sweep(B %*% (attr(B,"backtransform")), 2, attr(B,"center"), "+") - Y)




cleanEx()
nameEx("coef.epplab")
### * coef.epplab

flush(stderr()); flush(stdout())

### Name: coef.epplab
### Title: Print the directions of an epplab Object
### Aliases: coef.epplab coef-method coef,epplab-method
### Keywords: methods print

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
print(res)



cleanEx()
nameEx("fitted.epplab")
### * fitted.epplab

flush(stderr()); flush(stdout())

### Name: fitted.epplab
### Title: Calculates projections of the Data
### Aliases: fitted.epplab fitted-method fitted,epplab-method
### Keywords: methods print

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)

# Projection to the best direction
fitted(res)

# Projection to the 1,2,5 best directions:
fitted(res,which=c(1,2,5))



cleanEx()
nameEx("pairs.epplab")
### * pairs.epplab

flush(stderr()); flush(stdout())

### Name: pairs.epplab
### Title: Plot a Scatterplot Matrix for an epplab Object
### Aliases: pairs.epplab pairs-method pairs,epplab-method
### Keywords: methods hplot

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
pairs(res)



cleanEx()
nameEx("plot.epplab")
### * plot.epplab

flush(stderr()); flush(stdout())

### Name: plot.epplab
### Title: Plot for an epplab Object
### Aliases: plot.epplab plot-method plot,epplab-method
### Keywords: methods hplot

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)

# Plot with kernel estimator
plot(res)

# plot as histogram
plot(res,type="histogram")

# Just the best 5 and then 8
plot(res,which=c(1:5,8))




cleanEx()
nameEx("plot.epplabOutlier")
### * plot.epplabOutlier

flush(stderr()); flush(stdout())

### Name: plot.epplabOutlier
### Title: Plot for an epplabOutlier Object
### Aliases: plot.epplabOutlier plot,epplabOutlier-method
### Keywords: methods hplot

### ** Examples

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



cleanEx()
nameEx("predict.epplab")
### * predict.epplab

flush(stderr()); flush(stdout())

### Name: predict.epplab
### Title: Calculates projections for a new Data Object
### Aliases: predict.epplab predict-method predict,epplab-method
### Keywords: methods print

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10], PPalg="PSO", PPindex="KurtosisMin", n.simu=10, maxiter=20)

newData <- matrix(rnorm(800), ncol=8)

# Projection on the best direction
predict(res, data=newData)

# Projection on the best 3 directions
predict(res, which=1:3, data=newData)

# Similar with function fitted() when no data is given:
predict(res)
fitted(res)



cleanEx()
nameEx("print.epplab")
### * print.epplab

flush(stderr()); flush(stdout())

### Name: print.epplab
### Title: Print an epplab Object
### Aliases: print.epplab print-method print,epplab-method
### Keywords: methods print

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
print(res)



cleanEx()
nameEx("print.epplabOutlier")
### * print.epplabOutlier

flush(stderr()); flush(stdout())

### Name: print.epplabOutlier
### Title: Print an epplabOutlier Object
### Aliases: print.epplabOutlier print,epplabOutlier-method
### Keywords: methods print

### ** Examples

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
OUT



cleanEx()
nameEx("screeplot.epplab")
### * screeplot.epplab

flush(stderr()); flush(stdout())

### Name: screeplot.epplab
### Title: Creating a Screeplot for an epplab Object
### Aliases: screeplot.epplab screeplot-method screeplot,epplab-method
### Keywords: hplot

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
screeplot(res)

# Pretty useless:
screeplot(res,type="barplot")

screeplot(res,which=1:5)



cleanEx()
nameEx("summary.epplab")
### * summary.epplab

flush(stderr()); flush(stdout())

### Name: summary.epplab
### Title: Summarize an epplab Object
### Aliases: summary.epplab summary-method summary,epplab-method
### Keywords: methods print

### ** Examples

library(tourr)
data(olive)
res <- EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMin",n.simu=10, maxiter=20)
summary(res)



cleanEx()
nameEx("summary.epplabOutlier")
### * summary.epplabOutlier

flush(stderr()); flush(stdout())

### Name: summary.epplabOutlier
### Title: Summarize an epplabOutlier Object
### Aliases: summary.epplabOutlier summary,epplabOutlier-method
### Keywords: methods print

### ** Examples

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



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
