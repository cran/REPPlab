outlier <- function(x,k=3,location=mean,scale=sd)
    {
    LOCATION <- location(x)
    SCALE <- scale(x)
    OUTLIER <- ifelse(abs(x-LOCATION) >= k*SCALE, 1, 0)
    OUTLIER
    }


EPPlabOutlier <- function(x, which=1:ncol(x$PPdir), k=3, location=mean, scale=sd)
    {
    LOCname <- deparse(substitute(location))
    SCAname <- deparse(substitute(scale))
    
    FITS <- fitted(x, which=which)
    OUTLIER <- apply(FITS,2,outlier,k=k,location=location, scale=scale)
    RES <- list(outlier=OUTLIER, k=k, location=LOCname, scale=SCAname, PPindex=x$PPindex, PPalg=x$PPalg)
    class(RES) <- "epplabOutlier"
    RES
    }