WhitenSVD <- function(x, tol=1e-06)
    {

    Nm <- nrow(x)-1
    MEANS <- colMeans(x)
    x.c <- as.matrix(sweep(x,2,MEANS,"-"))

    SVD <- svd(x.c, nu = 0)
    SV <- SVD$d 
    if (!is.null(tol)) {
        rank <- sum(SVD$d > (SVD$d[1L] * tol))
        if (rank < ncol(x)) {
            SVD$v <- SVD$v[, 1L:rank, drop = FALSE]
            SVD$d <- SVD$d[1L:rank]
            }
        }
    SIGMAs <- SVD$d / sqrt(Nm)
    TRANS <- SVD$v %*% diag(1/SIGMAs)
    RES = x.c %*% TRANS


    attr(RES, "center") <- MEANS
    attr(RES, "transform") <- TRANS
    attr(RES, "backtransform") <- diag(SIGMAs) %*% t(SVD$v)
    attr(RES, "SingularValues") <- SV
    RES
    }