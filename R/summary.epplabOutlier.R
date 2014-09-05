summary.epplabOutlier <- function(object, ...)
    {
    RowSUMS <- rowSums(object$outlier)
    
    if (sum(RowSUMS) !=0){
        IDrowSUMS <- RowSUMS>0
        OutlierNames <- row.names(object$outlier)[IDrowSUMS]
        OutlierFreq <- RowSUMS[IDrowSUMS]
        OutlierN <- length(OutlierNames)
    
        OutlierTable <- as.table(rbind(OutlierNames,OutlierFreq))
        attr(OutlierTable,"dimnames")<-list(c("OutlierID:", "Frequency:"),rep("",OutlierN))

        cat("REPPlab Outlier Summary\n")
        cat("-----------------------\n")
        cat("Index name       :", object$PPindex, "\n")
        cat("Algorithm used   :", object$PPalg, "\n")
        cat("Location used    :", object$location, "\n")
        cat("Scale used       :", object$scale, "\n")
        cat("k value used     :", object$k, "\n")
        cat("-----------------------\n")
        cat("\n")
        cat("Number of outliers detected:\n", length(OutlierNames), "\n")
        cat("\n")
        cat("Observations considered outliers:")
        print(OutlierTable)
        cat("\n")} else {
        cat("REPPlab Outlier Summary\n")
        cat("-----------------------\n")
        cat("Index name       :", object$PPindex, "\n")
        cat("Algorithm used   :", object$PPalg, "\n")
        cat("Location used    :", object$location, "\n")
        cat("Scale used       :", object$scale, "\n")
        cat("k value used     :", object$k, "\n")
        cat("-----------------------\n")
        cat("\n")
        cat("No outliers detected")
        cat("\n")
        }
    invisible(object)
    }
