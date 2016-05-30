install.packages("rmarkdown")
library(rmarkdown)
install.packages("rmarkdown")
devtools::use_vignette("mRTvignettes")
mixRasch.plot <- function(x, ...){
  nc<- length(x$LatentClass)
    plottemp <- sapply(x$LatentClass, function(x) x$item.par$delta.i)
    matplot(plottemp, type = "o", main = "Item Parameters by Class", xlab = "Items", ylab = "Parameters", pch = 20, bg = 1:nc)
    abline(h = 0, lty = 2)
    par(mfrow = c(1, 2))
    legend("topright", legend = paste("class", 1:nc, sep = " "), fill = 1:nc, col=1:nc)
}

mixRasch.plot(fourclass_ex)
load("/Users/pamela/Desktop/EDPS 845/edps-845/Project/threeclass-ex.rda")
load("/Users/pamela/Desktop/EDPS 845/edps-845/Project/fourclass-ex.rda")

avg.theta<- function(x){
  nc<- length(x$LatentClass)
  outtheta <- sapply(x$LatentClass, function(x) mean(x$person.par$theta))
  return(outtheta)
}

