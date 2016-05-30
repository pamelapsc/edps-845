## ---- include=FALSE------------------------------------------------------
library(knitr)

## ---- include=FALSE------------------------------------------------------
load("/Users/pamela/Desktop/EDPS 845/edps-845/Project/threeclass-ex.rda")
load("/Users/pamela/Desktop/EDPS 845/edps-845/Project/fourclass-ex.rda")
mixRasch.plot <- function(x, ...){
  nc<- length(x$LatentClass)
    plottemp <- sapply(x$LatentClass, function(x) x$item.par$delta.i)
    matplot(plottemp, type = "o", main = "Item Parameters by Class", xlab = "Items", ylab = "Parameters", pch = 20, bg = 1:nc)
    abline(h = 0, lty = 2)
    par(mfrow = c(1, 2))
}


## ---- fig.keep=TRUE, fig.cap= "Three Class Model", fig.height=3, fig.width= 6----
mixRasch.plot(threeclass_ex)

## ---- fig.keep=TRUE, fig.cap= "Four Class Model", fig.height=3, fig.width= 6----
mixRasch.plot(fourclass_ex)

## ---- include=FALSE------------------------------------------------------

avg.theta<- function(x){
  nc<- length(x$LatentClass)
  outtheta <- sapply(x$LatentClass, function(x) mean(x$person.par$theta))
  return(outtheta)
}

## ------------------------------------------------------------------------
avg.theta(threeclass_ex)
avg.theta(fourclass_ex)

