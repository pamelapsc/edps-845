#' mixRaschTools
#' Functions to plot for Item Parameters in Multiple Class Models and to Return Average Ability Levels
#' Version 0.1
#' Authors@R: person("Pamela", "Trantham", email = "Pamela.Trantham57@gmail.com",
#' role = c("aut", "cre"))
#'
#' Description: This package provides a plotting function for plotting item parameters for mixed Rasch
#' models with multiple classes.  A function for returning average ability levels for multiple classes
#' in a mixed Rasch model is also provided.
#' Depends: R (>= 3.1.0); mixRasch 1.1
#' License: None
#'

#' mixRasch Plot -----------------------------------------------------------
#' Generate a plot for a mixture Rasch model
#' @param x the name of the model
mixRasch.plot <- function(x, ...){
  nc<- length(x$LatentClass)
  for (i in nc){
    plottemp <- sapply(x$LatentClass, function(x) x$item.par$delta.i)
  }
  matplot(plottemp, type = "o", main = "Item Parameters by Class", xlab = "Items", ylab = "Parameters", pch = 20, bg = 1:nc)
  abline(h = 0, lty = 2)
  par(mfrow = c(1, 2))
  legend("topright", legend = paste("class", 1:nc, sep = " "), fill = 1:nc, col=1:nc)
}

#'@examples
#'Example models included with mixRaschTools

data(threeclass_ex)

#' Default item paramater plot
mixRasch.plot(threeclass_ex)

data(fourclass_ex)

mixRasch.plot(fourclass_ex)

#' Function to obtain average ability levels by class
avg.theta<- function(x){
  nc<- length(x$LatentClass)
  for (i in nc){
    outtheta <- sapply(x$LatentClass, function(x) mean(x$person.par$theta))
  }
  return(outtheta)
}

#Obtain average ability levels by class - 3 class model
avg.theta(threeclass_ex)

#Obtain average ability levels by class - 4 class model
avg.theta(fourclass_ex)

