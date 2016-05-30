
#' mixRaschTools Functions -----------------------------------------------------------
#' @name Item parameter plot for mixed Rasch model
#' @alias Item parameter plot for mixed Rasch model
#' @title Function for producing plots of item parameters for multiple class models.
#' @description This function produces item parameters by class for Rasch calibrated mixture models.
#' @usage
#' mixRasch.plot(modelname, itemSet, xlab, ylab, file)
#' @param modelname A fitted multiple class mixture Rasch model
#' @details This function provides an item parameter plot that can be used to compare the different item
#' parameters in mixture Rasch models that contain more than one class.
#' @author Pamela S Trantham
#' @examples
#' Example multiple class mixture Rasch models included with mixRaschTools
#' data(threeclass_ex)
#' Default item paramater plot
#' mixRasch.plot(threeclass_ex)
mixRasch.plot <- function(x, ...){
  nc<- length(x$LatentClass)
    plottemp <- sapply(x$LatentClass, function(x) x$item.par$delta.i)
  matplot(plottemp, type = "o", main = "Item Parameters by Class", xlab = "Items", ylab = "Parameters", pch = 20, bg = 1:nc)
  abline(h = 0, lty = 2)
  par(mfrow = c(1, 2))
  legend("topright", legend = paste("class", 1:nc, sep = " "), fill = 1:nc, col=1:nc)
}

#' mixRasch Average Theta -----------------------------------------------------------
#' @name Average Ability Levels for Multiple Class Mixed Rasch Model
#' @alias Average Ability Levels for Multiple Class Mixed Rasch Model
#' @title Function for producing mean theta values for each class in a mixture Rasch model.
#' @description This function produces mean ability levels for each class in a mixture Rasch model.
#' @usage
#' avg.theta(modelname)
#' @param modelname A fitted multiple class mixture Rasch model
#' @details This function produces a matrix containing the average theta values for each class
#' included in mixture Rasch model.
#' @author Pamela S Trantham
#' @examples
#' Example multiple class mixture Rasch models included with mixRaschTools
#' data(threeclass_ex)
#' Default item paramater plot
#' avg.theta(threeclass_ex)
avg.theta<- function(x){
  nc<- length(x$LatentClass)
    outtheta <- sapply(x$LatentClass, function(x) mean(x$person.par$theta))
  return(outtheta)
}



