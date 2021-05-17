#' Standardize column vector
#'
#' Function to standardize a column vector in a similar way that glmnet does. Used to calculate max penalty in \code{lambda} grid.
#' Taken from: https://stackoverflow.com/questions/23686067/default-lambda-sequence-in-glmnet-for-cross-validation
#'
#' @param z Input
#
#'@export
mysd <- function(z){
  sqrt(sum((z-mean(z))^2)/length(z))
}
