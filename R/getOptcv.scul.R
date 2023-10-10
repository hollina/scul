#' Extract cross-validated \code{lamdas}
#'
#' This is a simple adaptation of the \code{getOptcv.glmnet} function from  \code{glmnet}.
#' The code takes a grid of \code{lambda} penalties along with a vector of associated mean squared errors (\code{mse}), standard errors (\code{se}), and the mean squared error from each cross-validation run (\code{fullMSE}).
#' From this three potential options for the cross-validated penalty parameter are computed. 1) The \code{lambda} that has the minimum average mean squared error across all the cross-validation runs (\code{lambda$lambda.min}),
#' 2) The \code{lambda} the largest \code{lambda} that is associated with an average cross-validated mean squared error within one standard error of the minimum average cross-validated mean squared error (\code{lambda$lambda.1se}), and
#' 3) the \code{lamda} that is the median of the set of \code{lambdas} (\code{lambda$lambda.median}).
#'
#' @param lambdapath A grid of lambdas that is used in each cross-validation run as potential options for the optimal penalty parameter.
#' @param mse A vector of the average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.
#' @param se A vector of the standard error associated with each average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.
#' @param fullMSE A matrix of the mean squared error for each cross-validation run and each given \code{lambda} in the \code{lambdapath} grid.
#' @param medianLambda A single \code{lambda} that is the median minimum lambda that minimizes the CV path.
#
#'@export
getOptcv.scul <-
  function (lambdapath, mse, se, fullMSE, medianLambda)
  {
    # What is the minimum MSE
    cvmin = min(mse, na.rm = TRUE)

    # Where is the minimum MSE
    idmin = mse <= cvmin

    lambda.min = max(lambdapath[idmin], na.rm = TRUE)
    idmin = match(lambda.min, lambdapath)
    semin = (mse + se)[idmin]

    id1se = mse <= semin
    lambda.1se = max(lambdapath[id1se], na.rm = TRUE)
    id1se = match(lambda.1se, lambdapath)

    # Find the column id of the minimum cv in each row. then take the median
    # idmedian <-  median(apply(fullMSE, 1, which.min))
    idmedian <- match(which.min(abs(lambdapath - medianLambda)), lambdapath)
    # Find the lambda associated with this median.
    # lambda.median = max(lambdapath[idmedian], na.rm = TRUE)
    # idmedian = match(lambda.median, lambdapath)

    index=matrix(c(idmin,id1se,idmedian),3,1,dimnames=list(c("min", "1se", "median"),"Lambda"))

    # Return list
    return(list(lambda.min = lambda.min, lambda.1se = lambda.1se, lambda.median = medianLambda, index = index))

  }
