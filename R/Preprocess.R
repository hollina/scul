#' Process and clean input data for SCUL procedure
#'
#' This function loads a matrix or a data-fram a file as a matrix.
#' It ensures that each column of the matrix is numeric, has all non-missing values,
#' and is not constant. It drops any columns that violate these conditions.
#'
#' @param z A matrix or a data frame.
#' @return A cleaned matrix or data frame with violating columns dropped.
#' @importFrom caret nearZeroVar
#' @export

Preprocess <- function(z) {
  # Function: Clean up a set of data to make sure it is numeric, has all non-missing values, and is not constant
  #
  # Args:
  #   z : A matrix or data frame
  #
  # Returns:
  #   A cleaned matrix or dataframe that is all numeric vairables, with variance and that have no missing elements
  # Dependency:
  # caret

  # Remove any columns if any elements are missing
  z <- data.frame(z[,!sapply(z, function(q) any(is.na(q)))])

  # Remove any columns if they are not numeric
  z <- data.frame(z[,sapply(z,is.numeric)])

  # Remove any columns if they are constant or nearly constant
  nzv <- caret::nearZeroVar(z)
  if(length(nzv > 0)) {
    z <- z[,-nzv]
  }

  # Return trimmed matrix or data frame
  return(z)
}
