#' Drops columns that have no variation in training subsets
#'
#' This function loads a matrix or a data-fram a file as a matrix.
#' It ensures that for all relevant subsets that could be used as a training 
#' dataset for the later SCUL procedure have variation. 
#' It drops any columns that violate these conditions.
#'
#' @param z A matrix or a data frame.
#' @param TreatmentBeginsAt Indicator of when (which row) treatment begins.
#' @param PostPeriodLength The number of timer periods post-treatment. Defaults to all time since treatment begins. 
#' @param TrainingPostPeriodLength The number of timer periods post-treatment for training data. Defaults to all time since treatment begins. 
#' @param NumberInitialTimePeriods The number of initial time periods in analysis. Defaults to length of post-treatment period.
#' @param PrePeriodLength The number of time periods before treatment (i.e. the length of the entire training dataset). Defaults to time before treatment begins.
#' @return A cleaned matrix or data frame with violating columns dropped. 
#' @importFrom caret nearZeroVar
#' @export

PreprocessSubset <- function(
                              z,
                              TreatmentBeginsAt,
                              NumberInitialTimePeriods = nrow(z)-TreatmentBeginsAt+1 ,
                              PostPeriodLength = nrow(z)-TreatmentBeginsAt+1,
                              PrePeriodLength = TreatmentBeginsAt-1,
                              TrainingPostPeriodLength = nrow(z)-TreatmentBeginsAt+1
                            ) {
  
  # Determine the maximum number of rolling-origin k-fold cross validations that can occur
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength+1
  # Determine the last position of the largest possible training dataset that only uses pre-treatment data
  StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1
  
  # Make a temporary version of z
  temp.z <- z
  
  # Loop over all possible rolling-origin k-fold cross-validation groups
  for (i in NumberInitialTimePeriods:StoppingPoint) {
    
    # Check to see if this subset of z has variation
    nzv <- nearZeroVar(temp.z[1:i,])
    
    # If it does not, then drop the offending columns
    if(length(nzv > 0)) {
      temp.z <- temp.z[,-nzv]
    }
  }
  
  # Return a data frame or matrix without violating columns
  return(temp.z)
}
##