#' Set up data for SCUL procedure
#'
#' This function takes input and creates data products necessary
#' for scul procedure. This input data will be called by all later
#' programs
#'
#' @param time A dataframe that is a column vector (T by 1) with the running time variable.
#'         Must be sorted by time (oldest to most recent).
#' @param y A dataframe that is a column vector (T by 1) containing the target variable of interest.
#'         Must be sorted by time (oldest to most recent).
#' @param TreatmentBeginsAt An integer indicating the time period (<T) in which treatment begins.
#' @param x.DonorPool A (T by K) data frame containing all set of donor pool candidate that will be used to construct synthetic groups.
#'         Must be sorted by time (oldest to most recent).
#' @param CohensDThreshold A real number greater than 0, indicating the Cohen's D threshold at which
#'                     fit is determined to be "poor". The difference is in standard deviation units. Default is .25
#' @param NumberInitialTimePeriods An integer indicating the minimum number of pre-treatment time periods to be
#'                              included in the trainging data for the first cross-validation run.
#'                             Default is the length of the post-treatment time period.
#' @param TrainingPostPeriodLength The number of timer periods post-treatment for training data. Defaults to all time since treatment begins.
#' @param x.PlaceboPool A (T by J) data frame containing all products that you wish to include in the placebo distribution
#'         Must be sorted by time. Default is to be the same as x.
#' @param OutputFilePath A file path to store output. Default is current working directory
#'
#' @return InputDataForSCUL A list of items that will be called on for running the SCUL procedure.
#' @importFrom caret nearZeroVar
#' @export
OrganizeDataAndSetup <- function(
                                  time,
                                  y,
                                  TreatmentBeginsAt,
                                  x.DonorPool,
                                  CohensDThreshold=0.25,
                                  NumberInitialTimePeriods=nrow(y)-TreatmentBeginsAt+1,
                                  x.PlaceboPool=x.DonorPool,
                                  TrainingPostPeriodLength = nrow(y)-TreatmentBeginsAt+1,
                                  OutputFilePath=getwd()
                                ) {
  # Function: Creates data products necessary for scul procedure and takes inputs needed for later procedures
  #
  # Args:
  #   time : A dataframe that is a column vector (T by 1) with the running time variable.
  #         Must be sorted by time.
  #   y : A dataframe that is a column vector (T by 1) containing the variable of interest.
  #         Must be sorted by time.
  #   TreatmentBeginsAt : An integer indicating the time period (<T) in which treatment begins.
  #   x.DonorPool : A (T by K) data frame containing all products used to construct synthetic groups.
  #         Must be sorted by time.
  #
  #   CohensDThreshold : A real number greater than 0, indicating the Cohen's D threshold at which
  #                     fit is determined to be "poor".
  #                     Default is .25
  #   NumberInitialTimePeriods : An integer indicating the minimum number of pre-treatment time periods to be
  #                              included in the trainging data for the first cross-validation run.
  #                             Default is the length of the post-treatment time period.
  #   x.PlaceboPool : A (T by J) data frame containing all products that you wish to include in the placebo distribution
  #         Must be sorted by time.
  #         Default is to be the same as x.
  #   OutputFilePath : A file path to store output. Default is current working directory
  #   TimeFormat: A lubridate-style time format. TODO (hollina). Add this feature.
  #
  # Returns:
  #   A list with all of these inputs organized, to be called SCUL programs later
  #
  # Dependency:
  # preprocess

  # Preprocess each dataset
  y <- Preprocess(y)
  x.DonorPool<- Preprocess(x.DonorPool)
  x.PlaceboPool<- Preprocess(x.PlaceboPool)

  #NumberInitialTimePeriods <- nrow(z)-TreatmentBeginsAt+1
  PostPeriodLength <- nrow(y)-TreatmentBeginsAt+1
  PrePeriodLength <- TreatmentBeginsAt-1

  # Preprocess each dataset subset that could be used in training
  y<-PreprocessSubset(
                      y,
                      TreatmentBeginsAt,
                      NumberInitialTimePeriods,
                      PostPeriodLength,
                      PrePeriodLength,
                      TrainingPostPeriodLength
                      )
  x.DonorPool<-PreprocessSubset(
                                x.DonorPool,
                                TreatmentBeginsAt,
                                NumberInitialTimePeriods,
                                PostPeriodLength,
                                PrePeriodLength,
                                TrainingPostPeriodLength
                                 )
  x.PlaceboPool<-PreprocessSubset(
                                  x.PlaceboPool,
                                  TreatmentBeginsAt,
                                  NumberInitialTimePeriods,
                                  PostPeriodLength,
                                  PrePeriodLength,
                                  TrainingPostPeriodLength
                                  )
  # Create pre treatment datasets
  y.PreTreatment <- as.matrix(y[(1:TreatmentBeginsAt-1),])
  x.DonorPool.PreTreatment <- as.matrix(x.DonorPool [(1:TreatmentBeginsAt-1),])
  x.PlaceboPoolPreTreatment <- as.matrix(x.PlaceboPool [(1:TreatmentBeginsAt-1),])

   # Create post treatment datasets
  y.PostTreatment <- as.matrix(y[(TreatmentBeginsAt:nrow(y)),])
  x.DonorPool.PostTreatment <- as.matrix(x.DonorPool [(TreatmentBeginsAt:nrow(x.DonorPool)),])
  x.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPool [(TreatmentBeginsAt:nrow(x.PlaceboPool)),])

  # Wrap all of the items we will need for later into a list.
  InputDataForSCUL <- list(
    time = time,
    y = y,
    TreatmentBeginsAt = TreatmentBeginsAt,
    x.DonorPool = x.DonorPool,
    CohensDThreshold = CohensDThreshold,
    NumberInitialTimePeriods = NumberInitialTimePeriods,
    x.PlaceboPool = x.PlaceboPool,
    OutputFilePath = OutputFilePath,
    y.PreTreatment =  y.PreTreatment,
    x.DonorPool.PreTreatment =  x.DonorPool.PreTreatment,
    x.PlaceboPoolPreTreatment = x.PlaceboPoolPreTreatment,
    y.PostTreatment = y.PostTreatment,
    x.DonorPool.PostTreatment =  x.DonorPool.PostTreatment,
    x.PlaceboPool.PostTreatment =  x.PlaceboPool.PostTreatment,
    TrainingPostPeriodLength = TrainingPostPeriodLength
  )

  # Return list
  return(InputDataForSCUL)
}
