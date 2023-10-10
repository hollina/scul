#' Run scul procedure on placebo pool to create null distribution
#'
#' Runs the scul procedure on the whole placebo pool
#' (using itself as the donor pool; this could be altered to have a custom donor pool)
#' to create null distribution that can be used for inference in later steps.
#'
#' @param CohensDThreshold A real number greater than 0, indicating the Cohen's D threshold at which
#'                     fit is determined to be "poor". The difference is in standard deviation units. Default is SCUL.input$CohensDThreshold.
#' @param OutputFilePath Output file path. Default is  SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt  An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param PostPeriodLength An integer that indicates the length of the post-treatment period.
#'        Defailt is calculated using SCUL.input data.
#' @param PrePeriodLength An integer that indicates the length of the pre-treatment period.
#'        Defailt is calculated using SCUL.input data.
#' @param NumberInitialTimePeriods An integer that indicates the number of time periods desired in the training data for the first cross-validation run.
#'        Default is the stated amount in SCUL.input data.
#' @param x.PlaceboPool A (T by J) data frame containing all products that you wish to include in the placebo distribution
#'         Must be sorted by time. Default is SCUL.input$x.PlaceboPool.
#' @param TrainingPostPeriodLength The number of timer periods post-treatment for training data. Defaults to all time since treatment begins.
#' @param DonorPoolRestrictionForEachPlacebo This is a customizeable restriction on the placebo pool.
#'         In Hollingsworth and Wing (2020) we use it to ensure that no analysis on a placebo series uses donors from the same state.
#'         Where state is identified by a pattern in each variable name. Default is no restriction.
#'         The placebo pool is indexed by `h`:
#'         For a restriction based on the first two characters: "select(-starts_with(substring(names(x.PlaceboPool)[h],1, 2)))"
#'         For a restriction based on the last two characters: "select(-ends_with(substring(names(x.PlaceboPool)[h],nchar(names(x.PlaceboPool)[h]) - 2 + 1, nchar(names(x.PlaceboPool)[h]))))"
#' @param cvOption Do you want to use the median CV lambda (cvOption = lambda.median),
#'         one that produces the minimum MSE (cvOption = lambda.min), or the largest
#'         lambda that produces a MSE within one standard error of the minimum MSE
#'         (cvOption = lambda.1se).
#'         Default is lambda.1se
#'
#' @return list  A list of standardized placbo effect sizes
#' @import glmnet
#' @import stats
#' @import lubridate
#' @import progress
#'
#' @export

CreatePlaceboDistribution <- function(
  x.PlaceboPool = SCUL.input$x.PlaceboPool,
  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
  PostPeriodLength = nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1,
  PrePeriodLength = SCUL.input$TreatmentBeginsAt-1,
  NumberInitialTimePeriods = SCUL.input$NumberInitialTimePeriods,
  OutputFilePath = SCUL.input$OutputFilePath,
  CohensDThreshold = SCUL.input$CohensDThreshold,
  TrainingPostPeriodLength = SCUL.input$TrainingPostPeriodLength,
  DonorPoolRestrictionForEachPlacebo = "",
  cvOption = "lambda.1se"
) {


  ####################
  # Extract relevant information from placebo data #
  x.PlaceboPool.StandardizedDiff<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=nrow(x.PlaceboPool)))
  x.PlaceboPool.CohensD<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=1))

  ###################
  # Set up variables that are constant across all runs

  # Initialize a matrix of 0's, that is 1 by max # of C.V. runs
  # MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength+1

  # Determine stopping point
  # StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1


  # Set up progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent complete in :elapsed | eta: :eta",
    total = ncol(x.PlaceboPool), clear = FALSE, width= 60)

  ###################
  # Loop over each placebo. Calculate a diff and a % diff. Then store in dataframe.
    for (h in 1:ncol(x.PlaceboPool)){

      #################
      # Create a matrix with pre-treatment values for a given placebo run
      # Here h is the target and -h is the donor pool

      # Extract the target and save as another variable
      y.PlaceboPool.PreTreatment <- as.matrix(x.PlaceboPool[(1:TreatmentBeginsAt-1),h])
      y.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPool[(TreatmentBeginsAt:PostPeriodLength),h])
      y.PlaceboPool <- as.matrix(x.PlaceboPool[, h])

      # Remove the target variable and any variables from the same state as the target
      x.PlaceboPoolWithoutTarget <- x.PlaceboPool[,-h]

      # If there is a restriction, apply it
      if (nchar(DonorPoolRestrictionForEachPlacebo) > 0){
        full_statement <- paste("x.PlaceboPoolWithoutTarget %>% ", DonorPoolRestrictionForEachPlacebo)
        x.PlaceboPoolRestricted <- eval(parse(text = full_statement))
      }
      # If there is not a restriction, leave the donor pool for the placebo as is
      if (nchar(DonorPoolRestrictionForEachPlacebo) == 0){
        x.PlaceboPoolRestricted <- x.PlaceboPoolWithoutTarget
      }

      # Split into pre and post period
      x.PlaceboPool.PreTreatment <- as.matrix(x.PlaceboPoolRestricted [(1:TreatmentBeginsAt-1),])
      x.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPoolRestricted [(TreatmentBeginsAt:PostPeriodLength),])

      # We don't need the time variable for this, but it's required for SCUL.
      time <- data.frame(time = 1:nrow(y.PlaceboPool))

      ###################
      # Run SCUL Procedure
      placeboSCUL <- SCUL(
        PostPeriodLength = PostPeriodLength,
        PrePeriodLength = PrePeriodLength,
        NumberInitialTimePeriods = NumberInitialTimePeriods,
        OutputFilePath = OutputFilePath,
        x.DonorPool.PreTreatment = x.PlaceboPool.PreTreatment,
        y.PreTreatment = y.PlaceboPool.PreTreatment,
        x.DonorPool = x.PlaceboPoolRestricted,
        time = time,
        y.actual = y.PlaceboPool,
        TreatmentBeginsAt = TreatmentBeginsAt,
        TrainingPostPeriodLength = TrainingPostPeriodLength,
        cvOption = cvOption,
        plotCV = FALSE
      )

      # Calculate the standard deviation of the outcome variable in the pre-treatment period
      PreTreatmentSD <-sd(y.PlaceboPool.PreTreatment)

      # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
      x.PlaceboPool.StandardizedDiff[,h] <- (placeboSCUL$y.scul - placeboSCUL$y.actual)/PreTreatmentSD
      colnames(x.PlaceboPool.StandardizedDiff)[h] <- h

      # Calculate the mean pre-treatment cohen's D
      x.PlaceboPool.CohensD[1, h] <- placeboSCUL$CohensD

      # Display Progress bar
      pb$tick()
    }


  x.PlaceboPool.StandardizedDiff.trimmed <- x.PlaceboPool.StandardizedDiff[ , x.PlaceboPool.CohensD <= CohensDThreshold]


  # Wrap all of the items we will need for later into a list.
  OutputData <- list(
    y.placebo.StandardizedDifference = x.PlaceboPool.StandardizedDiff.trimmed,
    y.placebo.StandardizedDifference.Full = x.PlaceboPool.StandardizedDiff,
    y.placebo.CohensD = x.PlaceboPool.CohensD
  )
  # Return list
  return(OutputData)
}
