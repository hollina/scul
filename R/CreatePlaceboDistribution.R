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
#' @return list  A list of standardized placbo effect sizes
#' @import glmnet
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
  DonorPoolRestrictionForEachPlacebo = ""
) {


  ####################
  # Extract relevant information from placebo data #
  x.PlaceboPool.StandardizedDiff<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=nrow(x.PlaceboPool)))
  x.PlaceboPool.CohensD<-data.frame(matrix(ncol(x.PlaceboPool),nrow=1))

  ###################
  # Set up variables that are constant across all runs

  # Initialize a matrix of 0's, that is 1 by max # of C.V. runs
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength+1

  # Determine stopping point
  StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1

  ###################
  # Loop over each placebo. Calculate a diff and a % diff. Then store in dataframe.
  for (h in 1:ncol(x.PlaceboPool)){
    #################
    # Create a matrix with pre-treatment values for a given placebo run
    # Here h is the target and -h is the donor pool

    # Extract the target and save as another variable
    y.PlaceboPool.PreTreatment <- as.matrix(x.PlaceboPool[(1:TreatmentBeginsAt-1),h])
    y.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPool[(TreatmentBeginsAt:PostPeriodLength),h])

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


    ###################
    # Initialize a matrix of 0's, that is 1 by max # of C.V. runs
    MinimumLambdaRolling<-matrix(0,1,MaxCrossValidations)

    ###############
    for (i in NumberInitialTimePeriods:StoppingPoint) {
      j = i -NumberInitialTimePeriods + 1


      # Specify training data
      placebo.x.training <-x.PlaceboPool.PreTreatment[1:i,]
      placebo.y.training <-y.PlaceboPool.PreTreatment[1:i,]

      # Make sure that y is not constant within the training period
      # nzv <- nearZeroVar(placebo.y.training)
      #  if(length(nzv > 0)) {
      #   print("Y constant in training period. Skipped")

      #} else {

      # Run a lasso with only the training data. It will run on for a grid of lambdas by default
      fit=glmnet(placebo.x.training,placebo.y.training)

      # Extract the entire grid of lambdas from the training data
      LambdaList = fit$lambda

      # Determine when the testing data begins for this CV run
      BeginingOfTestData = i +1

      # Determine when the testing data ends for this CV run
      EndOfTestData = i + TrainingPostPeriodLength

      # Specify testing data
      placebo.x.testing <-x.PlaceboPool.PreTreatment[BeginingOfTestData:EndOfTestData,]
      placebo.y.testing <-y.PlaceboPool.PreTreatment[BeginingOfTestData:EndOfTestData,]

      # Create a predicted y value for the entire pre-treatment time period for each lambda in the the exact grid of lambdas from the training data
      prediction<-predict(fit,newx=x.PlaceboPool.PreTreatment,x=placebo.x.training, y=placebo.y.training, s=fit$lambda,exact=TRUE)

      ###########
      # Find the lambda that results in the minimum mean squared error

      # first take the difference from the prediction and the test data
      mse <- prediction[BeginingOfTestData:EndOfTestData,]  -placebo.y.testing

      # Then square each term
      for (z in 1:length(mse)) {
        mse[z] <- mse[z]^2
      }

      # Now find the minimum squared error, how we do this depends on the length of the post-period
      # Due to how glmnet stores the optimal lambda, we need an if statement for different post-period lengths. If the post-period is only one time period, it will be stored as a single number and if it's longer it will be a vector. The if statement makes sure it is stores as the same type no matter what
      if (PostPeriodLength < 2) {
        # Create a vector, that dat (a single column)
        newV <- as.vector(mse[,1])
        # extract the lambda that minimizes the prediction error
        MinimumLambdaRolling[j]<- LambdaList[which.min(newV)]
      }

      if (PostPeriodLength > 1) {
        # extract the lambda that minimizes the prediction error summed across each time period in the test data
        MinimumLambdaRolling[j]<-LambdaList[which.min(colSums(mse))]
        # }
      }
    }
    #Display the output. Useful for debugging.
    #print("The training data is running from 1 to")
    #print(i)
    #print("The testing data is running from")
    #print(begining_of_test_data)
    #print("to")
    #print(end_of_test_data)
    #print("The minimum lambda is")
    #print(MinimumLambdaRolling[j])


    # Calculate cross-validated lambda
    CrossValidatedLambda<-median(MinimumLambdaRolling)

    # Run the actual fit
    EntirePretreatmentFit=glmnet(x.PlaceboPool.PreTreatment,y.PlaceboPool.PreTreatment)

    # Extract the exact prediction for the cross-validated lambda
    placebo.y.scul <- predict(x=x.PlaceboPool.PreTreatment, y=y.PlaceboPool.PreTreatment, newx = as.matrix(x.PlaceboPoolRestricted), EntirePretreatmentFit,s = CrossValidatedLambda, exact = TRUE)

    # Calculate the standard deviation of the outcome variable in the pre-treatment period
    PreTreatmentSD <-sd(y.PlaceboPool.PreTreatment)

    # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
    CohensD <- (placebo.y.scul-x.PlaceboPool[,h])/PreTreatmentSD

    # Calculate the mean pre-treatment cohen's D
    MeanCohensD <- mean(abs(CohensD[(1:TreatmentBeginsAt-1),]))

    # Store as dataframe, with labeled columns
    x.PlaceboPool.StandardizedDiff[,h]<- CohensD
    colnames(x.PlaceboPool.StandardizedDiff)[h] <- h

    # Calculate the mean pre-treatment cohen's D
    x.PlaceboPool.CohensD[1,h] <- MeanCohensD
    print(paste0(round((h/ncol(x.PlaceboPool))*100, digits = 2)," percent done"))
  }

  x.PlaceboPool.StandardizedDiff.trimmed<-x.PlaceboPool.StandardizedDiff[,x.PlaceboPool.CohensD<=CohensDThreshold]


  # Wrap all of the items we will need for later into a list.
  OutputData <- list(
    y.placebo.StandardizedDifference = x.PlaceboPool.StandardizedDiff.trimmed,
    y.placebo.StandardizedDifference.Full = x.PlaceboPool.StandardizedDiff,
    y.placebo.CohensD = x.PlaceboPool.CohensD
  )
  # Return list
  return(OutputData)
}
