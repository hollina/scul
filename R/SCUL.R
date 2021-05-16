#' Run SCUL procedure on target data
#'
#' Calculate a synthetic time series using a penalized regression and a donor pool.
#'
#' @param PostPeriodLength An integer that indicates the length of the post-treatment period.
#'        Defailt is calculated using SCUL.input data.
#' @param PrePeriodLength An integer that indicates the length of the pre-treatment period.
#'        Defailt is calculated using SCUL.input data.
#' @param NumberInitialTimePeriods An integer that indicates the number of time periods desired in the training data for the first cross-validation run.
#'        Default is the stated amount in SCUL.input data.
#' @param  OutputFilePath A file path to store output. Default is taken from
#'        SCUL.input$OutputFilePath
#' @param  x.DonorPool.PreTreatment A data frame with the pre-treament values of the donor pool.
#'        Default is to extract this from SCUL.input$x.DonorPool.PreTreatment
#' @param  y.PreTreatment A data frame with the pre-treament values of the target variable
#'        Default is to extract this from SCUL.input$y.PreTreatment
#' @param  x.DonorPool A data frame with the all values of the donor pool.
#'        Default is to extract this from SCUL.input$x.DonorPool
#' @param  time A data frame that is a column vector (Tx1) sorted by time. Default is taken from
#'        SCUL.input$time
#' @param  y.actual A data frame that is a column vector (Tx1) sorted by time
#'         Default is taken from SCUL.input$y
#' @param  TreatmentBeginsAt An integer indicating which row begins treatment.
#'         Default is to begin this at SCUL.input$TreatmentBeginsAt.
#' @param TrainingPostPeriodLength The number of timer periods post-treatment for training data. Defaults to all time since treatment begins.
#' @param cvOption Do you want to use the median CV lambda (cvOption = lambda.median),
#'         one that produces the minimum MSE (cvOption = lambda.min), or the largest
#'         lambda that produces a MSE within one standard error of the minimum MSE
#'         (cvOption = lambda.1se).
#'         Default is lambda.1se
#'
#' @return OutputDataSCUL  A list with the synthetic series, weights used to construct the series, and measures of fit.
#' @import glmnet
#' @import stats
#' @export
SCUL <- function(
                  PostPeriodLength = nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1,
                  PrePeriodLength = SCUL.input$TreatmentBeginsAt-1,
                  NumberInitialTimePeriods = SCUL.input$NumberInitialTimePeriods,
                  OutputFilePath = SCUL.input$OutputFilePath,
                  x.DonorPool.PreTreatment = SCUL.input$x.DonorPool.PreTreatment,
                  y.PreTreatment = SCUL.input$y.PreTreatment,
                  x.DonorPool = SCUL.input$x.DonorPool,
                  time = SCUL.input$time,
                  y.actual = SCUL.input$y,
                  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
                  TrainingPostPeriodLength = SCUL.input$TrainingPostPeriodLength,
                  cvOption = "lambda.1se"
){
  ### Function to standardize variables: (need to use n instead of (n-1) as denominator)
  ### Note: This came from https://stackoverflow.com/questions/23686067/default-lambda-sequence-in-glmnet-for-cross-validation
  mysd <- function(z) sqrt(sum((z-mean(z))^2)/length(z))

  # Function to get optimal cv. Simple adaptation from getOptcv.glmnet
  # Function to get optimal cv
  getOptcv.scul <-
    function (lambdapath, mse, se, fullMSE)
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
      idmedian <-  median(apply(fullMSE, 1, which.min))

      # Find the lambda associated with this median.
      lambda.median = max(lambdapath[idmedian], na.rm = TRUE)
      idmedian = match(lambda.median, lambdapath)

      index=matrix(c(idmin,id1se, idmedian),3,1,dimnames=list(c("min", "1se", "median"),"Lambda"))
      list(lambda.min = lambda.min, lambda.1se = lambda.1se,lambda.median = lambda.median, index = index)
    }

  # Determine the maximum number of rolling-origin k-fold cross validations that can occur
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength+1
  # Determine the last position of the largest possible training dataset that only uses pre-treatment data
  StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1

  # What are the number of lambdas we want to search over.
  MaxLambdasInGrid <- 100 # by default glmnet won't go passed 100, but it may stop earlier. So picking 100 here ensures that this list is contained.

    ## Calculate the max-lambda across all of the cross-validation runs.
    MaxLambdaList <- matrix(0, 1, MaxCrossValidations)

      ### First calculate the max lambda for each cross-validation run.
      for (i in NumberInitialTimePeriods:StoppingPoint){
        #i <- NumberInitialTimePeriods
        j = i - NumberInitialTimePeriods + 1

        # Specify training data
        x.training <-x.DonorPool.PreTreatment[1:i,] # this had me switch row and column. Why?
        y.training <-y.PreTreatment[1:i] # this had me switch row and column. Why?

        sx <- as.matrix(scale(x.training, scale = apply(x.training, 2, mysd)))

        # Get lambda_max: See https://stackoverflow.com/questions/25257780/how-does-glmnet-compute-the-maximal-lambda-value/
        MaxLambdaList[j] <- max(abs(colSums(sx*y.training)))/nrow(sx)
      }

      ### Now calculate the lambda sequence
      epsilon <- .0001 # default used in glmnet

      lambdapath <- round(exp(seq(log(max(MaxLambdaList)), log(max(MaxLambdaList)*epsilon),
                                  length.out = MaxLambdasInGrid)), digits = 10)
      # Initialize a matrix of 0's, that is # of lambdas in grid by max # of C.V. runs
      # MinimumLambdaRolling <- matrix(0, MaxLambdasInGrid, MaxCrossValidations)

      # Initialize a matrix of 0's, that is # of lambdas in grid by max # of C.V. runs
      MinimumLambdaRollingMSE <- matrix(0, MaxLambdasInGrid, MaxCrossValidations)

      # Preform lasso for each cross-validation run and store test MSE for each run for each lambda in grid
      for (i in NumberInitialTimePeriods:StoppingPoint){
        #i <- NumberInitialTimePeriods
        j = i -NumberInitialTimePeriods + 1

        # Specify training data
        x.training <-x.DonorPool.PreTreatment[1:i,] # this had me switch row and column. Why?
        y.training <-y.PreTreatment[1:i] # this had me switch row and column. Why?

        # Run a lasso with only the training data. It will run on for a grid of lambdas by default
        fit = glmnet(x.training, y.training, lambda = lambdapath)

        # Determine when the testing data begins for this CV run
        BeginingOfTestData = i +1

        # Determine when the testing data ends for this CV run
        EndOfTestData = i + TrainingPostPeriodLength

        # Specify testing data
        x.testing <-x.DonorPool.PreTreatment[BeginingOfTestData:EndOfTestData,]
        y.testing <-y.PreTreatment[BeginingOfTestData:EndOfTestData,]

        # Create a predicted y value for the entire pre-treatment time period for each lambda in the the exact grid of lambdas from the training data
        prediction <- predict(fit,
                              newx = x.DonorPool.PreTreatment,
                              x = x.training,
                              y = y.training,
                              s = lambdapath,
                              exact = TRUE)
        # Squared error
        squared_error <- (prediction[BeginingOfTestData:EndOfTestData,] - y.testing)^2

        # Mean squared error
        mse <- colMeans(squared_error)

        # Save MSE
        MinimumLambdaRollingMSE[ , j ] <- mse

      }

        # Take mean of MSE to get mean MSE for each lambda across all CV runs
        cvMSE <- rowMeans(MinimumLambdaRollingMSE)

        # Calculate standard error of MSE
        # length of y.testing*number of runs is N for SE calculation.
        cvSE <- sqrt(cvMSE/(TrainingPostPeriodLength*MaxCrossValidations-1))

        # Extract 1) lambda that minimizes cvMSE, 2) max lambda that prodcuses an MSE within one standard error of the minimum 3) and median min lambda.
        lambda <- getOptcv.scul(lambdapath = lambdapath,
                      mse = cvMSE,
                      se = cvSE,
                      fullMSE = MinimumLambdaRollingMSE)

      # Based on options, pick cross validated lambda
       CrossValidatedLambda <- switch(cvOption,
                                      lambda.1se = lambda$lambda.1se,
                                      lambda.min = lambda$lambda.min,
                                      lambda.median = lambda$lambda.median,
       )

      # Run the actual fit
      EntirePretreatmentFit = glmnet(x = x.DonorPool.PreTreatment,
                                     y = y.PreTreatment)

      # Extract the exact prediction for the cross-validated lambda
      y.scul <- predict(x = x.DonorPool.PreTreatment,
                        y = y.PreTreatment,
                        newx = as.matrix(x.DonorPool),
                        EntirePretreatmentFit,
                        s = CrossValidatedLambda,
                        exact = TRUE)

      # Extract the exact coefficients used
      coef.exact = coef(x = x.DonorPool.PreTreatment,
                        y = y.PreTreatment,EntirePretreatmentFit,
                        s = CrossValidatedLambda,
                        exact = TRUE)

      # Calculate the standard deviation of the outcome variable in the pre-treatment period
      PreTreatmentSD <- sd(y.PreTreatment[1:TreatmentBeginsAt-1])

      # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
      CohensD <- abs(y.scul[1:(TreatmentBeginsAt-1),]-y.actual[1:(TreatmentBeginsAt-1),])/PreTreatmentSD

      # Calculate the mean pre-treatment cohen's D
      MeanCohensD <- mean(CohensD)

  # unlist y.actual
  y.actual <- as.numeric(unlist(y.actual))
  time <- as.numeric(unlist(time))

  # Wrap all of the items we will need for later into a list.
  OutputDataSCUL <- list(
    time = time,
    y.actual = y.actual,
    y.scul = y.scul,
    CrossValidatedLambda = CrossValidatedLambda,
    TreatmentBeginsAt = TreatmentBeginsAt,
    CohensD = MeanCohensD,
    coef.exact = coef.exact
  )
  # Return list
  return(OutputDataSCUL)
  # TODO (hollina): Figure out why y.actual and time are coming in as a list.
}
