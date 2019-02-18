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
#'         
#' @return OutputDataSCUL  A list with the synthetic series, weights used to construct the series, and measures of fit.
#' @import glmnet
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
                  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt
                ) {
  # Calculate the maximum number of possible runs given the previous three choices
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-PostPeriodLength+1
  
  # Determine stopping point
  StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1
  
  # Initialize a matrix of 0's, that is 1 by max # of C.V. runs
  MinimumLambdaRolling<-matrix(0,1,MaxCrossValidations)
  
  # Preform the task once for the correct number of times
  for (i in NumberInitialTimePeriods:StoppingPoint){
    j = i -NumberInitialTimePeriods + 1
    # Specify training data
    x.training <-x.DonorPool.PreTreatment[1:i,]
    y.training <-y.PreTreatment[1:i,]
    
    # Run a lasso with only the training data. It will run on for a grid of lambdas by default
    fit=glmnet(x.training,y.training)
    
    # Extract the entire grid of lambdas from the training data
    LambdaList = fit$lambda
    
    # Determine when the testing data begins for this CV run
    BeginingOfTestData = i +1
    
    # Determine when the testing data ends for this CV run
    EndOfTestData = i + PostPeriodLength
    
    # Specify testing data
    x.testing <-x.DonorPool.PreTreatment[BeginingOfTestData:EndOfTestData,]
    y.testing <-y.PreTreatment[BeginingOfTestData:EndOfTestData,]  
    
    # Create a predicted y value for the entire pre-treatment time period for each lambda in the the exact grid of lambdas from the training data
    prediction<-predict(fit,newx=x.DonorPool.PreTreatment,x=x.training, y=y.training, s=fit$lambda,exact=TRUE)
    
    
    # Find the lambda that results in the minimum mean squarred error 
    
    # first take the difference from the prediction and the test data
    mse <- prediction[BeginingOfTestData:EndOfTestData,]  -y.testing
    
    # Then square each term
    for (z in 1:length(mse)) {
      mse[z] <- mse[z]^2
    }
    
    # Now findind the minimum squarred error, how we do this depends on the length of the post-period
    # Due to how glmnet stores the optimal lambda, we need an if statement for different post-period lengths. If the post-period is only one time period, it will be stored as a single number and if it's longer it will be a vector. The if statement makes sure it is stores as the same type no matter what
    if (PostPeriodLength < 2) {
      # Create a vector, that dat (a sinlge column)
      newV <- as.vector(mse[,1])
      # extract the lamda that minimizes the prediction error
      MinimumLambdaRolling[j]<- LambdaList[which.min(newV)]
    }
    
    if (PostPeriodLength > 1) {
      # extract the lamda that minimizes the prediction error summed across each time period in the test data
      MinimumLambdaRolling[j]<-LambdaList[which.min(colSums(mse))]
    }
    
    #Display the output. Useful for debugging. 
    print("The training data is running from 1 to")
    print(i)
    print("The testing data is running from")
    print(BeginingOfTestData)
    print("to")
    print(EndOfTestData)
    print("The minimum lambda is")
    print(MinimumLambdaRolling[j])
  }
  
  # Calculate cross-validated lambda
  CrossValidatedLambda<-median(MinimumLambdaRolling)
  
  # Run the actual fit
  EntirePretreatmentFit=glmnet(x.DonorPool.PreTreatment,y.PreTreatment)
  
  # Extract the exact prediction for the cross-validated lambda
  y.scul <- predict(x=x.DonorPool.PreTreatment, y=y.PreTreatment, newx = as.matrix(x.DonorPool), EntirePretreatmentFit,s = CrossValidatedLambda, exact = TRUE)
  
  # Extract the exact coefficients used
  coef.exact = coef(x=x.DonorPool.PreTreatment,y=y.PreTreatment,EntirePretreatmentFit, s = CrossValidatedLambda, exact = TRUE)
  
  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  PreTreatmentSD <-sd(y.PreTreatment[1:TreatmentBeginsAt-1])
  
  # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
  CohensD <- abs(y.scul[1:(TreatmentBeginsAt-1),]-y.actual[1:(TreatmentBeginsAt-1),])/PreTreatmentSD
  
  # Calculate the mean pre-treatment cohen's D
  MeanCohensD <- colMeans(CohensD)
  
  # Wrap all of the items we will need for later into a list.
  OutputDataSCUL <- list(
    time = time,
    y.actual = y.actual,
    y.scul = y.scul,
    CrossValidatedLambda = CrossValidatedLambda,
    TreatmentBeginsAt = TreatmentBeginsAt,
    CohensD = MeanCohensD,
    coef.exact =coef.exact
  )
  # Return list
  return(OutputDataSCUL)  
  # TODO (hollina): Make sure that y.actual and y.scul are the same type. Can't figure it out for now.
}