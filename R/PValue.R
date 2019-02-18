#' Run placebo scul procedure on placebo pool and create p-values
#'
#' Runs the same scul procedure on every column in the donor pool to create
#' a rank based p-value. 
#'
#' @param y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
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
#'        
#' @return list  A list of standardized placbo effect sizes and bounds for the p-value.
#' @import glmnet
#' 
#' @export

PVaule <- function(
                    x.PlaceboPool = SCUL.input$x.PlaceboPool,
                    TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
                    PostPeriodLength = nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1,
                    PrePeriodLength = SCUL.input$TreatmentBeginsAt-1,
                    NumberInitialTimePeriods = SCUL.input$NumberInitialTimePeriods,
                    OutputFilePath = SCUL.input$OutputFilePath, 
                    CohensDThreshold = SCUL.input$CohensDThreshold,
                    y.actual = SCUL.output$y.actual,
                    y.scul = SCUL.output$y.scul
                  ) {
  ###################
  #Set up actual scul results for future comparison
  # Calculate the difference between the two
  y.difference <- y.actual - y.scul
  
  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  y.PreTreatmentSD <-sd(unlist(y.actual[1:(TreatmentBeginsAt-1),]))
  
  # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
  y.StandardizedDifference <- y.difference/y.PreTreatmentSD
  
  ####################
  # Extract relevant information from placebo data #
  x.PlaceboPool.StandardizedDiff<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=nrow(x.PlaceboPool)))
  x.PlaceboPool.CohensD<-data.frame(matrix(ncol(x.PlaceboPool),nrow=1))
  
  ###################
  # Set up variables that are constant across all runs
  
  # Initialize a matrix of 0's, that is 1 by max # of C.V. runs
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-PostPeriodLength+1
  
  # Determine stopping point
  StoppingPoint <- NumberInitialTimePeriods+MaxCrossValidations-1
  
  ###################
  # Loop over each placebo. Calculate a diff and a % diff. Then store in dataframe.
  for (h in 1:ncol(x.PlaceboPool)){
    # h<-2122
    #################
    # Create a matrix with pre-treatment values for a given placebo run
    # Here h is the target and -h is the donor pool
    
    y.PlaceboPool.PreTreatment <- as.matrix(x.PlaceboPool[(1:TreatmentBeginsAt-1),h])
    x.PlaceboPool.PreTreatment <- as.matrix(x.PlaceboPool [(1:TreatmentBeginsAt-1),-h])
    
    y.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPool[(TreatmentBeginsAt:PostPeriodLength),h])
    x.PlaceboPool.PostTreatment <- as.matrix(x.PlaceboPool [(TreatmentBeginsAt:PostPeriodLength),-h])
    
    
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
      EndOfTestData = i + PostPeriodLength
      
      # Specify testing data
      placebo.x.testing <-x.PlaceboPool.PreTreatment[BeginingOfTestData:EndOfTestData,]
      placebo.y.testing <-y.PlaceboPool.PreTreatment[BeginingOfTestData:EndOfTestData,]  
      
      # Create a predicted y value for the entire pre-treatment time period for each lambda in the the exact grid of lambdas from the training data
      prediction<-predict(fit,newx=x.PlaceboPool.PreTreatment,x=placebo.x.training, y=placebo.y.training, s=fit$lambda,exact=TRUE)
      
      ###########
      # Find the lambda that results in the minimum mean squarred error 
      
      # first take the difference from the prediction and the test data
      mse <- prediction[BeginingOfTestData:EndOfTestData,]  -placebo.y.testing
      
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
    placebo.y.scul <- predict(x=x.PlaceboPool.PreTreatment, y=y.PlaceboPool.PreTreatment, newx = as.matrix(x.PlaceboPool[,-h]), EntirePretreatmentFit,s = CrossValidatedLambda, exact = TRUE)
    
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
    print(paste((h/ncol(x.PlaceboPool))*100," percent done"))
  }
  
  x.PlaceboPool.StandardizedDiff.trimmed<-x.PlaceboPool.StandardizedDiff[,x.PlaceboPool.CohensD<=CohensDThreshold]
  
  PValue.low<-1-((sum(abs(colMeans(x.PlaceboPool.StandardizedDiff.trimmed[TreatmentBeginsAt:nrow(y.actual),])) <= mean(abs(y.StandardizedDifference[TreatmentBeginsAt:nrow(y.actual),])))-1)/ncol(x.PlaceboPool.StandardizedDiff.trimmed))
  PValue.high<-1-((sum(abs(colMeans(x.PlaceboPool.StandardizedDiff.trimmed[TreatmentBeginsAt:nrow(y.actual),])) <= mean(abs(y.StandardizedDifference[TreatmentBeginsAt:nrow(y.actual),]))))/ncol(x.PlaceboPool.StandardizedDiff.trimmed))
  
  # Wrap all of the items we will need for later into a list.
  OutputDataPValue <- list(
    y.StandardizedDifference = y.StandardizedDifference,
    y.placebo.StandardizedDifference = x.PlaceboPool.StandardizedDiff.trimmed,
    PValue.low = PValue.low,
    PValue.high = PValue.high
  )
  # Return list
  return(OutputDataPValue)  
}