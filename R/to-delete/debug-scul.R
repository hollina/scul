rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scul, tidyverse, glmnet, pBrackets, knitr, kableExtra, cowplot, formattable)


# Look at cigarette data
dim(cigarette_sales)
head(cigarette_sales[,1:6])

AllYData <- cigarette_sales[, 1:2]
AllXData <- cigarette_sales %>%
  select(-c("year", "cigsale_6", "retprice_6"))

processed.AllYData <- Preprocess(AllYData)

TreatmentBeginsAt <- 19 # Here the 18th row is 1988
PostPeriodLength <- nrow(processed.AllYData) - TreatmentBeginsAt + 1
PrePeriodLength <- TreatmentBeginsAt-1
NumberInitialTimePeriods <- 5
processed.AllYData <- PreprocessSubset(processed.AllYData,
                                       TreatmentBeginsAt ,
                                       NumberInitialTimePeriods,
                                       PostPeriodLength,
                                       PrePeriodLength)

SCUL.input <- OrganizeDataAndSetup (
  time =  data.frame(AllYData[, 1]),
  y = data.frame(AllYData[, 2]),
  TreatmentBeginsAt = TreatmentBeginsAt,
  x.DonorPool = AllXData[, -1],
  CohensDThreshold = 0.25,
  NumberInitialTimePeriods = NumberInitialTimePeriods,
  TrainingPostPeriodLength = 7,
  x.PlaceboPool = AllXData[, -1],
  OutputFilePath="vignette_output/"
)

PostPeriodLength <- nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1
PrePeriodLength <- SCUL.input$TreatmentBeginsAt-1
NumberInitialTimePeriods<-SCUL.input$NumberInitialTimePeriods
OutputFilePath<-SCUL.input$OutputFilePath
x.DonorPool.PreTreatment<-SCUL.input$x.DonorPool.PreTreatment
y.PreTreatment<-SCUL.input$y.PreTreatment
x.DonorPool<-SCUL.input$x.DonorPool
time<-SCUL.input$time
y.actual<-SCUL.input$y
TreatmentBeginsAt<-SCUL.input$TreatmentBeginsAt
TrainingPostPeriodLength<-SCUL.input$TrainingPostPeriodLength
cvOption<-"lambda.1se"

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

  y.actual <- as.numeric(unlist(y.actual))
  time <- as.numeric(unlist(time))


  OutputDataSCUL <- list(
    time = time,
    y.actual = y.actual,
    y.scul = y.scul,
    CrossValidatedLambda = CrossValidatedLambda,
    TreatmentBeginsAt = TreatmentBeginsAt,
    CohensD = MeanCohensD,
    coef.exact = coef.exact
  )
  plotData <- data.frame(cbind(OutputDataSCUL$time, OutputDataSCUL$y.actual, OutputDataSCUL$y.scul))
  ggplot(plotData, aes(x = time, y = y.actual - y.scul )) +
    geom_line() +
    theme_classic()


