rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scul, ggrepel, caret, tidyverse, glmnet, pBrackets, knitr, kableExtra, cowplot, formattable)

#files.sources = paste0("R/", list.files("R/"))
#sapply(files.sources, source)

# Make it so cigarette_sales is stored as data.frame
cigarette_sales <- data.frame(cigarette_sales)

# Look at cigarette data
dim(cigarette_sales)
head(cigarette_sales[,1:6])

AllYData <- cigarette_sales  %>%
  select(c("year", "cigsale_6"))
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

y <- Preprocess(AllYData %>% select(cigsale_6))


SCUL.input <- OrganizeDataAndSetup (
  time =  AllYData %>% select(year),
  y = AllYData %>% select(cigsale_6),
  TreatmentBeginsAt = TreatmentBeginsAt,
  x.DonorPool = AllXData,
  CohensDThreshold = 0.25,
  NumberInitialTimePeriods = NumberInitialTimePeriods,
  TrainingPostPeriodLength = 7,
  x.PlaceboPool = AllXData,
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
cvOption<-"lambda.median"

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
epsilon = .01 # ifelse(nrow(sx) < nvars, 0.01, 1e-04) # default used in glmnet

lambdapath <- round(exp(seq(log(max(MaxLambdaList)), log(min(MaxLambdaList)*epsilon),
                            length.out = MaxLambdasInGrid)), digits = 10)
# Initialize a matrix of 0's, that is # of lambdas in grid by max # of C.V. runs
# MinimumLambdaRolling <- matrix(0, MaxLambdasInGrid, MaxCrossValidations)

# Initialize a matrix of 0's, that is # of lambdas in grid by max # of C.V. runs
#if (cvPath != lambda.median) {
MinimumLambdaRollingMSE <- matrix(0, MaxLambdasInGrid, MaxCrossValidations)
#}
#if (cvPath == lambda.median) {
MinimumLambdaRollingMSE.median <- matrix(0, 1, MaxCrossValidations)
#}
# Preform lasso for each cross-validation run and store test MSE for each run for each lambda in grid
for (i in NumberInitialTimePeriods:StoppingPoint){
  #i <- NumberInitialTimePeriods
  j = i -NumberInitialTimePeriods + 1

  # Specify training data
  x.training <-x.DonorPool.PreTreatment[1:i,] # this had me switch row and column. Why?
  y.training <-y.PreTreatment[1:i] # this had me switch row and column. Why?

  # Run a lasso with only the training data. It will run on for a grid of lambdas by default
  fit = glmnet(x.training, y.training)

  # Determine when the testing data begins for this CV run
  BeginingOfTestData = i +1

  # Determine when the testing data ends for this CV run
  EndOfTestData = i + TrainingPostPeriodLength

  # Specify testing data
  x.testing <-x.DonorPool.PreTreatment[BeginingOfTestData:EndOfTestData,]
  y.testing <-y.PreTreatment[BeginingOfTestData:EndOfTestData,]

  # When using the non-default lambda path (i.e., extending it to 100. we get a warning)
  # Create a predicted y value for the entire pre-treatment time period for each lambda in the the exact grid of lambdas from the training data
  #if (cvPath != lambda.median) {
  prediction <- predict(fit,
                        newx = x.DonorPool.PreTreatment,
                        x = x.training,
                        y = y.training,
                        s = lambdapath,
                        exact = TRUE)
  #}
  #if (cvPath == lambda.median) {
  prediction.median <- predict(fit,
                               newx = x.DonorPool.PreTreatment,
                               x = x.training,
                               y = y.training,
                               s = fit$lambda,
                               exact = TRUE)
  #}
  # Squared error
  squared_error <- (prediction[BeginingOfTestData:EndOfTestData,] - y.testing)^2
  squared_error.median <- (prediction.median[BeginingOfTestData:EndOfTestData,] - y.testing)^2

  # Mean squared error
  mse <- colMeans(squared_error)
  #if (cvPath != lambda.median) {
  # Save MSE
  MinimumLambdaRollingMSE[ , j ] <- mse
  #}
  #if (cvPath == lambda.median) {
  # Save MSE
  MinimumLambdaRollingMSE.median[ j ] <- fit$lambda[which.min(mse)]
  #}
}
medianLambda = median(MinimumLambdaRollingMSE.median)
#if (cvPath != lambda.median) {
# Take mean of MSE to get mean MSE for each lambda across all CV runs
cvMSE <- rowMeans(MinimumLambdaRollingMSE)

# Calculate standard error of MSE for each lambda
cvSE <- apply(MinimumLambdaRollingMSE, 1, sd)

# Extract 1) lambda that minimizes cvMSE, 2) max lambda that prodcuses an MSE within one standard error of the minimum 3) and median min lambda.
lambda <- getOptcv.scul(lambdapath = lambdapath,
                        mse = cvMSE,
                        se = cvSE,
                        fullMSE = MinimumLambdaRollingMSE,
                        medianLambda = medianLambda)


lambdapath <- lambdapath
                          mse <- cvMSE
                          se <- cvSE
                          lambda <- lambda
                          save.figure = TRUE
               minLambdas <-MinimumLambdaRollingMSE.median
                         OutputFilePath <- '/Users/hollinal/Desktop/'

  # Create a dataframe of the data we want to plot
  plotdata <- data.frame(cbind(lambdapath, mse, se))
  names(plotdata) <- c("lambda", "MSE", "SE") # Add names
  plotdata$lambda <- -log(plotdata$lambda) # Take the negative of the penalty parameter

  # Create a mini-dataframe of the optimal lambdas
  plotLambda <- data.frame(Ref = c("Min MSE", "Median Lambda", "1 SE"),
                           vals = c(-log(lambda$lambda.min), -log(lambda$lambda.median), -log(lambda$lambda.1se)),
                           lcols = c("red", "blue", "black"),
                           ltype = c('solid', 'dashed', 'longdash'),
                           stringsAsFactors = FALSE)

  # Make plot
  cvPLOT <-  ggplot(data = plotdata, aes(x = lambda, y = MSE)) +
    geom_vline(data = plotLambda,
               mapping = aes(xintercept = vals,
                             linetype = ltype,
                             color = lcols),
               show.legend = FALSE,
               size = 1, alpha = .75) +
    geom_errorbar(data = plotdata, aes(ymin = MSE - SE, ymax = MSE + SE), width=.25) +
    geom_point(data = plotdata, aes(x = lambda, y = MSE), size = 3) +
    theme_classic(base_size = 22) +
    labs(title = "Cross-validated mean squared error vs. penalty parameter",
         x = "-Log(Lambda)",
         y = "Mean squared error") +
    geom_label_repel(mapping = aes(x = vals,
                                   y = max(plotdata$MSE + plotdata$SE),
                                   label = Ref,
                                   hjust = 1,
                                   vjust = 0),
                     data = plotLambda) +
    geom_rug(data = data.frame(-log(t(minLambdas))) %>% select( medianLambda = 1, everything() ),
             aes(x = medianLambda),
             inherit.aes = F,
             sides="b",
             size = 4,
             alpha = .1)
# Plot CV
# plotCV = TRUE
# if (plotCV == TRUE) {
#   plotCVfunction(lambdapath = lambdapath,
#           mse = cvMSE,
#           se = cvSE,
#           lambda = lambda,
#           save.figure = TRUE,
#          OutputFilePath = '/Users/hollinal/Desktop/')
# }
#

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
                  y = y.PreTreatment,
                  EntirePretreatmentFit,
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

SCUL.output <- list(
    time = time,
    y.actual = y.actual,
    y.scul = y.scul,
    CrossValidatedLambda = CrossValidatedLambda,
    TreatmentBeginsAt = TreatmentBeginsAt,
    CohensD = MeanCohensD,
    coef.exact = coef.exact,
    lambda = lambda
  )
  plotData <- data.frame(cbind(SCUL.output$time, SCUL.output$y.actual, SCUL.output$y.scul))
  ggplot(plotData, aes(x = time, y = y.actual - y.scul )) +
    geom_line() +
    theme_classic()
# Make share table
  PlotShareTable()

# Statement to drop last two characters if they match the target seris
  drop_vars_from_same_FIPS <-
    "select(-ends_with(substring(names(x.PlaceboPool)[h],nchar(names(x.PlaceboPool)[h]) - 2 + 1, nchar(names(x.PlaceboPool)[h]))))"

  #######################################################
  pacman::p_load(parallel, doParallel, progress, foreach)

  x.PlaceboPool <- SCUL.input$x.PlaceboPool
  TreatmentBeginsAt <- SCUL.input$TreatmentBeginsAt
  PostPeriodLength <- nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1
  PrePeriodLength <- SCUL.input$TreatmentBeginsAt-1
  NumberInitialTimePeriods <- SCUL.input$NumberInitialTimePeriods
  OutputFilePath <- SCUL.input$OutputFilePath
  CohensDThreshold <- SCUL.input$CohensDThreshold
  TrainingPostPeriodLength <- SCUL.input$TrainingPostPeriodLength
  DonorPoolRestrictionForEachPlacebo <- ""
  cvOption <- "lambda.1se"
  parallel <- TRUE

  ####################
  # Extract relevant information from placebo data #
  x.PlaceboPool.StandardizedDiff<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=nrow(x.PlaceboPool)))
  x.PlaceboPool.CohensD<-data.frame(matrix(ncol=ncol(x.PlaceboPool),nrow=1))



  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  numCores <- detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  # Progress combine function
  # https://gist.github.com/kvasilopoulos/d49499ea854541924a8a4cc43a77fed0
  f <- function(iterator){
    pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0
    function(...) {
      count <<- count + length(list(...)) - 1
      setTxtProgressBar(pb, count)
      flush.console()
      cbind(...) # this can feed into .combine option of foreach
    }
  }


  ###################
  # Loop over each placebo. Calculate a diff and a % diff. Then store in dataframe.
  #for (h in 1:ncol(x.PlaceboPool)){
  foreach(h = 1:ncol(x.PlaceboPool), .combine = f(ncol(x.PlaceboPool))) %dopar% {
    #summary(rnorm(1e6))[3]
    #}
    # initTime[h] <- Sys.time()
    # h <- 1
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
    placeboSCUL <- scul::SCUL(
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
    PreTreatmentSD <- sd(y.PlaceboPool.PreTreatment)

    # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
    x.PlaceboPool.StandardizedDiff[,h] <- (placeboSCUL$y.scul - placeboSCUL$y.actual)/PreTreatmentSD
    colnames(x.PlaceboPool.StandardizedDiff)[h] <- h

    # Calculate the mean pre-treatment cohen's D
    x.PlaceboPool.CohensD[1, h] <- placeboSCUL$CohensD

    # Display Progress bar
    #setTxtProgressBar(progressBar, h)
    #endTime[h] <- Sys.time()

    #elapsedTime <- round(lubridate::seconds_to_period(sum(endTime - initTime)), 0)

    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    #estTime <- ncol(x.PlaceboPool) * (mean(endTime[endTime != 0] - initTime[initTime != 0])) - elapsedTime
    #remaininingTime <- round(lubridate::seconds_to_period(estTime), 0)

    #cat(paste(" // Elapsed time:", elapsedTime,
    #         " // Est. time remaining:", remaininingTime), "")
    # pbTracker(pb,h,numCores)
    print(h)
  }
  stopCluster(cl)
  ##########################################################################
SCUL.inference <- CreatePlaceboDistribution(
    DonorPoolRestrictionForEachPlacebo = drop_vars_from_same_FIPS
  )

PValue(
  CohensD = 999,
  StartTime = SCUL.input$TreatmentBeginsAt,
  EndTime = nrow(Results.y.StandardizedDiff)
)

PValue(
  CohensD = .25,
  StartTime = SCUL.input$TreatmentBeginsAt,
  EndTime = nrow(Results.y.StandardizedDiff)
)

data_for_traditional_scm <- pivot_longer(data = cigarette_sales,
                                         cols = starts_with(c("cigsale_", "retprice_")),
                                         names_to = c("variable", "fips"),
                                         names_sep = "_",
                                         names_prefix = "X",
                                         values_to = "value",
                                         values_drop_na = TRUE
)

StandardizedDiff <- data.frame(abs(SCUL.output$y.actual-SCUL.output$y.scul)/PreTreatmentSD)
names(StandardizedDiff) <- c("scul.cv")

TreatmentEffect <- data.frame(SCUL.output$y.actual-SCUL.output$y.scul)
names(TreatmentEffect) <- c("scul.cv")

data_for_traditional_scm <- pivot_wider(data = data_for_traditional_scm,
                                        names_from = variable,
                                        values_from = value)


data_for_traditional_scm$fips <- as.numeric(data_for_traditional_scm$fips)

data(synth.data)

# create matrices from panel data that provide inputs for synth()
data_for_traditional_scm$idno = as.numeric(as.factor(data_for_traditional_scm$fips))  # create numeric country id required for synth()

data_for_traditional_scm <- as.data.frame(data_for_traditional_scm)

dataprep.out<-
  dataprep(
    foo = data_for_traditional_scm,
    predictors = c("retprice"),
    predictors.op = "mean",
    dependent = "cigsale",
    unit.variable = "idno",
    time.variable = "year",
    special.predictors = list(
      list("cigsale", 1970, "mean"),
      list("cigsale", 1980, "mean"),
      list("cigsale", 1985, "mean")
    ),
    treatment.identifier = unique(data_for_traditional_scm$idno[data_for_traditional_scm$fips==6]),
    controls.identifier = unique(data_for_traditional_scm$idno[data_for_traditional_scm$fips!=6]),
    time.predictors.prior = c(1970:1987),
    time.optimize.ssr = c(1970:1987),
    time.plot = 1970:1997
  )

## run the synth command to identify the weights
## that create the best possible synthetic
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
#round(synth.out$solution.w,2)
# contains the unit weights or
#synth.out$solution.v
## contains the predictor weights.

## the output from synth opt
## can be flexibly combined with
## the output from dataprep to
## compute other quantities of interest
## for example, the period by period
## discrepancies between the
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
)


StandardizedDiff$traditional_scm <- (dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
))/PreTreatmentSD

# Show Cohen's D for each of these
CohensD <- data.frame(colMeans(abs(StandardizedDiff[1:(TreatmentBeginsAt-1),])))


# Calculate treatment effect for each of these
TreatmentEffect <- data.frame(colMeans(StandardizedDiff[TreatmentBeginsAt:nrow(StandardizedDiff),])*PreTreatmentSD)

## also there are three convenience functions to summarize results.
## to get summary tables for all information
## (V and W weights plus balance btw.
## treated and synthetic control) use the
## synth.tab() command
#ynth.tables <- synth.tab(
#  dataprep.res = dataprep.out,
#  synth.res = synth.out)
#print(synth.tables)

## to get summary plots for outcome trajectories
## of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
#path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
#gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)



StandardizedDiff<-cbind(StandardizedDiff, SCUL.input$time)
names(StandardizedDiff)[length(names(StandardizedDiff))] <- "time"

# create smoke plot
difference_plot <- ggplot() +
  theme_classic() +
  geom_line(data = StandardizedDiff, aes(x = time, y = -scul.cv), alpha = 1, size = 2., color = "black") +
  geom_line(data = StandardizedDiff, aes(x = time, y = -scul.cv), alpha = 1, size = 1.75, color = "#4dac26") +
  geom_line(data = StandardizedDiff, aes(x = time, y = traditional_scm), alpha = 1, size = 2., color = "black") +
  geom_line(data = StandardizedDiff, aes(x = time, y = traditional_scm), alpha = 1, size = 1.75, color = "orange", linetype = "dashed") +
  geom_vline(
    xintercept = SCUL.input$time[TreatmentBeginsAt,1],
    linetype = "dashed",
    size = 1,
    color = "grey37"
  ) +
  labs(
    title = "Difference between actual cigarette sales and synthetic predictions\n from SCUL (green-solid)\n and a traditional synthetic control method (orange-dashed)",
    x = "Time",
    y = "Difference between actual data and predictions\n in pre-treatment standard deviations for each product"
  ) +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),
    title = element_text(size = 12)
  )

# Display graph
difference_plot
