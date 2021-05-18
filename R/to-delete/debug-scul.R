rm(list=ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scul, ggrepel, caret, tidyverse, glmnet, pBrackets, knitr, kableExtra, cowplot, formattable)

files.sources = paste0("R/", list.files("R/"))
sapply(files.sources, source)

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
cvOption<-"lambda.1se"

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

# Plot CV
plotCV = TRUE
if (plotCV == TRUE) {
  plot.cv(lambdapath = lambdapath,
          mse = cvMSE,
          se = cvSE,
          lambda = lambda,
          save.figure = TRUE)
}


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
