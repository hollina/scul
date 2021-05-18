rm(list=ls())
## Load library and generate some data to illustrate:
library("glmnet")
library("ggrepel")

set.seed(1225)
n <- 1000
x1 <- matrix(rnorm(n*10000), n, 10000)
y1 <- rnorm(n) + x1[ ,1]*3 + x1[ ,2]*-1 + x1[ , 3]*1

x2 <- matrix(rnorm(n*10000), n, 10000)
y2 <- rnorm(n) + x2[ ,1]*3 + x2[ ,2]*-1 + x2[ , 3]*1

## Standardize variables: (need to use n instead of (n-1) as denominator)
source("R/mysd.R")

sx1 <- scale(x1, scale = apply(x1, 2, mysd))
#sx1 <- as.matrix(sx1, ncol = 20, nrow = 100)

sx2 <- scale(x2, scale = apply(x2, 2, mysd))
#sx2 <- as.matrix(sx2, ncol = 20, nrow = 100)


## Calculate lambda path (first get lambda_max):
lambda_max <- max(abs(colSums(sx1*y1)))/n
epsilon <- .0001
K <- 100

lambdapath <- round(exp(seq(log(lambda_max), log(lambda_max*epsilon),
                            length.out = K)), digits = 10)

## Run GLMnet
fitGLM1 <- glmnet(sx1[1:500,], y1[1:500], lambda = lambdapath)
fitGLM2 <- glmnet(sx1[1:600,], y1[1:600], lambda = lambdapath)
fitGLM3 <- glmnet(sx1[1:700,], y1[1:700], lambda = lambdapath)


prediction1 <- suppressWarnings(
                predict(fitGLM1,
                      newx = sx1[701:800,] ,
                      x = sx1[1:500,],
                      y =  y1[1:500],
                      s = lambdapath,
                      exact = TRUE,
                      ties = min)
                )

prediction2 <- predict(fitGLM2,
                       newx = sx1[701:800,] ,
                       x = sx1[1:600,],
                       y =  y1[1:600],
                       s = lambdapath,
                       exact = TRUE)
prediction3 <- predict(fitGLM3,
                       newx = sx1[701:800,] ,
                       x = sx1[1:700,],
                       y =  y1[1:700],
                       s = lambdapath,
                       exact = TRUE)
# Squared error
squared_error1 <- (prediction1 - y1[701:800])^2
squared_error2 <- (prediction2 - y1[701:800])^2
squared_error3 <- (prediction3 - y1[701:800])^2

# Mean squared error
mse1 <- colMeans(squared_error1)
mse2 <- colMeans(squared_error2)
mse3 <- colMeans(squared_error3)

MinimumLambdaRollingMSE <- rbind(mse1, mse2, mse3)

# Joint MSE
# Take mean of MSE to get mean MSE for each lambda across all CV runs
cvMSE <- colMeans(MinimumLambdaRollingMSE)

# Calculate standard error of MSE
# length of y.testing*number of runs is N for SE calculation.
cvSE <- sqrt(cvMSE/(100*3-1))

# Function to get optimal cv
source("R/getOptcv.scul")

# Extract 1) lambda that minimizes cvMSE, 2) max lambda that prodcuses an MSE within one standard error of the minimum 3) and median min lambda.
lambda <- getOptcv.scul(lambdapath = lambdapath,
              mse = cvMSE,
              se = cvSE,
              fullMSE = MinimumLambdaRollingMSE)

cvOption <- "lambda.1se"
CrossValidatedLambda <- switch(cvOption,
                               lambda.1se = lambda$lambda.1se,
                               lambda.min = lambda$lambda.min,
                               lambda.median = lambda$lambda.median,
)
CrossValidatedLambda

# plot.cv: Function to plot mean CV with error bars across lambda grid
source("R/plot.cv.R")


plot.cv(lambdapath = lambdapath,
        mse = cvMSE,
        se = cvSE,
        lambda = lambda,
        save.figure = TRUE,
        OutputFilePath = '/Users/hollinal/Desktop/')

EntirePretreatmentFit = glmnet(x = x2,
                               y = y2)

# Extract the exact prediction for the cross-validated lambda
y.scul <- predict(x = x1,
                  y = y1,
                  newx = x2,
                  EntirePretreatmentFit,
                  s = CrossValidatedLambda,
                  exact = TRUE)

# Extract the exact coefficients used
coef.exact = coef(x = x2,
                  y = y2,
                  EntirePretreatmentFit,
                  s = CrossValidatedLambda,
                  exact = TRUE)
head(coef.exact)
