rm(list=ls())
## Load library and generate some data to illustrate:
library("glmnet")
set.seed(1234)
n <- 100
x1 <- matrix(rnorm(n*3), n, 3)
y1 <- rnorm(n) + x1[ ,1]*3 + x1[ ,2]*-1 + x1[ , 3]*.5

x1a <- x1[1:50 , ]
x1b <- x1[51:100 , ]

x2 <- matrix(rnorm(n*3), n, 3)
y2 <- rnorm(n) + x2[ ,1]*3 + x2[ ,2]*-1 + x2[ , 3]*.5

## Standardize variables: (need to use n instead of (n-1) as denominator)
mysd <- function(z) sqrt(sum((z-mean(z))^2)/length(z))
sx1 <- scale(x1, scale = apply(x1, 2, mysd))
#sx1 <- as.matrix(sx1, ncol = 20, nrow = 100)

sx2 <- scale(x2, scale = apply(x2, 2, mysd))
#sx2 <- as.matrix(sx2, ncol = 20, nrow = 100)


## Calculate lambda path (first get lambda_max):
lambda_max <- max(abs(colSums(sx1*y1)))/n
epsilon <- .0001
K <- 45

lambdapath <- round(exp(seq(log(lambda_max), log(lambda_max*epsilon),
                            length.out = K)), digits = 10)

## Run GLMnet
fitGLM1 <- glmnet(sx1[1:50,], y1[1:50], lambda = lambdapath)
fitGLM2 <- glmnet(sx1[1:60,], y1[1:60], lambda = lambdapath)
fitGLM3 <- glmnet(sx1[1:70,], y1[1:70], lambda = lambdapath)


prediction1 <- predict(fitGLM1,
                      newx = sx1[71:80,] ,
                      x = sx1[1:50,],
                      y =  y1[1:50],
                      s = lambdapath,
                      exact = TRUE)

prediction2 <- predict(fitGLM2,
                       newx = sx1[71:80,] ,
                       x = sx1[1:60,],
                       y =  y1[1:60],
                       s = lambdapath,
                       exact = TRUE)
prediction3 <- predict(fitGLM3,
                       newx = sx1[71:80,] ,
                       x = sx1[1:70,],
                       y =  y1[1:70],
                       s = lambdapath,
                       exact = TRUE)
# Squared error
squared_error1 <- (prediction1 - y1[61:70])^2
squared_error2 <- (prediction2 - y1[61:70])^2
squared_error3 <- (prediction3 - y1[61:70])^2

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
cvSE <- sqrt(cvMSE/(20-1))

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
