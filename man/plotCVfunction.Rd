% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotCVfunction.R
\name{plotCVfunction}
\alias{plotCVfunction}
\title{Plot mean square error across potential penalty paramters, highlighting three options for cross-validated \code{lambda}}
\usage{
plotCVfunction(
  lambdapath,
  mse,
  se,
  lambda,
  minLambdas,
  save.figure = TRUE,
  OutputFilePath = SCUL.input$OutputFilePath
)
}
\arguments{
\item{lambdapath}{A grid of lambdas that is used in each cross-validation run as potential options for the optimal penalty parameter.}

\item{mse}{A vector of the average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.}

\item{se}{A vector of the standard error associated with each average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.}

\item{lambda}{Output from the \code{getOptcv.scul} function. Will have three candidate \code{lambdas}.}

\item{minLambdas}{Vector of lambdas that minimize MSE in each CV run}

\item{save.figure}{Boolean if you want to save figure. Default is to save (save.figure = TRUE).}

\item{OutputFilePath}{File path prefix if you are saving the figure. Default is file path set in \code{SCUL.inut} (OutputFilePath = SCUL.input$OutputFilePath).}
}
\description{
This is a simple adaptation of the \code{getOptcv.glmnet} function from  \code{glmnet}.
The code takes a grid of \code{lambda} penalties along with a vector of associated mean squared errors (\code{mse}), standard errors (\code{se}), and the mean squared error from each cross-validation run (\code{fullMSE}).
From this three potential options for the cross-validated penalty parameter are computed. 1) The \code{lambda} that has the minimum average mean squared error across all the cross-validation runs (\code{lambda$lambda.min}),
2) The \code{lambda} the largest \code{lambda} that is associated with an average cross-validated mean squared error within one standard error of the minimum average cross-validated mean squared error (\code{lambda$lambda.1se}), and
3) the \code{lamda} that is the median of the set of \code{lambdas} (\code{lambda$lambda.median}).
}
