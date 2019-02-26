#' Calculate a simple bounded p-value for the treatment effect
#'
#' Determines the rank of the mean absolute value of the standardized treatment effect 
#' of the target product relative to the placebo distribution. There are many alternative 
#' ways to use the placebo distribution to calculate a mean value (e.g., mean p-value or another test statistic). 
#' This is a simple example of one such way.
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
#' @param x.PlaceboPool.StandardizedDiff.trimmed A (T by L), where L<=J)  data frame containing all products that are included in the placebo distribution
#'         Trimmed by Cohen's D using CreatePlaceboDistribution function. Default is SCUL.inference$x.PlaceboPool.StandardizedDiff.trimmed
#'        
#' @return list  The bounds for the rank based p-value based upon rank of mean absolute value of post-treatment standardized effect against null distribution. 
#' @import glmnet
#' 
#' @export

PValue <- function(
                    x.PlaceboPool.StandardizedDiff.trimmed = SCUL.inference$y.placebo.StandardizedDifference,
                    TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
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
  # Calculate p-values #
  PValue.low<-1-((sum(abs(colMeans(x.PlaceboPool.StandardizedDiff.trimmed[TreatmentBeginsAt:nrow(y.actual),])) <= mean(abs(y.StandardizedDifference[TreatmentBeginsAt:nrow(y.actual),])))-1)/ncol(x.PlaceboPool.StandardizedDiff.trimmed))
  PValue.high<-1-((sum(abs(colMeans(x.PlaceboPool.StandardizedDiff.trimmed[TreatmentBeginsAt:nrow(y.actual),])) <= mean(abs(y.StandardizedDifference[TreatmentBeginsAt:nrow(y.actual),]))))/ncol(x.PlaceboPool.StandardizedDiff.trimmed))
  
  # Wrap all of the items we will need for later into a list.
  OutputDataPValue <- list(
    y.StandardizedDifference = y.StandardizedDifference,
    PValue.low = PValue.low,
    PValue.high = PValue.high
  )
  # Return list
  return(OutputDataPValue)  
}