#' Calculate a simple bounded p-value for the treatment effect reporting the larger end of the bound
#'
#' Determines the rank of the mean absolute value of the standardized treatment effect
#' of the target product relative to the placebo distribution. There are many alternative
#' ways to use the placebo distribution to calculate a mean value (e.g., mean p-value or another test statistic).
#' This is a simple example of one such way.
#'
#' @param y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
#' @param CohensD A real number greater than 0, indicating the Cohen's D threshold at which
#'                     fit is determined to be "poor". The difference is in standard deviation units. Default is SCUL.input$CohensDThreshold.
#' @param StartTime The begining time period for which the average pseduo treatment effect is calculated. T
#' @param EndTime The end time period for which the average pseduo treatment effect is calculated. T
#' @param OutputFilePath Output file path. Default is  SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt  An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param x.PlaceboPool.full A (T by L), where L<=J)  data frame containing all products that are included in the placebo distribution
#'        Default is SCUL.inference$y.placebo.StandardizedDifference.Full
#' @param x.PlaceboPool.CohensD A (1 by L)  data frame containing all pre-period Cohen's D fit statistic for each placebo unit.
#'        Default is SCUL.inference$y.placebo.CohensD,
#'
#'
#' @return list  The bounds for the rank based p-value based upon rank of mean absolute value of post-treatment standardized effect against null distribution.
#' @import glmnet
#' @import stats
#'
#' @export

PValue <-  function(
  x.PlaceboPool.full = SCUL.inference$y.placebo.StandardizedDifference.Full,
  x.PlaceboPool.CohensD = SCUL.inference$y.placebo.CohensD,
  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
  OutputFilePath = SCUL.input$OutputFilePath,
  CohensD = SCUL.input$CohensDThreshold,
  y.actual = SCUL.output$y.actual,
  y.scul = SCUL.output$y.scul,
  StartTime = SCUL.input$TreatmentBeginsAt,
  EndTime = nrow(SCUL.output$y.scul)
) {


  ###################
  #Set up actual scul results for future comparison
  # Calculate the difference between the two
  y.difference <- data.frame(y.actual - y.scul)
  #names(y.difference) <- names(y.actual)

  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  y.PreTreatmentSD <- sd(y.actual[1:(TreatmentBeginsAt-1)])

  # Take the absolute value of the difference between the prediction and the actual data divided by the standard deviation
  y.StandardizedDifference <- y.difference/y.PreTreatmentSD

  # Calculate the Cohens D
  y.CohenD <- abs(y.StandardizedDifference[1:(TreatmentBeginsAt-1),])
  y.CohenD <- data.frame(y.CohenD<=CohensD)
  # y.CohenD[1,1] <- FALSE


  ###################
  # Trim the time for the stat
  # y.StandardizedDifference.trim <- y.StandardizedDifference[StartTime:EndTime,unlist(y.CohenD[1,])]
  y.StandardizedDifference.trim <- data.frame(y.StandardizedDifference)

  ###################
  #Set up placebo distribution
  placebo.distribution.trim <- x.PlaceboPool.full[ , x.PlaceboPool.CohensD <= CohensD]
  # placebo.distribution.trim <- data.frame(placebo.distribution.full[StartTime:EndTime,cd<=CohensD])

  ####################
  # Calculate p-values #
  PValue.batch.low <- data.frame(matrix(ncol=ncol(y.scul),nrow=1))
  names(PValue.batch.low) <- names(y.actual)
  PValue.batch.high <- data.frame(matrix(ncol=ncol(y.scul),nrow=1))
  names(PValue.batch.high) <- names(y.actual)

  for (e in 1:ncol(y.StandardizedDifference.trim)) {
    if (y.CohenD[1,e]==TRUE) {
      PValue.batch.high[1,e] <- sum(abs(colMeans(placebo.distribution.trim))>abs(mean(data.frame(y.StandardizedDifference.trim)[,e])))/ncol(placebo.distribution.trim)
      PValue.batch.low[1,e] <-(sum(abs(colMeans(placebo.distribution.trim))>abs(mean(data.frame(y.StandardizedDifference.trim)[,e])))-1)/ncol(placebo.distribution.trim)
    }
  }

  # High estimate as default
  ColumnPValues <-data.frame(t(PValue.batch.high))
  ColumnPValues$sym <-""
  for (e in 1:ncol(y.StandardizedDifference.trim)) {
    if (is.na(ColumnPValues[e,1])==FALSE){
      if (ColumnPValues[e,1]<=.1) {
        ColumnPValues$sym[e] <- ""
      }
      if (ColumnPValues[e,1]<=.05) {
        ColumnPValues$sym[e] <- ""
      }
      if (ColumnPValues[e,1]<=.01) {
        ColumnPValues$sym[e] <- ""
      }
    }
  }

  ColumnPValues$forExport <- paste0(round(ColumnPValues[,1],digits=2),ColumnPValues[,2])
  for (e in 1:nrow(ColumnPValues)) {
    if (ColumnPValues[e,3]=="NA"){
      ColumnPValues[e,3]<-""
    }
  }

  # Wrap all of the items we will need for later into a list.
  OutputDataPValue <- data.frame(ColumnPValues[,3])
  names(OutputDataPValue) <- c("p.value.high")


  # Return list
  return(OutputDataPValue)
}
