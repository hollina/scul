#' Graph output from SCUL procedure
#'
#' Plot output from the SCUL procedure in levels, differences, and standardized difference
#'
#' @param  y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
#' @param time  Default is  SCUL.output$time.
#' @param OutputFilePath  Default is  SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt  Default is  SCUL.output$TreatmentBeginsAt.
#' @param save.figure Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE
#'
#' @return plots  Three plots that compare scul results to actual data; in levels, difference, and standardized difference.
#' @import ggplot2
#' @import stats
#' @export
PlotActualvSCUL <- function(
  y.actual = SCUL.output$y.actual,
  y.scul = SCUL.output$y.scul,
  time = SCUL.output$time,
  OutputFilePath = SCUL.input$OutputFilePath,
  TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt,
  save.figure = FALSE
) {
  # Unlist all variables and store in the same format
  y.actual <- as.numeric(unlist(y.actual))
  y.scul <- as.numeric(unlist(y.scul))
  time <- as.numeric(unlist(time))

  # Calculate the difference between the two
  difference <- y.actual - y.scul

  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  PreTreatmentSD <-sd(y.actual[1:(TreatmentBeginsAt-1)])

  # Take the absolute value of the difference between the predicition and the actual data divided by the standard deviation
  StandardizedDifference <- difference/PreTreatmentSD

  # Store data to plot as dataframe, with labeled columns
  DataToPlot <- data.frame(time, y.actual, y.scul, difference, StandardizedDifference)
  colnames(DataToPlot) <- c("time", "y.actual", "y.scul",  "difference", "StandardizedDifference")

  #######################################
  ## Plot the actual next to the predicted

  TempBothPlot <- ggplot() +
    geom_line(data=DataToPlot, aes(x=time,y = y.actual,color="Actual"), size=1) +
    geom_line(data=DataToPlot, aes(x=time,y = y.scul,color="Synthetic"), size=1, linetype="dashed") +
    scale_color_manual(name = "", values = c("Actual" = "black", "Synthetic" = "grey"))+  theme_bw(base_size = 22)  +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  BothPlot <- TempBothPlot  +
    geom_vline(xintercept =DataToPlot$time[TreatmentBeginsAt-1], colour="grey", linetype = "dashed") +
    theme(legend.position="bottom") + ylab("")  +
    xlab("Time") +
    ggtitle("Actual and synthetic prediction") +
    annotate("text", x = (DataToPlot$time[TreatmentBeginsAt-1]-DataToPlot$time[1])/2+DataToPlot$time[1], y = max(y.actual)*1.01, label = "Pre-treatment",cex=6)  +
    annotate("text", x = (DataToPlot$time[length(y.actual)]-DataToPlot$time[TreatmentBeginsAt-1])/2+DataToPlot$time[TreatmentBeginsAt-1], y = max(y.actual)*1.01, label = "Post-treatment",cex=6)

  if (save.figure == TRUE) {
    # Save graph
    BothPlotPath<-paste(OutputFilePath,"actual_v_synthetic_plot.pdf",sep='')
    ggsave(BothPlotPath, width=12, height=8, dpi=300)
  }
  ####################
  ## Return plot
  return(BothPlot)
}
