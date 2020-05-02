#' Graph target data series across time
#'
#' This function graphs the target data series across time.
#'
#' @param time A dataframe that is a column vector (T by 1) with the running time variable.
#'         Must be sorted by time (oldest to most recent). Default is taken from SCUL.input$time.
#' @param y A dataframe that is a column vector (T by 1) containing the target variable of interest.
#'         Must be sorted by time (oldest to most recent). Default is SCUL.input$y.
#' @param TreatmentBeginsAt An integer indicating the time period (<T) in which treatment begins.
#'        Defailt is taken from SCUL.input$TreatmentBeginsAt.
#' @param  OutputFilePath A file path to store output. Default is taken from
#'        SCUL.input$OutputFilePath
#' @param save.figure Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE
#'
#' @return FullPlotFilePath A plot of the target varibale across time, with the pre and post treatment periods clearly marked
#' @export
#' @import ggplot2
#' @import stats
#' @export
GraphTargetData <- function(
                              time = SCUL.input$time,
                              TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
                              y = SCUL.input$y,
                              OutputFilePath = SCUL.input$OutputFilePath,
                              save.figure = FALSE
                            ) {
  # Function: Graph target data across time
  #
  # Args:
  #   time : From SCUL.input. Time data in (T x 1) column vector
  #   TreatmentBeginsAt : From SCUL.input. Indicator of the first time period (i.e. row) of treatment
  #   y : From SCUL.input. Variable of interest in a (T x 1 column vector)
  #   OutputFilePath : From SCUL.input.  A file path to store output.
  #   TimeFormat: A lubridate-style time format. TODO (hollina). Add this feature.
  #
  # Dependencies:
  #   ggplot2
  #
  # Returns:
  #   A saved pdf of the target data plotted across time


  ####################
  ## Graph pre and post treatment data

  ## Below for any random dataset. Use above if you want exact dates and provide them.
  # Create a dataframe of the treated data
  temp.DataToPlot <- data.frame(time,y)

  # Label the columns
  colnames(temp.DataToPlot) <- c("time", "y.actual")

  # Create a binary if it is in the post treatment period
  temp.DataToPlot$posttreat <- 0
  temp.DataToPlot$posttreat[(TreatmentBeginsAt):nrow(y)] <- 1

  # Plot the data
  plot <- ggplot(
    data=temp.DataToPlot, aes(x=temp.DataToPlot$time,y = temp.DataToPlot$y.actual)) +
    geom_line(aes(color=posttreat)) +
    theme_bw(base_size = 22) +
    theme(
      panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    geom_vline(xintercept =temp.DataToPlot$time[TreatmentBeginsAt], colour="grey", linetype = "dashed") +
    theme(legend.position="none") +
    ylab("") +
    xlab("") +
    annotate("text", x = (temp.DataToPlot$time[TreatmentBeginsAt]-temp.DataToPlot$time[1])/2+temp.DataToPlot$time[1], y = max(temp.DataToPlot$y.actual)*1.01, label = "Pre-treatment", cex=6) +
    annotate("text", x = (temp.DataToPlot$time[length(temp.DataToPlot$time)]-temp.DataToPlot$time[TreatmentBeginsAt])/2+temp.DataToPlot$time[TreatmentBeginsAt], y = max(temp.DataToPlot$y.actual)*1.01, label = "Post-treatment", cex=6) +
    ggtitle("Actual data across time")

  if (save.figure == TRUE) {
    # Save the plot
    FullPlotFilePath<-paste(OutputFilePath,"actual_y_plot.pdf",sep='')
    ggsave(FullPlotFilePath, width=12, height=8, dpi=300)
  }


  ####################
  ## Return plot
  return(plot)

  ####################
  # Remove temp data used to create this plot
  rm(temp.DataToPlot, plot, FullPlotFilePath)

  # TODO (hollina) : Add ability to put custom label on Y axis.

}
