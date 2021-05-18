#' Plot a smoothed density plot of the null distribution
#'
#' Determines the rank of the mean absolute value of the standardized treatment effect
#' using any placebo analyses that survive a user specified Cohen's D threshold
#' ways to use the placebo distribution to calculate a mean value (e.g., mean p-value or another test statistic).
#' This is a simple example of one such way.
#'
#' @param CohensD A real number greater than 0, indicating the Cohen's D threshold at which
#'                     fit is determined to be "poor". The difference is in standard deviation units. Default is SCUL.input$CohensDThreshold.
#' @param StartTime The begining time period for which the average pseduo treatment effect is calculated. T
#' @param EndTime The end time period for which the average pseduo treatment effect is calculated. T
#' @param width width of histogram. Default is 20
#' @param height height of histogram. Default is 0.05
#' @param AdjustmentParm Adjustment parameter for guassian histogram. Default is 3.
#' @param BandwidthParm Bandwidth parameter for guassian histogram. Default is 3.
#' @param OutputFilePath Output file path. Default is  SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt  An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param y_label label for the y-axis
#' @param x_label label for the x-axis
#' @param title_label Graph title
#' @param subtitle_label  Graph sub-title
#' @param rejection_label Label for the top of the red rejection region
#' @param save.figure Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE
#'
#' @return graph  A denisty plot of the null distribution conditional on satisfactory fit
#' @import ggplot2
#' @import stats
#'
#' @export

PlotNullDistribution <- function(
  CohensD = 999,
  StartTime = TreatmentBeginsAt,
  EndTime = nrow(SCUL.output$y.actual),
  width = 5,
  height = 2.76,
  AdjustmentParm = 1,
  BandwidthParm = .25,
  y_label = "",
  subtitle_label = "",
  x_label = "Distribution of standardized difference\n for placebo donor pool",
  title_label = "Cohen's-D restriction: None",
  rejection_label = "Rejection region\n for null hypothesis\n of no treatment effect",
  save.figure = FALSE,
  OutputFilePath = SCUL.input$OutputFilePath,
  TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt
) {


  widthBreaks  <- c(-width,-round(width/2,2), 0, round(width/2,2), width)


  #########################################################################################################
  # Create a trimmed distribution
  trim <-  SCUL.inference$y.placebo.StandardizedDifference.Full[ , SCUL.inference$y.placebo.CohensD <= CohensD]

  trim.CohensD <- trim[StartTime:EndTime,]

  test <- data.frame(colMeans(trim.CohensD, na.rm = TRUE))

  names(test) <- c("value")


  ####################
  # Where is 5% Absolute value
  test$abs <- abs(test$value)
  test <- test[order(-test$abs),]
  critical_value <- test[ceiling(.10*nrow(test)),2]


  OutputGraphName <-  ggplot() +
    geom_density(data = test, aes(x = value), kernel = "gaussian",fill = "white") +
    geom_rect(aes(ymin=0,ymax=height,xmin=-width,xmax=-critical_value),
              fill = "red", size=0.5, alpha=0.2) +
    geom_rect(aes(ymin=0,ymax=height,xmin=critical_value,xmax=width),
              fill = "red", size=0.5, alpha=0.2) +
    geom_rug(data = test, aes(x = value)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(name=x_label,
                       limits=c(-width, width),
                       breaks = widthBreaks) +
    ylab(NULL) +
    scale_y_continuous(name  = y_label, limits=c(0, height)) +
    annotate("text", x = -(critical_value+width)/2, y = height*.775,
             label = rejection_label) +
    annotate("text", x = (critical_value+width)/2, y = height*.775,
             label = rejection_label) +
    annotate("text", x = -(critical_value+width)/2, y = height*.45,
             label = paste("< ",-round(critical_value,2), sep=''), size = 6) +
    annotate("text", x = (critical_value+width)/2, y = height*.45,
             label = paste("> ",round(critical_value,2), sep=''), size = 6) +
    labs(
      title = title_label,
      subtitle = subtitle_label) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.05, size = 10)
    )

  if (save.figure == TRUE) {
    # Save graph
    PlotPath<-paste(OutputFilePath,"null_density.pdf",sep='')
    ggsave(PlotPath, width=12, height=8, dpi=300)
  }

  return(OutputGraphName)
}
