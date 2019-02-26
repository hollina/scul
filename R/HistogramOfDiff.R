#' Make histogram of null with effect size
#'
#' Plot a histogram of the null two different ways with estimated effect size plotted as well.
#' 
#' @param  y.StandardizedDifference The standardized difference between target data and synthetic data series across time. Default is SCUL.inference$y.StandardizedDifference.
#' @param  y.placebo.StandardizedDifference Standardized difference between each placebo data series and its synthetic prediction across time. Default is SCUL.inference$y.placebo.StandardizedDifference.
#' @param  time The time variable across. Default is SCUL.input$time.
#' @param  OutputFilePath Output file path. Default is SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt  An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param width width of histogram. Default is 20
#' @param widthBreaks width breaks histogram. Default is 5, evenly spaced.
#' @param height height of histogram. Default is 0.05
#' @param AdjustmentParm Adjustment parameter for guassian histogram. Default is 3.
#' @param BandwidthParm Bandwidth parameter for guassian histogram. Default is 3.

#' @return graphs  Two histograms with effect size
#' @import ggplot2
#' @export
HistogramOfDiff <- function(
                            y.StandardizedDifference=SCUL.pvalue$y.StandardizedDifference,
                            y.placebo.StandardizedDifference=SCUL.inference$y.placebo.StandardizedDifference,
                            time = SCUL.input$time,
                            TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
                            OutputFilePath = SCUL.input$OutputFilePath, 
                            width = 40, 
                            widthBreaks = c(-width,-width/2, 0, width/2, width),
                            height = 0.05, 
                            AdjustmentParm = 3,
                            BandwidthParm = 3
                          ) {

  PlaceboStandardizedDiff<-data.frame(colMeans(y.placebo.StandardizedDifference[TreatmentBeginsAt:nrow(y.placebo.StandardizedDifference),]))
  colnames(PlaceboStandardizedDiff)[1] <- c("coef")
  
  # Import Dataset
  caption <-paste("Note: Gaussian kernel with adjustment parameter of ",
                  AdjustmentParm,
                  " and bandwidth parameter of ",
                  BandwidthParm, 
                  ".")
  StandardizedPostTreatmentEffectSize <- mean(y.StandardizedDifference[TreatmentBeginsAt:nrow(y.placebo.StandardizedDifference),])
  CustomHistogram <-  ggplot(PlaceboStandardizedDiff, aes(x=PlaceboStandardizedDiff$coef)) + 
    geom_density( kernel = "gaussian",adjust = AdjustmentParm,bw=BandwidthParm,fill = "white")+
    geom_rug() + 
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(name="Smoothed placebo distribution of post-treatment mean standardized", 
                       limits=c(-width, width), 
                       breaks = widthBreaks) +
    scale_y_continuous(limits=c(0, height)) +
    ylab(NULL) +
    labs(caption = caption) +
    geom_vline(xintercept=StandardizedPostTreatmentEffectSize, colour="red", linetype="solid") 
  
  CustomHistogram
  
  # Save graph
  CustomHistogramPath<-paste(OutputFilePath,"custom_histogram.png",sep='')
  ggsave(CustomHistogramPath, width=6, height=4, dpi=300)  
  
  
  Defaultcaption <-"Note: Gaussian kernel with default height, width, adjustment, and bandwidth parameters." 
  DefaultHistogram <- ggplot(PlaceboStandardizedDiff, aes(x=PlaceboStandardizedDiff$coef)) + 
    geom_density( kernel = "gaussian",fill = "white")+
    geom_rug() + 
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(name="Smoothed placebo distribution of post-treatment mean standardized") +
    scale_y_continuous() +
    ylab(NULL) +
    labs(caption = Defaultcaption) +
    geom_vline(xintercept=StandardizedPostTreatmentEffectSize, colour="red", linetype="solid") 
  
  DefaultHistogram
  
  # Save graph
  DefaultHistogramPath<-paste(OutputFilePath,"default_histogram.png",sep='')
  ggsave(DefaultHistogramPath, width=6, height=4, dpi=300)  
  
  list(CustomHistogram,DefaultHistogram)
  
}
