#' Make smoke plot
#'
#' Plot standardized differences of all placebo goods and target good.
#'
#' @param  y.StandardizedDifference The standardized difference between target data and synthetic data series across time. Default is SCUL.pvalue$y.StandardizedDifference.
#' @param  y.placebo.StandardizedDifference Standardized difference between each placebo data series and its synthetic prediction across time. Default is SCUL.inference$y.placebo.StandardizedDifference.
#' @param  time The time variable across. Default is SCUL.input$time.
#' @param  OutputFilePath Output file path. Default is SCUL.input$OutputFilePath.
#'
#' @return graph  A smoke plot of the standardized effect size compared to placbos.
#' @import reshape2
#' @import ggplot2
#' @export
SmokePlot <- function(
  x.PlaceboPool.StandardizedDiff.full = SCUL.inference$y.placebo.StandardizedDifference.Full,
  x.PlaceboPool.CohensD = SCUL.inference$y.placebo.CohensD,
  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
  OutputFilePath = SCUL.input$OutputFilePath,
  CohensD = SCUL.input$CohensDThreshold,
  y.actual = SCUL.output$y.actual,
  y.scul = SCUL.output$y.scul
                  ) {


  ###################
  #Set up actual scul results for future comparison
  # Calculate the difference between the two
  y.difference <- data.frame(y.actual - y.scul)
  names(y.difference) <- names(y.actual)

  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  y.PreTreatmentSD <-t(apply(data.frame(y.actual[1:(TreatmentBeginsAt-1),]), 2, sd))

  # Take the absolute value of the difference between the prediction and the actual data divided by the standard deviation
  y.StandardizedDifference <- sweep(y.difference,MARGIN=2,FUN="/",STATS=y.PreTreatmentSD)

  # Calculate the Cohens D
  y.CohenD <- (t(colMeans(abs(data.frame(y.StandardizedDifference[1:(TreatmentBeginsAt-1),])))))
  y.CohenD <- data.frame(y.CohenD<=CohensD)
  # y.CohenD[1,1] <- FALSE


  ###################
  # Trim the time for the stat
  # y.StandardizedDifference.trim <- y.StandardizedDifference[StartTime:EndTime,unlist(y.CohenD[1,])]
  y.StandardizedDifference.trim <- data.frame(y.StandardizedDifference[StartTime:EndTime,])

  ###################
  #Set up placebo distribution
  placebo.distribution.trim <- x.PlaceboPool.StandardizedDiff.full[StartTime:EndTime,x.PlaceboPool.CohensD<=CohensD]
  # placebo.distribution.trim <- data.frame(placebo.distribution.full[StartTime:EndTime,cd<=CohensD])



  # reshape the placebo data to be in long form
  data_to_plot_wide_y <- cbind( SCUL.input$time, Results.y.StandardizedDiff)
  names(data_to_plot_wide_y) <- c("time", "std_diff")

  data_to_plot_wide <- cbind( SCUL.input$time, placebo.distribution.trim)
  names(data_to_plot_wide)[1] <- c("time")


  # Trim placebos with poor pre-treatment fit
  placebo.distribution.trim <- data.frame(SCUL.inference$y.placebo.StandardizedDifference.Full)[,data.frame(SCUL.inference$y.placebo.CohensD) <=.25 ]



  y.placebo.StandardizedDifference <- cbind(time,y.placebo.StandardizedDifference)
  y.StandardizedDifference <- cbind(time,y.StandardizedDifference)

  colname <-colnames(y.placebo.StandardizedDifference[1])
  melted = melt(y.placebo.StandardizedDifference, id.vars=colname)

  # Plot all the differences
  BasePlot <- ggplot() +
    geom_line(data=melted, aes(x=melted[,1], y=value, group=variable), alpha=.1, size=.5) +
    geom_line()



  BasePlot <- BasePlot +
    geom_line(data = y.StandardizedDifference, aes(x=y.StandardizedDifference[,1] ,y = y.StandardizedDifference[,2],color='outline'),size=1.1,linetype = 1) +
    geom_line(data = y.StandardizedDifference, aes(x=y.StandardizedDifference[,1] ,y = y.StandardizedDifference[,2],color='fill'),size=.75,linetype = 1) +
    scale_colour_manual(name = 'the colour', values =c('outline'='black','fill'='darkorange2'))


  SmokePlot<- BasePlot +
    theme_bw() +
    ylab("") +
    xlab(colname) +
    theme(legend.title=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")
  SmokePlot
  # Save graph
  SmokePlotPath<-paste(OutputFilePath,"smoke_plot.png",sep='')
  ggsave(SmokePlotPath, dpi=300)
  ####################
  ## Return plot
  return(SmokePlot)
}
