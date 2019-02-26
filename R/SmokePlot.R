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
                    y.StandardizedDifference=SCUL.pvalue$y.StandardizedDifference,
                    y.placebo.StandardizedDifference=SCUL.inference$y.placebo.StandardizedDifference,
                    time = SCUL.input$time,
                    OutputFilePath = SCUL.input$OutputFilePath
                  ) {
  y.placebo.StandardizedDifference <- cbind(time,y.placebo.StandardizedDifference)
  y.StandardizedDifference <- cbind(time,y.StandardizedDifference)
  
  colname <-colnames(y.placebo.StandardizedDifference[1])
  melted = melt(y.placebo.StandardizedDifference, id.vars=colname)
  
  # Plot all the differences 
  BasePlot <- ggplot() +
    geom_line(data=melted, aes(x=melted[,1], y=value, group=variable), alpha=.1, size=.5) +
    geom_line()
  
  
  
  BasePlot <- BasePlot +
    geom_line(data = y.StandardizedDifference, aes(x=y.StandardizedDifference[,1] ,y = y.StandardizedDifference$y,color='outline'),size=1.1,linetype = 1) +
    geom_line(data = y.StandardizedDifference, aes(x=y.StandardizedDifference[,1] ,y = y.StandardizedDifference$y,color='fill'),size=.75,linetype = 1) +
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