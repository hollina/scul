#' Graph output from SCUL procedure
#'
#' Plot output from the SCUL procedure in levels, differences, and standardized difference
#' Must also have run this command. webshot::install_phantomjs()
#'
#' @param y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
#' @param time  Default is  SCUL.output$time.
#' @param OutputFilePath Output file path. Default is  SCUL.input$OutputFilePath.
#' @param TreatmentBeginsAt   An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param coef.exact The sparse matrix of coefficients used to construc the synthetic estimate. 
#'                   Default is SCUL.output$coef.exact.
#' @param x.DonorPool The (T by K) donor pool matrix. Default is SCUL.input$x.DonorPool.
#'         
#' @return graphs  Four plots that show contribution of each donor to synthetic estimate and a similar table.
#' @import ggplot2
#' @import formattable
#' @import tidyr
#' @import htmltools
#' @import webshot
#' 
#' @export
PlotShareForFirst <- function(
  coef.exact=SCUL.output$coef.exact,
  x.DonorPool = SCUL.input$x.DonorPool,
  y.scul = SCUL.output$y.scul,
  y.actual = SCUL.output$y.actual,
  time = SCUL.output$time,
  OutputFilePath = SCUL.input$OutputFilePath,
  TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt
) {
  
  # Make the exact coefficients are stored as 
  coef.exact<-as.matrix(coef.exact)
  
  # Add an intercept variable (always takes the value of 1) to the donor pool
  x.DonorPoolWithIntercept <- as.matrix(data.frame(rep(1,nrow(x.DonorPool)),x.DonorPool))
  
  # Multiply the coefficients by the value in at each time period
  ContributionToPrediction <- sweep(t(x.DonorPoolWithIntercept),MARGIN=1,FUN="*",STATS=coef.exact)
  ContributionToPrediction <- t(ContributionToPrediction)
  
  # Determine sign of coefficient
  SignOfContribution <- ContributionToPrediction
  SignOfContribution[SignOfContribution>0] <-  1
  SignOfContribution[SignOfContribution<0] <- 2 #This is in this numeric order on purpose. When there are only positive weights the graphic defaults to showing them in the "first" category. So we want that to be the lower number here. 
  SignOfContribution<-SignOfContribution[ , !apply(SignOfContribution==0,2,all)]
  
  # Take the absolute value of everything since we are concerned with contribution
  ContributionToPrediction <- abs(ContributionToPrediction)
  
  # Without the intercept, percentage of contribution each variable makes to the prediction
  ShareOfPrediction <-sweep(ContributionToPrediction,MARGIN=1,FUN="/",STATS=rowSums(ContributionToPrediction))
  #rowSums(ShareOfPrediction)
  
  ## Remove zero contributions
  #Value_of_prediction<-value_of_prediction[ , !apply(share_of_prediction==0,2,all)]
  ShareOfPrediction<-ShareOfPrediction[ , !apply(ShareOfPrediction==0,2,all)]
  coef.exact<-coef.exact[apply(coef.exact,1,function(x) all(abs(x)>0))]
  
  ## Show the top five items in the time treatment began without intercept
  # Note: Percents are [0-1]
  #head(sort(ShareOfPrediction[SCUL.input$TreatmentBeginsAt,], decreasing=TRUE))
  
  ## Create a data frame with the share of prediction in the time of treatment and the last time observed.
  ShareOfPredictionForPlot<-data.frame(ShareOfPrediction[TreatmentBeginsAt,],ShareOfPrediction[length(y.actual),],SignOfContribution[TreatmentBeginsAt,],coef.exact)
  
  # Make sure the first row is called intercept
  rownames(ShareOfPredictionForPlot)[1]<-"Intercept"
  
  #create a variable from the row names
  ShareOfPredictionForPlot$names=row.names(ShareOfPredictionForPlot)
  
  #label the columns
  colnames(ShareOfPredictionForPlot) <- c("share.1","share.2","sign", "coef", "RowNames")
  
  # sort by the contribution in the first time
  ShareOfPredictionForPlot<-ShareOfPredictionForPlot[order(-abs(ShareOfPredictionForPlot$share.1)),]
  
  # Create a variable that stores this order
  for (i in 1:nrow(ShareOfPredictionForPlot)) {
    ShareOfPredictionForPlot$order[i]<- nrow(ShareOfPredictionForPlot)-i+1
  }
  
  # Plot the results for the first time
  ShareForFirst <- ggplot(data=ShareOfPredictionForPlot,aes(share.1, order,label=RowNames)) +
    geom_point() + 
    geom_text(data = ShareOfPredictionForPlot, aes( label = ShareOfPredictionForPlot$RowNames),
              size = 6, hjust = -.15) +
    theme_bw(base_size = 22) +
    theme(panel.border = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    ylab("") +
    xlab("Percent [0-100]") +
    ggtitle("Contribution of Donor Variable to Synthetic Prediction in the First time of Treatment") +
    scale_x_continuous(limits = c(0, max(ShareOfPredictionForPlot$share.1)+.1) ) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())
  
  # Show the graph
  ShareForFirst
  
  # Save graph
  ShareForFirstPath<-paste(OutputFilePath,"share_for_first_time_time_period.pdf",sep='')
  ggsave(ShareForFirstPath, width=12, height=8, dpi=300)
  ####################
  ## Return plot
  return(ShareForFirst)
}