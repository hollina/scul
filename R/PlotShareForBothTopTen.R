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
PlotShareForBothTopTen <- function(
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
  # Label the first column
  #colnames(x.DonorPoolWithIntercept) <- c("Intercept")
  
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
  ShareOfPredictionForPlot$order<-as.numeric(ShareOfPredictionForPlot$order)
  # Reshape the data so we can plot both on the same graph easily. 
  # Here value.2 indicates the most recent time period
  
  ShareOfPredictionForPlot.long <- ShareOfPredictionForPlot %>% gather(time_period, share, share.1:share.2)
  ShareOfPredictionForPlot.long$order<-as.numeric(ShareOfPredictionForPlot.long$order)
  
  #replace names as missing for first value (so they don't plot twice)
  ShareOfPredictionForPlot.long$RowNames[ShareOfPredictionForPlot$time_period=="share.1"]<-""
  
  # Select custom colors for the outline
  custom_colors <- c("#FF0000", "#5BBCD6")
  
  # Indicate where the labels should be right aligned
  ShareOfPredictionForPlot.long$zero <- -.01
  
  # Redo the graph for just the top ten
  
  # Rearrange share of prediction so top 20 are the top 10 in the first year
  ShareOfPredictionForPlot.long<-arrange(ShareOfPredictionForPlot.long,desc(ShareOfPredictionForPlot.long$order))
  
  # Initialize the graph
  graph <- ggplot(data=ShareOfPredictionForPlot.long[1:20,],aes(x=share, y=order,label=RowNames, color=factor(sign)))
  
  # Create a forloop to generate custome "minor grid-lines." The allows us to stop them so they don't get in the way of reading the variable names   
  for (i in seq(from=max(ShareOfPredictionForPlot.long$order)-10, to=max(ShareOfPredictionForPlot.long$order), by=5)) {
    graph <- graph + geom_segment(x=-0.01, y=i, xend=(floor(max(ShareOfPredictionForPlot.long$share)/.05)+1)*.05, yend=i, linetype="dashed", color="grey")
  }
  
  for (i in seq(from=.025, to=(floor(max(ShareOfPredictionForPlot.long$share)/.05)+1)*.05, by=.025)) {
    graph <- graph + geom_segment(x=i, y=max(ShareOfPredictionForPlot.long$order)-10, xend=i, yend=max(ShareOfPredictionForPlot.long$order), linetype="solid", color="grey")
  }
  graph <- graph +
    geom_segment(x=0, y=max(ShareOfPredictionForPlot.long$order)-10, xend=0, yend=max(ShareOfPredictionForPlot.long$order)+1, linetype="solid", color="black")
  
  graph <- graph +
    geom_segment(x=0, y=max(ShareOfPredictionForPlot.long$order)-9.45, xend=(floor(max(ShareOfPredictionForPlot.long$share)/.05)+1)*.05, yend=max(ShareOfPredictionForPlot.long$order)-9.45, linetype="solid", color="black")
  
  # Continue with the graph, adding both points and the labels
  graph <- graph +
    geom_point(aes(shape=time_period, fill=factor(sign)), size = 6, alpha=.6) +
    geom_text(data = ShareOfPredictionForPlot.long[1:20,], aes(x=ShareOfPredictionForPlot.long$zero[1:20],y=ShareOfPredictionForPlot.long$order[1:20], label = ShareOfPredictionForPlot.long$RowNames[1:20]), size = 6, adj=1, color="black")
  
  # Continue with the graph, altering the appearance, axes, and limits
  graph <- graph +
    theme_bw(base_size = 22) +
    theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line=element_blank(),axis.ticks.y=element_blank()) 
  
  # Continue with the graph, altering the shapes, legends, titles, etc
  graph <- graph +
    ylab("") +
    xlab("Contribution Percent [0-1]") +
    ggtitle("Contribution of Each Donor Variable to Synthetic Prediction") +
    theme(legend.position="bottom") 
  
  
  # Continue with the graph, altering the color
  ShareForBothTopTen  <- graph +
    scale_shape_manual(values=c(24, 21),name="Treatment Period", labels=c("First", "Most Recent")) +
    scale_fill_manual(values = c("white", "black"),name="Direction of Contribution", labels=c("Positive", "Negative")) +
    guides(fill = guide_legend(override.aes=list(shape=22),title.position = "top",title.theme = element_text(size = 20,angle = 0))) + guides(shape = guide_legend(title.position = "top",title.theme = element_text(size = 20,angle = 0))) +
    scale_color_manual(values = c("black", "black"),name="Direction of Contribution", labels=c("Positive", "Negative")) +
    theme(legend.key.size = unit(2, 'lines')) +
    scale_x_continuous( breaks=seq(0,(floor(max(ShareOfPredictionForPlot.long$share)/.05)+1)*.05,.05), limits=c(-.15,max(ShareOfPredictionForPlot.long$share[1:20])))
  
  ShareForBothTopTen
  
  # Save graph
  ShareForBothTopTenPath<-paste(OutputFilePath,"share_for_both_time_top_ten.pdf",sep='')
  ggsave(ShareForBothTopTenPath, width=12, height=8, dpi=300)
  ####################
  ## Return plot
  return(ShareForBothTopTen)
}