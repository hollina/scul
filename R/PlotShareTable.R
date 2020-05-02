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
#' @param save.figure Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE
#'
#' @return graphs  Four plots that show contribution of each donor to synthetic estimate and a similar table.
#' @import ggplot2
#' @import formattable
#' @import tidyr
#' @import htmltools
#' @import webshot
#' @import stats
#'
#' @export
PlotShareTable <- function(
  coef.exact=SCUL.output$coef.exact,
  x.DonorPool = SCUL.input$x.DonorPool,
  y.scul = SCUL.output$y.scul,
  y.actual = SCUL.output$y.actual,
  time = SCUL.output$time,
  OutputFilePath = SCUL.input$OutputFilePath,
  TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt,
  save.figure = FALSE
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
  # Keep only the parts we need for the table
  ShareOfPredictionForTable<-ShareOfPredictionForPlot[,1:4]
  ShareOfPredictionForTable<-ShareOfPredictionForTable[,-3]
  ShareOfPredictionForTable[,1]<-round(ShareOfPredictionForTable[,1],digits=2)
  ShareOfPredictionForTable[,2]<-round(ShareOfPredictionForTable[,2],digits=2)
  ShareOfPredictionForTable[,3]<-round(ShareOfPredictionForTable[,3],digits=2)
  colnames(ShareOfPredictionForTable) <- c("Share for\n First Prediction" ,"Share for Most\n Recent Prediction","Coefficient")

  # Make it so the coef is green/red for positive/negative
  sign_formatter <- formatter("span",
                              style = x ~ style(color = ifelse(x > 0, "green",
                                                               ifelse(x < 0, "red", "black"))))

  # Plot the
  SharesInTable<-formattable(ShareOfPredictionForTable,align =c("r","r","r"), list(
    area(col = c("Share for\n First Prediction", "Share for Most\n Recent Prediction")) ~ normalize_bar(color = "#3399FF", 0.2),
    "Coefficient" = sign_formatter
  ))
  SharesInTable
  if (save.figure == TRUE) {

    # Make sure it's 300 dpi
    desire_width_px <- 720
    desire_height_px <- 480
    dpi <- 300
    fig.width <- desire_width_px / dpi
    fig.height <- desire_height_px / dpi

    # Save graph
    SharesInTablePath<-paste(OutputFilePath,"share_for_both_time_table.png",sep='')
    export_formattable <- function(f, file, width = "100%", height = NULL,
                                   background = "white", delay = 0.2)
    {
      w <- as.htmlwidget(f, width = width, height = height)
      path <- html_print(w, background = background, viewer = NULL)
      url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
      webshot(url,
              file = file,
              selector = ".formattable_widget",
              delay = delay,
              zoom=4)
    }
    export_formattable(f=SharesInTable,file=SharesInTablePath )
  }
  ####################
  ## Return plot
  return(SharesInTable)
}
