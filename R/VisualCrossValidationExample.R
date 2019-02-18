#' Illustrate rolling-origin k-fold cross-validation
#'
#'  Makes visual depiction of rolling-origin k-fold cross-validation to be used to find lambda penalty
#'
#' @param PostPeriodLength An integer that indicates the length of the post-treatment period.
#'        Defailt is calculated using SCUL.input data.  
#' @param PrePeriodLength An integer that indicates the length of the pre-treatment period.
#'        Defailt is calculated using SCUL.input data. 
#' @param NumberInitialTimePeriods An integer that indicates the number of time periods desired in the training data for the first cross-validation run. 
#'        Default is the stated amount in SCUL.input data. 
#' @param OutputFilePath A file path to store output. Default is taken from 
#'        SCUL.input$OutputFilePath
#'
#' @return graph  A visual depiction of rolling-origin k-fold cross-validation to be used to find lambda penalty
#' @import pBrackets
#' @export
VisualCrossValidationExample <- function(
                                          PostPeriodLength = nrow(SCUL.input$y)-SCUL.input$TreatmentBeginsAt+1,
                                          PrePeriodLength = SCUL.input$TreatmentBeginsAt-1,
                                          NumberInitialTimePeriods = SCUL.input$NumberInitialTimePeriods,
                                          OutputFilePath = SCUL.input$OutputFilePath
                                        ) {
  # Function: Make visual depiction of rolling-origin k-fold cross-validation to be used to find lambda penalty
  #
  # Args:
  #   time : From SCUL.input. Time data in (T x 1) column vector
  #   TreatmentBeginsAt : From SCUL.input. Indicator of the first time period (i.e. row) of treatment
  #   y : From SCUL.input. Variable of interest in a (T x 1 column vector)
  #   OutputFilePath : From SCUL.input.  A file path to store output.
  #   TimeFormat: A lubridate-style time format. TODO (hollina). Add this feature.
  #
  # Dependencies:
  #   pBrackets
  #
  # Returns:
  #   A saved pdf of the target data plotted across time
  
  # TODO (hollina): Add restriction to set up that min number must be great than one.
  
  # The portion of the code that plots the figure is adapted from code from Rob J Hyndman's GitHub on 27 July 2018; https://gist.github.com/robjhyndman
  # Seen first on his website; https://robjhyndman.com/hyndsight/tscv/
  
  # Not sure what these do yet. 
  #par(mar=c(0,0,0,0))
  
  # Calculate the maximum number of possible runs given the previous three choices
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-PostPeriodLength+1
  
  # Set up the limits of the plot and astetics. (spelling?)
  plot(0,0,xlim=c(-3.75,PrePeriodLength+2.5),ylim=c(-.25,1.3),
       xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
  
  # Set colors (train, test, left-out)
  # From Darjeeling Limited color palette, https://github.com/karthik/wesanderson
  #custom_colors <- c("#F98400", "#00A08A", "#5BBCD6")
  CustomColors <- c("black", "white", "red")
  # Loop through all the possible cross validations
  for(j in 1:MaxCrossValidations)
  {
    # Identify the possible set of test data: From one after the training data ends until the end of the pre-treatment period. 
    RangeOfFullSetOfTestData <- (NumberInitialTimePeriods+j):PrePeriodLength #7:20 
    
    # Identify the actual set of test data: From one after the training data ends until the end of test data 
    RangeOfjthTestData <- (NumberInitialTimePeriods+j):(NumberInitialTimePeriods+j+PostPeriodLength-1)  
    
    # Identify the actual set of data left out: From one after the test data ends until the end of pre-treatment data 
    RangeOfLeftoutData <- (NumberInitialTimePeriods+j+PostPeriodLength):PrePeriodLength
    
    #  Identify the training data. From the first one until adding (J-1).
    RangeOfTrainingData <- 1:(NumberInitialTimePeriods-1+j)
    
    # Put arrows through the data points to represent time
    arrows(0,1-j/MaxCrossValidations,PrePeriodLength+1,1-j/MaxCrossValidations,0.05)
    
    # Add squares to all of the training data
    points(RangeOfTrainingData,rep(1-j/MaxCrossValidations,length(RangeOfTrainingData)),pch=15,col=CustomColors[1])
    
    # Add X's to the unused data
    if(length(RangeOfFullSetOfTestData) > PostPeriodLength)
      points(RangeOfLeftoutData, rep(1-j/MaxCrossValidations,length(RangeOfLeftoutData)), pch=4, col=CustomColors[3])
    
    # Add circles to the test data
    if(length(RangeOfFullSetOfTestData) >= PostPeriodLength)
      points(RangeOfjthTestData,rep(1-j/MaxCrossValidations,length(RangeOfjthTestData)), pch=21, col="black",bg=CustomColors[2])
  }
  # Add informative text and bracket
  
  ## label what is training data
  brackets(1, 1 , NumberInitialTimePeriods, 1, h=.05)
  text((NumberInitialTimePeriods+1)/2,1.15,"Training data\n for 1st CV")
  
  ## label what is test data
  brackets((NumberInitialTimePeriods+1),1 , (NumberInitialTimePeriods+PostPeriodLength), 1, h=.05)
  text((NumberInitialTimePeriods+PostPeriodLength)-((PostPeriodLength-1)/2),1.15,"Test data\n for 1st CV")
  
  ## label what is left-out data
  brackets(NumberInitialTimePeriods+PostPeriodLength+1,1 , PrePeriodLength, 1, h=.05)
  text((PrePeriodLength-(PrePeriodLength-NumberInitialTimePeriods-PostPeriodLength)/2),1.15,"Left-out data\n for 1st CV")
  
  
  ## Add a legend so it will be clear in black and white
  
  legend( 
    x="bottom",
    legend=c("Training", "Testing","Left-out"),
    bg=CustomColors,
    col=c(CustomColors[1], "black", CustomColors[3]),
    lwd=1,
    lty=c(0,0), 
    pch=c(15,21,4),
    bty="n",
    horiz = TRUE,
    x.intersp=.5,
    y.intersp=.5,
    text.width=c(.5,.5,.5)
  )
  
  # Add custom x-axis labels to indicate time until treatment
  text(PrePeriodLength/2,-.12,"Time period relative to treatment", cex=1)
  CustomXLables <-seq((-PrePeriodLength),0, by=1)
  for (z in seq(from=1, to=PrePeriodLength+1, by=5)) {
    text(z,-.05,CustomXLables[z], cex=1)
  }
  
  # Add custom y-axis labels to indicate CV run number
  text(-3.5,1.1,bquote(underline("CV Run")), cex=1)
  for (z in seq(from=0, to=1-1/MaxCrossValidations, by=(1/MaxCrossValidations))) {
    TempLabel = MaxCrossValidations - z*MaxCrossValidations 
    text(-3.5,z,TempLabel, cex=1)
  }
  
  # Add title
  #title(main = "Example of rolling origin cross-validation procedure",line = -.8)
  text(-3.75, 1.3, "Example of rolling-origin k-fold cross-validation procedure",cex=1.75,adj=0)
  
  # Save graph
  FullPlotPath<-paste(OutputFilePath,"visual_illustration_of_cross_validation.pdf",sep='')
  dev.copy(pdf,FullPlotPath, width=12, height=8)
  dev.off()
  
  ####################
  ## Return plot
  return(pdf)
  
  # TODO (hollina) : Increase font size.
  
}