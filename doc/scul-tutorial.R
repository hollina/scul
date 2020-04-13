## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scul, tidyverse, glmnet, pBrackets, knitr, kableExtra, cowplot, formattable, Synth)

## -----------------------------------------------------------------------------
dim(cigarette_sales)

head(cigarette_sales[,1:6])

## -----------------------------------------------------------------------------
AllYData <- cigarette_sales[, 1:2]
AllXData <- cigarette_sales %>%
  select(-c("year", "cigsale_6", "retprice_6"))

## -----------------------------------------------------------------------------
processed.AllYData <- Preprocess(AllYData)

## -----------------------------------------------------------------------------
TreatmentBeginsAt <- 19 # Here the 18th row is 1988
PostPeriodLength <- nrow(processed.AllYData) - TreatmentBeginsAt + 1
PrePeriodLength <- TreatmentBeginsAt-1
NumberInitialTimePeriods <- 5
processed.AllYData <- PreprocessSubset(processed.AllYData,
                                       TreatmentBeginsAt ,
                                       NumberInitialTimePeriods,
                                       PostPeriodLength,
                                       PrePeriodLength)

## -----------------------------------------------------------------------------
SCUL.input <- OrganizeDataAndSetup (
    time =  data.frame(AllYData[, 1]),
    y = data.frame(AllYData[, 2]),
    TreatmentBeginsAt = TreatmentBeginsAt,
    x.DonorPool = AllXData[, -1],
    CohensDThreshold = 0.25,
    NumberInitialTimePeriods = NumberInitialTimePeriods,
    TrainingPostPeriodLength = 7,
    x.PlaceboPool = AllXData[, -1],
    OutputFilePath="vignette_output/"
)

## ---- fig.height=8, fig.width=12, warning = FALSE, fig.align = "center", out.width = '100%'----
    GraphTargetData()

## ---- echo = FALSE------------------------------------------------------------
first_guess_lasso = glmnet(as.matrix(SCUL.input$x.DonorPool[(1:TreatmentBeginsAt-1),]), as.matrix(SCUL.input$y[(1:TreatmentBeginsAt-1),]))

## ---- fig.height=5.33, fig.width=8, warning = FALSE, fig.align = "center", echo = FALSE, out.width = '100%'----
plot(first_guess_lasso, "lambda", label = TRUE )

## ---- fig.height=6, fig.width=10, warning = FALSE, fig.align = "center", echo = FALSE, out.width = '100%'----

# Create a dataframe of the treated data
data_to_plot_scul_vary_lambda <- data.frame(SCUL.input$time, SCUL.input$y)

# Label the columns
colnames(data_to_plot_scul_vary_lambda) <- c("time", "actual_y")

# Create four naive predictions that are based on random lambdas
data_to_plot_scul_vary_lambda$naive_prediction_1  <-
  predict(
    x = as.matrix(SCUL.input$x.DonorPool[(1:TreatmentBeginsAt-1),]),
    y = as.matrix(SCUL.input$y[(1:TreatmentBeginsAt-1),]),
    newx = as.matrix(SCUL.input$x.DonorPool),
    first_guess_lasso,
    s = first_guess_lasso$lambda[1],
    exact = TRUE
  )

data_to_plot_scul_vary_lambda$naive_prediction_2  <-
  predict(
    x = as.matrix(SCUL.input$x.DonorPool[(1:TreatmentBeginsAt-1),]),
    y = as.matrix(SCUL.input$y[(1:TreatmentBeginsAt-1),]),
    newx = as.matrix(SCUL.input$x.DonorPool),
    first_guess_lasso,
    s = first_guess_lasso$lambda[round(length(first_guess_lasso$lambda)/10)],
    exact = TRUE
  )


data_to_plot_scul_vary_lambda$naive_prediction_3  <-
  predict(
    x = as.matrix(SCUL.input$x.DonorPool[(1:TreatmentBeginsAt-1),]),
    y = as.matrix(SCUL.input$y[(1:TreatmentBeginsAt-1),]),
    newx = as.matrix(SCUL.input$x.DonorPool),
    first_guess_lasso,
    s = first_guess_lasso$lambda[round(length(first_guess_lasso$lambda)/5)],
    exact = TRUE
  )

data_to_plot_scul_vary_lambda$naive_prediction_4  <-
  predict(
    x = as.matrix(SCUL.input$x.DonorPool[(1:TreatmentBeginsAt-1),]),
    y = as.matrix(SCUL.input$y[(1:TreatmentBeginsAt-1),]),
    newx = as.matrix(SCUL.input$x.DonorPool),
    first_guess_lasso,
    s = 0,
    exact = TRUE
  )

# Plot these variables
lasso_plot <-
  ggplot() +
  geom_line(data = data_to_plot_scul_vary_lambda,
            aes(x = time, y = actual_y, color="Real Data"), size=1, linetype="solid")  +
  geom_line(data = data_to_plot_scul_vary_lambda,
            aes(x = time, y = naive_prediction_1, color = "#1; Removes all donors"), size=1, linetype="dashed") +
  geom_line(data = data_to_plot_scul_vary_lambda,
            aes(x = time, y = naive_prediction_2, color="#2"), size = 1, linetype="twodash")  +
  geom_line(data = data_to_plot_scul_vary_lambda, aes(x = time, y = naive_prediction_3,color = "#3"), size = 1, linetype = "longdash")  +
  geom_line(data = data_to_plot_scul_vary_lambda, aes(x = time, y = naive_prediction_4,color = "#4; Removes no donors"), size = 1, linetype="dotdash")  +
  scale_color_manual(name = "",
                     values = c("#1; Removes all donors" = "blue", "Real Data" = "#F21A00", "#2" = "#00A08A", "#3" = "#EBCC2A", "#4; Removes no donors" = "#0F0D0E")) +  
  theme_bw(base_size = 15)  +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"))  +  
  geom_vline(xintercept = data_to_plot_scul_vary_lambda$time[TreatmentBeginsAt-1], colour="grey", linetype = "dashed")  + theme(legend.position="bottom") +
  ylab("") +
  xlab("Time") +
  ggtitle(expression("Actual data v.SCUL predictions from different"~lambda~"penalties")) +
  annotate("text", x = (data_to_plot_scul_vary_lambda$time[TreatmentBeginsAt-1]-data_to_plot_scul_vary_lambda$time[1])/2+data_to_plot_scul_vary_lambda$time[1], y = max(data_to_plot_scul_vary_lambda[,-1])*1.01, label = "Pre-treatment",cex=6) +
  annotate("text", x = (data_to_plot_scul_vary_lambda$time[nrow(SCUL.input$y)] - data_to_plot_scul_vary_lambda$time[TreatmentBeginsAt-1])/2+data_to_plot_scul_vary_lambda$time[TreatmentBeginsAt-1], y = max(data_to_plot_scul_vary_lambda[,-1])*1.01, label = "Post-treatment",cex=6) +
  guides(color=guide_legend(ncol=3))

 lasso_plot

 various_scul_options_path<-paste(SCUL.input$OutputFilePath,"various_scul_options.pdf",sep='')
 ggsave(various_scul_options_path, width=12, height=8, dpi=100)

## ---- fig.height=5, fig.width=8, warning = FALSE, fig.align = "center", echo = FALSE, warning = FALSE, , message=FALSE, results='hide', out.width = '100%'----
TrainingPostPeriodLength <- 7
  # Calculate the maximum number of possible runs given the previous three choices
  MaxCrossValidations <- PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength+1

  # Set up the limits of the plot and astetics. (spelling?)
  plot(0,0,xlim=c(-3.75,PrePeriodLength+2.5),ylim=c(-.75,1.5),
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
    RangeOfjthTestData <- (NumberInitialTimePeriods+j):(NumberInitialTimePeriods+j+TrainingPostPeriodLength-1)

    # Identify the actual set of data left out: From one after the test data ends until the end of pre-treatment data
    RangeOfLeftoutData <- (NumberInitialTimePeriods+j+TrainingPostPeriodLength):PrePeriodLength

    #  Identify the training data. From the first one until adding (J-1).
    RangeOfTrainingData <- 1:(NumberInitialTimePeriods-1+j)

    # Put arrows through the data points to represent time
    arrows(0,1-j/MaxCrossValidations,PrePeriodLength+1,1-j/MaxCrossValidations,0.05)

    # Add squares to all of the training data
    points(RangeOfTrainingData,rep(1-j/MaxCrossValidations,length(RangeOfTrainingData)),pch=15,col=CustomColors[1])

    # Add X's to the unused data
    if(length(RangeOfFullSetOfTestData) > TrainingPostPeriodLength)
      points(RangeOfLeftoutData, rep(1-j/MaxCrossValidations,length(RangeOfLeftoutData)), pch=4, col=CustomColors[3])

    # Add circles to the test data
    if(length(RangeOfFullSetOfTestData) >= TrainingPostPeriodLength)
      points(RangeOfjthTestData,rep(1-j/MaxCrossValidations,length(RangeOfjthTestData)), pch=21, col="black",bg=CustomColors[2])
  }
  # Add informative text and bracket

  ## label what is training data
  brackets(1, .9 , NumberInitialTimePeriods, .9, h=.05)
  text((NumberInitialTimePeriods+1)/2,1.15,"Training data\n for 1st CV")

  ## label what is test data
  brackets((NumberInitialTimePeriods+1), .9 , (NumberInitialTimePeriods+TrainingPostPeriodLength), .9, h=.05)
  text((NumberInitialTimePeriods+TrainingPostPeriodLength)-((TrainingPostPeriodLength-1)/2),1.15,"Test data\n for 1st CV")

  ## label what is left-out data
  brackets(NumberInitialTimePeriods+TrainingPostPeriodLength+1, .9 , PrePeriodLength, .9, h=.05)
  text((PrePeriodLength-(PrePeriodLength-NumberInitialTimePeriods-TrainingPostPeriodLength)/2),1.15,"Left-out data\n for 1st CV")


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
    y.intersp= .25 ,
    text.width=c(2.5,2.5,2.5)
  )

  # Add custom x-axis labels to indicate time until treatment
  text(PrePeriodLength/2,-.35,"Time period relative to treatment", cex=1)
  CustomXLables <-seq((-PrePeriodLength),0, by=1)
  for (z in seq(from = 1, to = PrePeriodLength + 1, by=5)) {
    text(z, -.15, CustomXLables[z], cex=1)
  }

  # Add custom y-axis labels to indicate CV run number
  text(-3.5,1,bquote(underline("CV Run")), cex=1)
  for (z in seq(from = 0, to = 1-1/MaxCrossValidations, by = (1/MaxCrossValidations))) {
    TempLabel = MaxCrossValidations - z*MaxCrossValidations
    text(-3.5,z,TempLabel, cex=1)
  }

  # Add title
  #title(main = "Example of rolling origin cross-validation procedure",line = -.8)
  text(-3.75, 1.5, "Example of rolling-origin k-fold cross-validation",cex=1.5,adj=0)

  # Save graph
  FullPlotPath<-paste0(SCUL.input$OutputFilePath,"visual_illustration_of_cross_validation.pdf")
  dev.copy(pdf,FullPlotPath, width=12, height=8)
  dev.off()

## ---- warning = FALSE, message=FALSE, results = 'hide'------------------------
SCUL.output <- SCUL()

## ---- fig.height=8, fig.width=12, warning = FALSE, fig.align = "center", out.width = '100%'----
PlotActualvSCUL()

## ---- echo = FALSE------------------------------------------------------------
# Calculate pre-treatment sd
PreTreatmentSD <- sd(unlist(SCUL.output$y.actual[1:(SCUL.input$TreatmentBeginsAt-1),]))

# Store Cohen's D in each period for cross-validated lambda
StandardizedDiff <- abs(SCUL.output$y.actual-SCUL.output$y.scul)/PreTreatmentSD
names(StandardizedDiff) <- c("scul.cv")

# Store Cohen's D in each period for max lambda
StandardizedDiff$naive_prediction_1 <- abs(data_to_plot_scul_vary_lambda$naive_prediction_1 - data_to_plot_scul_vary_lambda$actual_y)/PreTreatmentSD
StandardizedDiff$naive_prediction_2 <- abs(data_to_plot_scul_vary_lambda$naive_prediction_2 - data_to_plot_scul_vary_lambda$actual_y)/PreTreatmentSD
StandardizedDiff$naive_prediction_3 <- abs(data_to_plot_scul_vary_lambda$naive_prediction_3 - data_to_plot_scul_vary_lambda$actual_y)/PreTreatmentSD
StandardizedDiff$naive_prediction_4 <- abs(data_to_plot_scul_vary_lambda$naive_prediction_4 - data_to_plot_scul_vary_lambda$actual_y)/PreTreatmentSD

# Show Cohen's D for each of these
CohensD <- data.frame(colMeans(StandardizedDiff[1:(TreatmentBeginsAt-1),]))


# Calculate treatment effect for each of these

TreatmentEffect <- SCUL.output$y.actual-SCUL.output$y.scul
names(TreatmentEffect) <- c("scul.cv")

TreatmentEffect$naive_prediction_1 <-
  data_to_plot_scul_vary_lambda$actual_y -   
  data_to_plot_scul_vary_lambda$naive_prediction_1
TreatmentEffect$naive_prediction_2 <-
  data_to_plot_scul_vary_lambda$actual_y -
  data_to_plot_scul_vary_lambda$naive_prediction_2
TreatmentEffect$naive_prediction_3 <-
  data_to_plot_scul_vary_lambda$actual_y-
  data_to_plot_scul_vary_lambda$naive_prediction_3
TreatmentEffect$naive_prediction_4 <-  
  data_to_plot_scul_vary_lambda$actual_y -
  data_to_plot_scul_vary_lambda$naive_prediction_4


AvgTreatmentEffect <- data.frame(colMeans(TreatmentEffect[TreatmentBeginsAt:nrow(StandardizedDiff),]))

# For target variable
Results.y.CohensD <- SCUL.output$CohensD
Results.y.StandardizedDiff <- (SCUL.output$y.actual-SCUL.output$y.scul)/sd(unlist(SCUL.output$y.actual[1:(SCUL.input$TreatmentBeginsAt-1),]))
Results.y <- SCUL.output$y.scul

## ---- echo = FALSE, warning = FALSE, message=FALSE----------------------------
table_for_hux <- cbind(
  (CohensD),
  (AvgTreatmentEffect)
)
names(table_for_hux) <- c("CohensD", "ATE")

table_for_hux$name <- c("Cross-validation for determining penalty", "Naive guess 1:\n Max penalty (remove all donors)", "Naive guess 2:\n Random penalty", "Naive guess 3:\n Random penalty", "Naive guess 4:\n No penalty (include all donors)")
table_for_hux$value <- c(SCUL.output$CrossValidatedLambda, first_guess_lasso$lambda[1], first_guess_lasso$lambda[round(length(first_guess_lasso$lambda)/10)], first_guess_lasso$lambda[round(length(first_guess_lasso$lambda)/5)], 0)

table_for_hux <- table_for_hux[c(3, 4, 1,2)]


kable(table_for_hux, col.names = c("Method","Penalty parameter", "Cohens D (pre-period fit)", "ATE estimate"), digits = 2, row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
column_spec(1, width = "5cm") %>%
column_spec(2:4, width = "2cm")  %>%
  pack_rows("SCUL procedure using:", 1, 1) %>%
  pack_rows("SCUL using naive guesses for penalty", 2, 5)


## ----echo=FALSE, fig.align="center", fig.height=8, fig.width=5, message=FALSE, warning=FALSE, out.width = '100%'----
PlotShareTable()

## ---- results='hide'----------------------------------------------------------
SCUL.inference <- CreatePlaceboDistribution()

## ---- fig.align="center", fig.height=5, fig.width=8, message=FALSE, warning=FALSE, echo = FALSE, out.width = '100%'----

###############################################################################################
# Make Smoke Plot

# Trim placebos with poor pre-treatment fit
placebo.distribution.trim <- data.frame(SCUL.inference$y.placebo.StandardizedDifference.Full)[,data.frame(SCUL.inference$y.placebo.CohensD) <=.25 ]

colMax <- function(data) sapply(data, max, na.rm = TRUE)


# reshape the placebo data to be in long form
data_to_plot_wide_y <- cbind( SCUL.input$time, Results.y.StandardizedDiff)
names(data_to_plot_wide_y) <- c("time", "std_diff")

data_to_plot_wide <- cbind( SCUL.input$time, placebo.distribution.trim)
names(data_to_plot_wide)[1] <- c("time")


data_to_plot_long <- pivot_longer(data = data_to_plot_wide,
                                  cols = starts_with("X"),
   names_to = "group",
   names_prefix = "X",
   values_to = "std_diff",
   values_drop_na = TRUE
 )

# create smoke plot
smoke_plot <- ggplot(data = data_to_plot_long, aes(x = time, y = std_diff)) +
  geom_line(aes(group = group), alpha = .5, size = 1) +
  theme_classic() +
    geom_line(data = data_to_plot_wide_y, aes(x = time, y = std_diff), alpha = 1, size = 2., color = "black") +
    geom_line(data = data_to_plot_wide_y, aes(x = time, y = std_diff), alpha = 1, size = 1.75, color = "#4dac26") +
    geom_vline(
        xintercept = SCUL.input$time[TreatmentBeginsAt,1],
        linetype = "dashed",
        size = 1,
        color = "grey37"
    ) +
    labs(
        title = "Standardized difference for CA cigarette sales compared to standardized\n difference for each placebo donor product",
        x = "Time",
        y = "Difference between actual data and scul prediction\n in pre-treatment standard deviations for each product"
    ) +
    theme(
        axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        title = element_text(size = 12),
        legend.position = "none"
    )

# Save graph
SmokePlotPath<-paste0(SCUL.input$OutputFilePath,"smoke_plot.png")
ggsave(SmokePlotPath,
       plot = smoke_plot,
       width = 8,
       height = 5,
       dpi = 300,
       units = "in")

# Display graph
smoke_plot

## ---- fig.height=8, fig.width=4, warning = FALSE, fig.align = "center", out.width = '50%'----

# Plot null distribution with no restriction on pre-period fit
NullDist.Full <- PlotNullDistribution(
    CohensD = 999,
    StartTime = TreatmentBeginsAt,
    EndTime = nrow(SCUL.output$y.actual),
    height = 2,
    AdjustmentParm = 1,
    BandwidthParm = .25,
    title_label = "Placebo distribution compared\n to ATE estimate in\n pre-period standard deviations",
    y_label  = " ",
    x_label  =  "",
    subtitle_label  =  "No Cohen's D restriction",
    rejection_label  =  ""
) +
  geom_vline(
        xintercept = mean(Results.y.StandardizedDiff[TreatmentBeginsAt:nrow(Results.y.StandardizedDiff),]),
        linetype = "dashed",
        size = 1,
        color = "red")

# Plot null distribution 0.25 cohen's D restriction on pre-period fit
NullDist.25 <- PlotNullDistribution(
    CohensD = 0.25,
    StartTime = TreatmentBeginsAt,
    EndTime = nrow(SCUL.output$y.actual),
    height = 2,
    AdjustmentParm = 1,
    BandwidthParm = .25,
    y_label  = "",
    x_label  =  "Distribution of standardized difference\n for placebo donor pool",
    subtitle_label  =  "0.25 Cohen's D restriction",
    rejection_label  =  "",
    title_label = " ",

) +
  geom_vline(
        xintercept = mean(Results.y.StandardizedDiff[TreatmentBeginsAt:nrow(Results.y.StandardizedDiff),]),
        linetype = "dashed",
        size = 1,
        color = "red")

# Plot n
# Combine the three plots
combined_plot <- plot_grid(
    NullDist.Full,NullDist.25,
    ncol = 1)

# Save the plot
FilePath <- paste0(SCUL.input$OutputFilePath,"mde_graph.pdf")
ggsave(FilePath,
       plot = combined_plot,
       width = 4,
       height = 8,
       units = "in")

# Display the plot
combined_plot

## -----------------------------------------------------------------------------
#########################################################################################################
# Calculate an average post-treatment p-value
PValue(
    CohensD = 999,
    StartTime = SCUL.input$TreatmentBeginsAt,
    EndTime = nrow(Results.y.StandardizedDiff)
)

PValue(
    CohensD = .25,
    StartTime = SCUL.input$TreatmentBeginsAt,
    EndTime = nrow(Results.y.StandardizedDiff)
)



## ---- echo = FALSE, warning = FALSE, , message=FALSE, results='hide'----------

data_for_traditional_scm <- pivot_longer(data = cigarette_sales,
                                  cols = starts_with(c("cigsale_", "retprice_")),
   names_to = c("variable", "fips"),
   names_sep = "_",
   names_prefix = "X",
   values_to = "value",
   values_drop_na = TRUE
 )

data_for_traditional_scm <- pivot_wider(data = data_for_traditional_scm,
                                  names_from = variable,
                                  values_from = value)


 data_for_traditional_scm$fips <- as.numeric(data_for_traditional_scm$fips)


## While synth() can be used to construct synthetic control groups
## directly, by providing the X1, X0, Z1, and Z0 matrices, we strongly
## recommend to first run dataprep() to extract these matrices
## and pass them to synth() as a single object

## The usual sequence of commands is:
## 1. dataprep() for matrix-extraction
## 2. synth() for the construction of the synthetic control group
## 3. synth.tab(), gaps.plot(), and path.plot() to summarize the results
## Below we provide two examples


## First Example: Toy panel dataset

# load data
data(synth.data)

# create matrices from panel data that provide inputs for synth()
data_for_traditional_scm$idno = as.numeric(as.factor(data_for_traditional_scm$fips))  # create numeric country id required for synth()

data_for_traditional_scm <- as.data.frame(data_for_traditional_scm)

dataprep.out<-
  dataprep(
    foo = data_for_traditional_scm,
    predictors = c("retprice"),
    predictors.op = "mean",
    dependent = "cigsale",
    unit.variable = "idno",
    time.variable = "year",
    special.predictors = list(
      list("cigsale", 1970, "mean"),
      list("cigsale", 1980, "mean"),
      list("cigsale", 1985, "mean")
    ),
    treatment.identifier = unique(data_for_traditional_scm$idno[data_for_traditional_scm$fips==6]),
    controls.identifier = unique(data_for_traditional_scm$idno[data_for_traditional_scm$fips!=6]),
    time.predictors.prior = c(1970:1987),
    time.optimize.ssr = c(1970:1987),
    time.plot = 1970:1997
  )

## run the synth command to identify the weights
## that create the best possible synthetic
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
#round(synth.out$solution.w,2)
# contains the unit weights or
#synth.out$solution.v
## contains the predictor weights.

## the output from synth opt
## can be flexibly combined with
## the output from dataprep to
## compute other quantities of interest
## for example, the period by period
## discrepancies between the
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
)

StandardizedDiff$traditional_scm <- abs(dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
))/PreTreatmentSD

# Show Cohen's D for each of these
CohensD <- data.frame(colMeans(StandardizedDiff[1:(TreatmentBeginsAt-1),]))


# Calculate treatment effect for each of these
TreatmentEffect <- data.frame(colMeans(StandardizedDiff[TreatmentBeginsAt:nrow(StandardizedDiff),])*PreTreatmentSD)

## also there are three convenience functions to summarize results.
## to get summary tables for all information
## (V and W weights plus balance btw.
## treated and synthetic control) use the
## synth.tab() command
#ynth.tables <- synth.tab(
#  dataprep.res = dataprep.out,
#  synth.res = synth.out)
#print(synth.tables)

## to get summary plots for outcome trajectories
## of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
#path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
#gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## ---- fig.height=5, fig.width=8, warning = FALSE, fig.align = "center", echo = FALSE, out.width = '100%'----

StandardizedDiff<-cbind(StandardizedDiff,SCUL.input$time)
colnames(StandardizedDiff)[7] <- "time"
# create smoke plot
difference_plot <- ggplot() +
  theme_classic() +
    geom_line(data = StandardizedDiff, aes(x = time, y = -scul.cv), alpha = 1, size = 2., color = "black") +
    geom_line(data = StandardizedDiff, aes(x = time, y = -scul.cv), alpha = 1, size = 1.75, color = "#4dac26") +
     geom_line(data = StandardizedDiff, aes(x = time, y = -traditional_scm), alpha = 1, size = 2., color = "black") +
    geom_line(data = StandardizedDiff, aes(x = time, y = -traditional_scm), alpha = 1, size = 1.75, color = "orange", linetype = "dashed") +
    geom_vline(
        xintercept = SCUL.input$time[TreatmentBeginsAt,1],
        linetype = "dashed",
        size = 1,
        color = "grey37"
    ) +
    labs(
        title = "Difference between actual cigarette sales and synthetic predictions\n from SCUL (green-solid)\n and a traditional synthetic control method (orange-dashed)",
        x = "Time",
        y = "Difference between actual data and predictions\n in pre-treatment standard deviations for each product"
    ) +
    theme(
        axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        title = element_text(size = 12)
    )


# Save graph
difference_plotPath <- paste0(SCUL.input$OutputFilePath,"difference_plot.png")

ggsave(difference_plotPath,
       plot = difference_plot,
       width = 8,
       height = 5,
       dpi = 300,
       units = "in")

# Display graph
difference_plot

## ---- echo = FALSE, warning = FALSE, message=FALSE----------------------------
table_for_hux_scm <- cbind(
  (CohensD),
  (TreatmentEffect)
)
table_for_hux_scm <- table_for_hux_scm[-(2:5),]

names(table_for_hux_scm) <- c("CohensD", "ATE")

table_for_hux_scm$name <- c("Cross-validation for determining penalty", " ")
table_for_hux_scm$value <- c(round(SCUL.output$CrossValidatedLambda, digits = 2), "NA")

table_for_hux_scm <- table_for_hux_scm[c(3, 4, 1,2)]


kable(table_for_hux_scm, col.names = c("Method","Penalty parameter", "Cohens D (pre-period fit)", "ATE estimate"), digits = 3, row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
column_spec(1, width = "5cm") %>%
column_spec(2:4, width = "2cm")  %>%
  pack_rows("SCUL procedure using:", 1, 1) %>%
  pack_rows("Traditional SCM method", 2, 2)


## ---- echo = FALSE, warning = FALSE, message=FALSE----------------------------
 # Make the exact coefficients are stored as
  coef.exact<-as.matrix(synth.out$solution.w)

  # Add an intercept variable (always takes the value of 1) to the donor pool
  # Label the first column
  #colnames(x.DonorPoolWithIntercept) <- c("Intercept")

  # Multiply the coefficients by the value in at each time period
  ContributionToPrediction <- sweep(t(dataprep.out$Y0plot),MARGIN=1,FUN="*",STATS=coef.exact)
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
  ShareOfPredictionForPlot<-data.frame(ShareOfPrediction[TreatmentBeginsAt,],ShareOfPrediction[length(SCUL.output$y.actual),],SignOfContribution[TreatmentBeginsAt,],coef.exact)


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
  ShareOfPredictionForTable[,1]<-round(ShareOfPredictionForTable[,1],digits=4)
  ShareOfPredictionForTable[,2]<-round(ShareOfPredictionForTable[,2],digits=4)
  ShareOfPredictionForTable[,3]<-round(ShareOfPredictionForTable[,3],digits=4)
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

## ---- echo = FALSE, results = 'hide'------------------------------------------
# Combine the two plots for the readme
PlotForReadMe <- plot_grid(
    smoke_plot, combined_plot,
    ncol = 2, rel_widths = c(1, .45))

ReadMePlotPath <- paste0(SCUL.input$OutputFilePath,"ReadMeFigure.png")

ggsave(ReadMePlotPath,
       plot = PlotForReadMe,
       width = 12,
       height = 6,
       dpi = 300,
       units = "in")

## ---- echo = FALSE, fig.height=12, fig.width=18, warning = FALSE, fig.align = "center", warning = FALSE, , message=FALSE, results='hide', out.width = '100%'----
#########################################################################################################
# Clear memory
rm(list = ls())

#########################################################################################################
# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  cowplot, tidyverse, Synth, scul
)

#########################################################################################################
# Set seed
set.seed(1234)

#########################################################################################################
# Number of time periods
n_time_periods <- 100

# Number of groups
n_groups <- 51

# Standard deviation for brownian motion
sig2 <- 0.05

###################################################################
# Initialize data frame
df <- data.frame(matrix(vector(), n_time_periods*n_groups, 3,
                        dimnames=list(c(), c("group_id", "time", "value"))),
                 stringsAsFactors=F)

# Set a random starting point for each variable
random_start_point <- rnorm(n_groups)

for (i in 1:n_groups) {

  # Store group id
  df[((i*n_time_periods) - n_time_periods + 1):(i*n_time_periods), 1] <- rep(i, n_time_periods)

  # Store time
  df[((i*n_time_periods) - n_time_periods + 1):(i*n_time_periods), 2] <- 1:n_time_periods - n_time_periods/2

  # Store value
  temp_normal_draws <- c(random_start_point[i], rnorm(n = n_time_periods - 1, sd = sqrt(sig2)))
  temp_cumulative_normal_draws <- cumsum(temp_normal_draws)

  df[((i*n_time_periods) - n_time_periods + 1):(i*n_time_periods), 3] <- temp_cumulative_normal_draws
}

# Remove temp objects
rm(list = ls()[grep("temp", ls())])

###################################################################
# Create a max time-series that won't be in the convex hull of the donor pool

temp_max <- aggregate(df$value, by = list(df$time), max)

names(temp_max) <- c("time", "value")
temp_max$value <- temp_max$value*1.25
temp_max$value[temp_max$time>=0] <- temp_max$value[temp_max$time>=0]*1.25
temp_max$group_id <- n_groups + 1

# Add to dataframe
df_with_max <- rbind(df, temp_max)

#########################################3
# Create a half max
temp_half_max <- temp_max
temp_half_max$value <- temp_half_max$value - 4
temp_half_max$group_id <- n_groups + 2

# Add to dataframe
df_with_max <- rbind(df_with_max, temp_half_max)

# Remove temp objects
rm(list = ls()[grep("temp", ls())])


# Plot data
max_plot <- ggplot(data = df_with_max, aes(x = time, y = value)) +
  geom_line(aes(group = group_id), alpha = .25, size = 1) +
  geom_line(data = subset(df_with_max, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
  geom_line(data = subset(df_with_max, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#ffab10') +
  geom_line(data = subset(df_with_max, group_id == n_groups + 2) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
  geom_line(data = subset(df_with_max, group_id == n_groups + 2) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#5810ff') +
  theme_classic() +
  labs(title = "Case 1: No convex combination of the donor pool can equal\n the target time series",
       y = "Value",
       x = "Time since treatment") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        title = element_text(size = 18)
  ) +
  ylim(-8, 8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "black") +
  geom_segment(aes(y = 6.7, x = 25, yend = 6, xend = 28 ),
               arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", y = 7 , x=18, label= "Target series", hjust = 0,  size = 5)  +
  geom_segment(aes(y = -4.75, x = -30, yend = -2, xend = -25 ),
               arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", y = -5.75 , x=-39, label= "Optimal donor series \n with weight = 1\n and intercept = 4", hjust = 0,  size = 5)

###################################################################
# Create an inverse time series
group_to_treat <- 15
df_with_inverse <- df
treated_value <- df_with_inverse$value + abs(df_with_inverse$value) + 1
df_with_inverse$value <- ifelse(df_with_inverse$group_id == group_to_treat & df_with_inverse$time>=0, treated_value, df_with_inverse$value)

temp_inverse <- subset(df_with_inverse, group_id == group_to_treat)
temp_inverse$value <- -temp_inverse$value
temp_inverse$group_id <-  n_groups + 1

# Add to dataframe
df_with_inverse <- rbind(df_with_inverse, temp_inverse)

# Remove temp objects
rm(list = ls()[grep("temp", ls())])

# Plot data
inv_plot <- ggplot(data = df_with_inverse, aes(x = time, y = value)) +
  geom_line(aes(group = group_id), alpha = .25, size = 1) +
  geom_line(data = subset(df_with_inverse, group_id == group_to_treat) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
  geom_line(data = subset(df_with_inverse, group_id == group_to_treat) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#ffab10') +
  geom_line(data = subset(df_with_inverse, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black', linetype = "solid") +
  geom_line(data = subset(df_with_inverse, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#5810ff', linetype = "solid") +
  theme_classic() +
  labs(title = "Case 2: The best donor series for this time series is\n countercyclical and would need a weight of -1",
       y = "Value",
       x = "Time since treatment") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        title = element_text(size = 18)
        ) +
  ylim(-8, 8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "black") +
  geom_segment(aes(y = 5.34, x = 20, yend = 4.5 , xend = 16 ),
               arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", y = 5.7 , x=18, label= "Target series", hjust = 0,  size = 5) +
  geom_segment(aes(y = -6, x = 29, yend = -4.2 , xend = 37 ),
               arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", y = -7.25 , x=18, label= "Optimal donor series\n with weight = -1\n and intercept = 0", hjust = 0,  size = 5)


###########################################################################################
# combined_plot <- plot_grid(max_plot, inv_plot,
#                            ncol = 2,
#                            align = c('v', 'h'),
#                            rel_heights = c(1,1),
#                            labels = c('A', 'B')
#                            )
# combined_plot
#
# ggsave("~/Documents/GitHub/scul_work/vignettes/time_series_convex_hull.png",
#        plot = combined_plot,
#        dpi = 300,
#        width = 16,
#        height = 8,
#        units = "in")

#

 # combined_plot <- plot_grid(max_plot, inv_plot, two_series,
 #                            ncol = 3,
 #                            align = c('v', 'h'),
 #                            rel_heights = c(1,1),
 #                            labels = c('A', 'B', 'C')
 # )
 # combined_plot
 #
 # ggsave("~/Documents/GitHub/scul_work/vignettes/time_series_convex_hull.png",
 #        plot = combined_plot,
 #        dpi = 300,
 #        width = 27,
 #        height = 9,
 #        units = "in")
 #


 ##########################################################
 ## Abadie synthetic control

 # Number of groups
 n_groups <- 51

 dataprep.out <-
   dataprep(df_with_max,
            dependent     = "value",
            unit.variable = "group_id",
            time.variable = "time",
            special.predictors = list(
              list("value",1:49,c("mean"))),
            treatment.identifier  = 52,
            controls.identifier   = c(1:51, 53),
            time.predictors.prior = c(-49:-1),
            time.optimize.ssr     = c(-49:-1),
            time.plot             = c(-49:50)
   )

 # Run synth
 synth.out <- synth(dataprep.out)

 # Store restults as a dataframe
 temp_scm_max <- data.frame(dataprep.out$Y0plot%*%synth.out$solution.w[,1])
 names(temp_scm_max) <- c("value")
 temp_scm_max$group_id <- -99
 temp_scm_max$time <- c(-49:50)

 ##################################################################################
 # Run the SCUL algorithm

 # Reshape data
 df_with_max_reshape <- reshape(df_with_max, idvar = "time", timevar = "group_id", direction = "wide")

 # Set up variables for import
 temp.y <- data.frame(df_with_max_reshape[,53])
 names(temp.y) <- c("y")
 temp.time <-data.frame(df_with_max_reshape[,1] + 50)
 names(temp.time) <- c("time")
 temp.x <- data.frame(df_with_max_reshape[,c(-1,-53)])

 #########################################################################################################
 # Setup for SCUL

  SCUL.input <- OrganizeDataAndSetup (
   time = temp.time,
   y = temp.y,
   TreatmentBeginsAt = 50,
   x.DonorPool = temp.x,
   CohensDThreshold=0.10,
   NumberInitialTimePeriods = 10,
   TrainingPostPeriodLength = 10,
   x.PlaceboPool=temp.x,
   OutputFilePath="output/"
 )


  #########################################################################################################
  # Main run
  SCUL.output<-SCUL()

  # Store results as a dataframe
  temp_scul_max <- data.frame(SCUL.output$y.scul)
  names(temp_scul_max) <- c("value")
  temp_scul_max$group_id <- -999
  temp_scul_max$time <- c(-49:50)


  # Add to dataframe
  df_with_max_scm <- rbind(df_with_max, temp_scm_max, temp_scul_max)

  # Plot data


  max_plot_scm_v_scul <- ggplot(data = df_with_max_scm, aes(x = time, y = value)) +
     stat_summary(data = subset(df, group_id > 0), geom="ribbon", alpha = 0.6, fun.max = max, fun.min = min) +
    geom_line(data = subset(df, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
    geom_line(data = subset(df_with_max_scm, group_id == n_groups + 1) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#ffab10') +
    geom_line(data = subset(df_with_max_scm, group_id == -99) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
    geom_line(data = subset(df_with_max_scm, group_id == -99) ,aes(group = group_id), alpha = 1 , size = 1.5, color = 'red') +
    geom_line(data = subset(df_with_max_scm, group_id == -999) ,aes(group = group_id), alpha = 1 , size = 1, color = 'blue', linetype = "dashed") +
    theme_classic() +
    labs(title = "Case 1: Traditional SCM is bound by convex hull and\n cannot use an intercept to select the optimal donor series",
         y = "Value",
         x = "Time since treatment") +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 18),
          title = element_text(size = 18)
    ) +
    ylim(-8, 8) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "black") +
    geom_segment(aes(y = 4.8, x = -32.5, yend = 2.5, xend = -25 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = 5.75 , x= -35, label= "SCUL prediction\n (dashed-line)", hjust = 0,  size = 5)  +
    geom_segment(aes(y = 6.7, x = 25, yend = 6, xend = 28 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = 7 , x=18, label= "Target series", hjust = 0,  size = 5)  +
    geom_segment(aes(y = -4.75, x = -30, yend = 0.5 , xend = -20 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = -5.5 , x=-39, label= "SCM Prediction", hjust = 0,  size = 5) +
    annotate("text", y = -2.25 , x=27, label= "Convex Hull of\n Donor Pool", hjust = 0,  size = 5, color = "white")




  ##########################################################
  ## Abadie synthetic control

  # Number of groups
  n_groups <- 51

  dataprep.out <-
    dataprep(df_with_inverse,
             dependent     = "value",
             unit.variable = "group_id",
             time.variable = "time",
             special.predictors = list(
               list("value",1:49,c("mean"))),
             treatment.identifier  = 15,
             controls.identifier   = c(1:14, 16:52),
             time.predictors.prior = c(-49:-1),
             time.optimize.ssr     = c(-49:-1),
             time.plot             = c(-49:50)
    )

  # Run synth
  synth.out <- synth(dataprep.out)

  # Store results as a dataframe
  temp_scm_inv <- data.frame(dataprep.out$Y0plot%*%synth.out$solution.w[,1])
  names(temp_scm_inv) <- c("value")
  temp_scm_inv$group_id <- -99
  temp_scm_inv$time <- c(-49:50)

  ##################################################################################
  # Run the SCUL algorithm

  # Reshape data
  df_with_inverse_reshape <- reshape(df_with_inverse, idvar = "time", timevar = "group_id", direction = "wide")

  # Set up variables for import
  temp.y <- data.frame(df_with_inverse_reshape[,16])
  names(temp.y) <- c("y")
  temp.time <-data.frame(df_with_inverse_reshape[,1] + 50)
  names(temp.time) <- c("time")
  temp.x <- data.frame(df_with_inverse_reshape[,c(-1,-16)])

  #########################################################################################################
  # Setup for SCUL


  SCUL.input <- OrganizeDataAndSetup (
    time = temp.time,
    y = temp.y,
    TreatmentBeginsAt = 50,
    x.DonorPool = temp.x,
    CohensDThreshold=0.10,
    NumberInitialTimePeriods = 10,
    TrainingPostPeriodLength = 10,
    x.PlaceboPool=temp.x,
    OutputFilePath="output/"
  )


  #########################################################################################################
  # Main run
  SCUL.output<-SCUL()

  # Store results as a dataframe
  temp_scul_inv <- data.frame(SCUL.output$y.scul)
  names(temp_scul_inv) <- c("value")
  temp_scul_inv$group_id <- -999
  temp_scul_inv$time <- c(-49:50)


  # Add to dataframe
  df_with_inv_scm <- rbind(df_with_inverse, temp_scm_inv, temp_scul_inv)

  # Plot data

  inv_plot_scm_v_scul <- ggplot(data = df_with_inv_scm, aes(x = time, y = value)) +
  stat_summary(data = subset(df_with_inverse, group_id > 0), geom="ribbon", alpha = 0.6, fun.max = max, fun.min = min) +
    geom_line(data = subset(df_with_inv_scm, group_id == 15) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
    geom_line(data = subset(df_with_inv_scm, group_id == 15) ,aes(group = group_id), alpha = 1 , size = 1.5, color = '#ffab10') +
    geom_line(data = subset(df_with_inv_scm, group_id == -99) ,aes(group = group_id), alpha = 1 , size = 2, color = 'black') +
    geom_line(data = subset(df_with_inv_scm, group_id == -99) ,aes(group = group_id), alpha = 1 , size = 1.5, color = 'red') +
    geom_line(data = subset(df_with_inv_scm, group_id == -999) ,aes(group = group_id), alpha = 1 , size = 1, color = 'blue', linetype = "dashed") +
    theme_classic() +
    labs(title = "Case 2: Traditional SCM cannot give -1 weight to\n optimal donor series",
         y = "Value",
         x = "Time since treatment") +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 18),
          title = element_text(size = 18)
    ) +
    ylim(-8, 8) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 1, color = "black")   +
    geom_segment(aes(y = 4.8, x = -32.5, yend = 1, xend = -30 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = 5.75 , x= -35, label= "SCUL prediction\n (dashed-line)", hjust = 0,  size = 5)  +
    geom_segment(aes(y = 5.34, x = 20, yend = 4.5 , xend = 16 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = 5.7 , x=18, label= "Target series", hjust = 0,  size = 5) +
    geom_segment(aes(y = -4.75, x = -30, yend = -0.5 , xend = -37 ),
                 arrow = arrow(length = unit(0.25, "cm"))) +
    annotate("text", y = -5.5 , x=-39, label= "SCM Prediction", hjust = 0,  size = 5) +
    annotate("text", y = -2.25 , x=27, label= "Convex Hull of\n Donor Pool", hjust = 0,  size = 5, color = "white")





  combined_plot <- plot_grid(max_plot, inv_plot, max_plot_scm_v_scul, inv_plot_scm_v_scul,
                             ncol = 2,
                             align = c('v', 'h'),
                             rel_heights = c(1,1),
                             labels = c('A', 'B', 'C' , 'D')
  )

combined_plot
  ggsave("vignette_output/time_series_convex_hull.png",
         plot = combined_plot,
         dpi = 300,
         width = 18,
         height = 12,
         units = "in")


## -----------------------------------------------------------------------------
sessionInfo()

