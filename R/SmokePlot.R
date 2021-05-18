#' Make smoke plot
#'
#' Plot standardized differences of all placebo goods and target good.
#'
#' @param  x.PlaceboPool.full A (T by L), where L<=J)  data frame containing all products that are included in the placebo distribution
#'        Default is SCUL.inference$y.placebo.StandardizedDifference.Full
#' @param  x.PlaceboPool.CohensD A (1 by L)  data frame containing all pre-period Cohen's D fit statistic for each placebo unit.
#'        Default is SCUL.inference$y.placebo.CohensD,
#' @param  TreatmentBeginsAt An integer indicating which row begins treatment. Default is  SCUL.output$TreatmentBeginsAt.
#' @param  OutputFilePath Output file path. Default is SCUL.input$OutputFilePath.
#' @param  CohensD A real number greater than 0, indicating the Cohen's D threshold at which
#'                     fit is determined to be "poor". The difference is in standard deviation units. Default is SCUL.input$CohensDThreshold.
#' @param  y.actual The actual (target) data. Default is SCUL.output$y.actual.
#' @param  y.scul Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.
#' @param  fig.title Title of smoke-plot. Default is "Standardized difference for target variable compared to standardized difference for each placebo"
#' @param  custom.alpha Choose transparancy of placebo pool lines. Default is .33.
#' @param  save.figure Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE
#'
#' @return graph  A smoke plot of the standardized effect size compared to placbos.
#' @import tidyverse
#' @import ggplot2
#' @export

SmokePlot <- function(
  x.PlaceboPool.full = SCUL.inference$y.placebo.StandardizedDifference.Full,
  x.PlaceboPool.CohensD = SCUL.inference$y.placebo.CohensD,
  TreatmentBeginsAt = SCUL.input$TreatmentBeginsAt,
  OutputFilePath = SCUL.input$OutputFilePath,
  CohensD = SCUL.input$CohensDThreshold,
  y.actual = SCUL.output$y.actual,
  y.scul = SCUL.output$y.scul,
  fig.title =  "Standardized differences of target compared\n and each placebo",
  custom.alpha = 0.33,
  save.figure = FALSE
                  ) {
  ###################
  #Set up actual scul results for future comparison
  # Calculate the difference between the two
  y.difference <- data.frame(y.actual - y.scul)
  #names(y.difference) <- names(y.actual)

  # Calculate the standard deviation of the outcome variable in the pre-treatment period
  y.PreTreatmentSD <- sd(y.actual[1:(TreatmentBeginsAt-1)])

  # Take the absolute value of the difference between the prediction and the actual data divided by the standard deviation
  y.StandardizedDifference <- y.difference/y.PreTreatmentSD

  # Calculate the Cohens D
  y.CohenD <- abs(y.StandardizedDifference[1:(TreatmentBeginsAt-1),])
  y.CohenD <- data.frame(y.CohenD<=CohensD)
  # y.CohenD[1,1] <- FALSE


  ###################
  # Trim the time for the stat
  # y.StandardizedDifference.trim <- y.StandardizedDifference[StartTime:EndTime,unlist(y.CohenD[1,])]
  y.StandardizedDifference.trim <- data.frame(y.StandardizedDifference)

  ###################
  #Set up placebo distribution
  placebo.distribution.trim <- x.PlaceboPool.full[ , x.PlaceboPool.CohensD <= CohensD]
  # placebo.distribution.trim <- data.frame(placebo.distribution.full[StartTime:EndTime,cd<=CohensD])

  # reshape the placebo data to be in long form
  data_to_plot_wide_y <- cbind( SCUL.input$time, y.StandardizedDifference)
  names(data_to_plot_wide_y) <- c("time", "std_diff")

  data_to_plot_wide <- cbind( SCUL.input$time, placebo.distribution.trim)
  names(data_to_plot_wide)[1] <- c("time")

  data_to_plot_long <- pivot_longer(data = data_to_plot_wide,
                                    cols = -c("time"),
                                    names_to = "group",
                                    names_prefix = "X",
                                    values_to = "std_diff",
                                    values_drop_na = TRUE
  )

  # create smoke plot
  smoke_plot <- ggplot(data = data_to_plot_long, aes(x = time, y = std_diff)) +
    geom_line(aes(group = group), alpha = custom.alpha, size = 1) +
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
      title = fig.title,
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
  if (save.figure == TRUE) {
    # Save graph
    SmokePlotPath<-paste0(SCUL.input$OutputFilePath,"smoke_plot.png")
    ggsave(SmokePlotPath,
           plot = smoke_plot,
           width = 8,
           height = 5,
           dpi = 300,
           units = "in")
  }
  ####################
  ## Return plot
  return(smoke_plot)
}
