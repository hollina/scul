#' Plot mean square error across potential penalty paramters, highlighting three options for cross-validated \code{lambda}
#'
#' This is a simple adaptation of the \code{getOptcv.glmnet} function from  \code{glmnet}.
#' The code takes a grid of \code{lambda} penalties along with a vector of associated mean squared errors (\code{mse}), standard errors (\code{se}), and the mean squared error from each cross-validation run (\code{fullMSE}).
#' From this three potential options for the cross-validated penalty parameter are computed. 1) The \code{lambda} that has the minimum average mean squared error across all the cross-validation runs (\code{lambda$lambda.min}),
#' 2) The \code{lambda} the largest \code{lambda} that is associated with an average cross-validated mean squared error within one standard error of the minimum average cross-validated mean squared error (\code{lambda$lambda.1se}), and
#' 3) the \code{lamda} that is the median of the set of \code{lambdas} (\code{lambda$lambda.median}).
#'
#' @param lambdapath A grid of lambdas that is used in each cross-validation run as potential options for the optimal penalty parameter.
#' @param mse A vector of the average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.
#' @param se A vector of the standard error associated with each average mean squared error (average across cross-validation runs) for each given \code{lambda} in the \code{lambdapath} grid.
#' @param lambda Output from the \code{getOptcv.scul} function. Will have three candidate \code{lambdas}.
#' @param save.figure Boolean if you want to save figure. Default is to save (save.figure = TRUE).
#' @param OutputFilePath File path prefix if you are saving the figure. Default is file path set in \code{SCUL.inut} (OutputFilePath = SCUL.input$OutputFilePath).
#' @param minLambdas Vector of lambdas that minimize MSE in each CV run
#'
#' @import ggrepel
#' @import ggplot2
#' @importFrom rlang .data
#'
#'@export
plotCVfunction <-
  function (lambdapath,
            mse,
            se,
            lambda,
            minLambdas,
            save.figure = TRUE,
            OutputFilePath = SCUL.input$OutputFilePath){

    # Create a dataframe of the data we want to plot
    plotdata <- data.frame(cbind(lambdapath, mse, se))
    names(plotdata) <- c("lambda", "MSE", "SE") # Add names
    plotdata$lambda <- -log(plotdata$lambda) # Take the negative of the penalty parameter

    # Create a mini-dataframe of the optimal lambdas
    plotLambda <- data.frame(Ref = c("Min MSE", "Median Lambda", "1 SE"),
                             vals = c(-log(lambda$lambda.min), -log(lambda$lambda.median), -log(lambda$lambda.1se)),
                             lcols = c("red", "blue", "black"),
                             ltype = c('solid', 'dashed', 'longdash'),
                             stringsAsFactors = FALSE)

    # Make plot
    cvPLOT <-  ggplot(data = plotdata, aes(x = lambda, y = MSE)) +
      geom_vline(data = plotLambda,
                 mapping = aes(xintercept = vals,
                               linetype = ltype,
                               color = lcols),
                 show.legend = FALSE,
                 size = 1, alpha = .75) +
      geom_errorbar(data = plotdata, aes(ymin = MSE - SE, ymax = MSE + SE), width=.25) +
      geom_point(data = plotdata, aes(x = lambda, y = MSE), size = 3) +
      theme_classic(base_size = 22) +
      labs(title = "Cross-validated mean squared error vs. penalty parameter",
           x = "-Log(Lambda)",
           y = "Mean squared error") +
      geom_label_repel(mapping = aes(x = vals,
                                     y = max(plotdata$MSE + plotdata$SE),
                                     label = Ref,
                                     hjust = 1,
                                     vjust = 0),
                       data = plotLambda) +
      geom_rug(data = data.frame(-log(t(minLambdas))) %>% select( ml = 1, everything() ),
               aes(x = ml),
               inherit.aes = F,
               sides="b",
               size = 4,
               alpha = .1)

    if (save.figure == TRUE) {
      # Save graph
      cvPLOTPath <- paste0(OutputFilePath,"cvPLOT.pdf")
      ggsave(cvPLOTPath,
             plot = cvPLOT,
             width=12,
             height=8,
             dpi=300)
    }

    ####################
    ## Return plot
    return(cvPLOT)
  }
