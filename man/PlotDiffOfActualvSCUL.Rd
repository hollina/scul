% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotDiffOfActualvSCUL.R
\name{PlotDiffOfActualvSCUL}
\alias{PlotDiffOfActualvSCUL}
\title{Graph output from SCUL procedure}
\usage{
PlotDiffOfActualvSCUL(
  y.actual = SCUL.output$y.actual,
  y.scul = SCUL.output$y.scul,
  time = SCUL.output$time,
  OutputFilePath = SCUL.input$OutputFilePath,
  TreatmentBeginsAt = SCUL.output$TreatmentBeginsAt,
  save.figure = FALSE
)
}
\arguments{
\item{y.actual}{The actual (target) data. Default is SCUL.output$y.actual.}

\item{y.scul}{Synthetic data created by SCUL procedure. Default is SCUL.output$y.scul.}

\item{time}{Default is  SCUL.output$time.}

\item{OutputFilePath}{Default is  SCUL.input$OutputFilePath.}

\item{TreatmentBeginsAt}{Default is  SCUL.output$TreatmentBeginsAt.}

\item{save.figure}{Boolean, set to TRUE if you want output saved as figure to OutputFilePath automatically. Default is FALSE}
}
\value{
plots  Three plots that compare scul results to actual data; in levels, difference, and standardized difference.
}
\description{
Plot output from the SCUL procedure in levels, differences, and standardized difference
}
