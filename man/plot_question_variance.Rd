% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_question_variance}
\alias{plot_question_variance}
\title{Plot question variance as a tile plot}
\usage{
plot_question_variance(data, q_cols, cols_to_analyze=q_cols[1:5], functions_to_use="var")
}
\arguments{
\item{data}{Dataset.}

\item{q_cols}{Question columns}

\item{cols_to_analyze}{(Optional.) Only a subset of q_cols, if that's all you want to look at. Default is all columns.}

\item{function_to_use}{(Optional.) What function to analyze across parties? Default is "var". Has to be length 1}

\item{palette}{(Optional.) Colour palette for the plot. Default: "BuGn".}
}
\description{
Analyzes question variance as in \code{\link{table_question_variance}} and plots the result as a tile plot.
}
