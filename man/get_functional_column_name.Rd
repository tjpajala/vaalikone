% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_functional_column_name}
\alias{get_functional_column_name}
\title{Get a column name with one of the alternative spellings}
\usage{
get_functional_column_name(data, alternative_spellings)
}
\arguments{
\item{data}{Data set to look into.}

\item{alternative_spellings}{Possible alternative spellings of the column name to look for.}
}
\value{
The name of the column  that was found.
}
\description{
Get a column name with one of the alternative spellings
}
\note{
Behavior not specified if many columns of the alternative_spellings exist.
}
