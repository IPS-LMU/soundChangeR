% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_rejection.R
\name{plot_rejection}
\alias{plot_rejection}
\title{Plot the rejection rate over simulation time}
\usage{
plot_rejection(intLog, groupVar = NULL, params)
}
\arguments{
\item{intLog}{interactions log data frame as loaded by load_intLog()}

\item{groupVar}{string vector (max. length: 4) of columns in pop which are used to group the data and plot it, e.g. agent group, phoneme, etc. If run is included here, it is supplied to the \code{group} argument in \code{aes()}. Further variables are colour-coded or plotted in facets.}

\item{params}{list of parameters as loaded by get_params()}
}
\description{
Plot the rejection rate over simulation time
}
