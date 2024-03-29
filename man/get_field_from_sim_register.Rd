% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_field_from_sim_register.R
\name{get_field_from_sim_register}
\alias{get_field_from_sim_register}
\title{Get values for field(s) in simulation register for all registered simulations}
\usage{
get_field_from_sim_register(rootLogDir, ...)
}
\arguments{
\item{rootLogDir}{root logging directory, as set by argument rootLogDir in run_simulation()}

\item{...}{vector of arguments of run_simulation()}
}
\value{
list with as many entries as there are simulations in the register where values are the values of the fields entered as argument to this function
}
\description{
Get values for field(s) in simulation register for all registered simulations
}
