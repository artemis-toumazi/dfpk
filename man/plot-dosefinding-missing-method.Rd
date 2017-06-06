% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4_classes.R
\docType{methods}
\name{plot,dosefinding,missing-method}
\alias{plot,dosefinding,missing-method}
\title{The graphical representation of dose escalation for each patient in the trial.}
\usage{
\S4method{plot}{dosefinding,missing}(x, y = NA, TR = 1, ask = TRUE, ...)
}
\arguments{
\item{x}{a "dosefinding" object.}

\item{y}{the "y" argument is not used in the plot-method for "scen" object.}

\item{TR}{The number of replicates clinical trials.}

\item{ask}{Choose plot or not.}

\item{\dots}{other arguments to the \code{\link[=graphics]{plot.default}} function can be passed here.}
}
\description{
The graphical representation of dose escalation for each patient in the trial.
}
\author{
Artemis Toumazi \email{artemis.toumazi@inserm.fr}, Moreno Ursino \email{moreno.ursino@inserm.fr}, Sarah Zohar \email{sarah.zohar@inserm.fr}
}
\references{
Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal.
}
