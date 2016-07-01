\name{dfpk-package}
\alias{dfpk-package}
\alias{dfpk}
\docType{package}
\title{A Bayesian dose-finding design using pharmacokinetics(PK) for Phase I clinical trials.
}
\description{
\code{dfpk} package includes methods involving PK measures in the dose allocation process during a Phase I clinical trials. These methods enter PK in the dose finding designs in different ways, including covariates models, dependent variable or hierarchical models. This package provides functions to generate scenarios, and to run simulations which their objective is to determine the maximum tolerated dose (MTD) and to investigate PK of a drug or a combination of drugs.
}
\details{
The two main functions of the dfpk package are \code{\link{scenarios}} and \code{\link{sim}} which the first one creates the scenarios for the PK data and the second one simulates the data in order to find the next maximum tolerated dose (MTD) of the drug. 
Subsequently, six dose-finding methods can be applied:  \code{\link{dtox}}, \code{\link{pktox}}, \code{\link{pkcrm}}, \code{\link{pkcov}}, \code{\link{pkpop}}, and \code{\link{pklogit}} which each one creates the model in Stan language and fits it using \pkg{Stan}.      

Because \pkg{dfpk} is based on \pkg{Stan}, a C++ compiler is required. 
The program Rtools (available on https://cran.r-project.org/bin/windows/Rtools/) comes with a C++ compiler for Windows. On Mac, you should use Xcode. 
For further instructions on how to get the compilers running,
see the prerequisites section on https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.
}

\author{
Artemis Toumazi \email{artemis.toumazi@inserm.fr} and Moreno Ursino \email{moreno.ursino@inserm.fr}

Maintainer: Artemis Toumazi \email{artemis.toumazi@inserm.fr}
}

\references{
Moreno et al, 2016
}

\keyword{ package }
