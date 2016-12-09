\name{dfpk-package}
\alias{dfpk-package}
\alias{dfpk}
\docType{package}
\title{A Bayesian Dose-Finding Design using Pharmacokinetics (PK) for Phase I Clinical Trials.
}

\description{
Includes methods involving PK measures in the dose allocation process during a Phase I clinical trials. These methods enter pharmacokinetics (PK) in the dose finding designs in different ways, including covariates models, dependent variable or hierarchical models. This package provides functions to generate data from several scenarios and functions to run simulations which their objectives are to determine the maximum tolerated dose (MTD) and to investigate the pharmacokinetics of a drug or a combination of drugs.
}

\details{
The three main functions of the dfpk package are \code{\link{scenarios}}, \code{\link{nsim}} and \code{\link{nextDose}}, the first one generates new PK data based on the scenario, the second one simulates "n" clinical trials in order to find the next maximum tolerated dose (MTD) of the drug and the third one determines the next recommended dose for an ongoing phase I clinical trial based on a currently data. 
Subsequently, six dose-finding methods can be applied:  \code{\link{dtox}}, \code{\link{pktox}}, \code{\link{pkcrm}}, \code{\link{pkcov}}, \code{\link{pkpop}}, and \code{\link{pklogit}} which each one creates a Bayesian model and fits it using \pkg{Stan}.

Because \pkg{dfpk} is based on \pkg{Stan}, a C++ compiler is required.
The program Rtools (available on https://cran.r-project.org/bin/windows/Rtools/) comes with a C++ compiler for Windows. On Mac, you should use Xcode.
For further instructions on how to get the compilers running,
see the prerequisites section on https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.
}

\author{
Artemis Toumazi \email{artemis.toumazi@inserm.fr} , Moreno Ursino \email{moreno.ursino@inserm.fr} and Sarah Zohar \email{sarah.zohar@inserm.fr}

Maintainer: Artemis Toumazi \email{artemis.toumazi@inserm.fr}
}

\references{
Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
}

\keyword{ package }
