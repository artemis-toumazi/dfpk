\name{pkpop}
\alias{pkpop}

\title{
Dose finding method PKPOP.
}

\description{
The PKPOP model is a variation of the PKLOGIT model which replaced AUCs (\eqn{z_j}) with AUC of the population (\eqn{z_{k,pop}}), where \eqn{z_{k,pop}} is the mean value of the logarithm of AUC at dose k, predicted by the hierarchical model:
\deqn{z_{i} \vert \boldsymbol{\beta}, \nu \sim N \left( \beta_0 + \beta_1 \log d_{i}, \nu^{2} \right)}
where \eqn{\boldsymbol{\beta} = (\beta_0,\beta_1)} are the regression parameters and \eqn{\nu} is the standard deviation.
and the logistic regression model:
\deqn{\mbox{logit}(p_T(z_{k,pop}, \boldsymbol{\beta})) = -\beta_3 + \beta_4 z_{k,pop}}
with a bivariate Uniform distribution as prior distribution for the parameters \eqn{\boldsymbol{\beta} = (\beta_3, \beta_4)}.

The default choices of the priors are:
\deqn{\boldsymbol{\beta} \vert \nu \sim N(m, \nu*beta0),}
\deqn{\nu \sim Beta(1,1),}
\deqn{m = (-log(CL_{pop}), 1),} where \eqn{Cl_{pop}} is the population clearance.
\deqn{\beta_3 \sim U(0, beta3mean),}
\deqn{\beta_4 \sim U(0, beta4mean)}
where default choices are \eqn{Cl_{pop} = 10}, beta0 = 10000, beta3mean = 10 and beta4mean = 5. Therefore, the default choices for model's priors are given by \deqn{betapriors = c(Cl_{pop} = 10, beta0 = 10000, beta3mean = 10, beta4mean = 5)}

Finally, the PKPOP model has the following stopping rule in toxicity:
if
\deqn{P(p_T(dose) > theta) > prob}
then, no dose is suggested and the trial is stopped. 
}

\usage{
pkpop(y, auc, doses, x, theta, prob = 0.9, options = list(nchains = 4, niter = 4000, 
      nadapt = 0.8), betapriors = c(10, 10000, 10, 5), thetaL = NULL, p0=NULL, 
      L=NULL, deltaAUC=NULL, CI = TRUE)
}

\arguments{
\item{y}{
    A binary vector of patient's toxicity outcomes; TRUE indicates a toxicity, FALSE otherwise.}

\item{doses}{
    A vector with the doses panel.
}
\item{x}{
    A vector with the dose level assigned to the patients.
}
\item{theta}{
    The toxicity target.
}
\item{prob}{
    The threshold of the posterior probability of toxicity for the stopping rule; defaults to 0.9.
}
\item{betapriors}{
    A vector with the value for the prior distribution of the regression parameters in the model; defaults to betapriors = c(\eqn{Cl_{pop}}, beta0, beta3mean, beta4mean), where \eqn{Cl_{pop} = 10}, beta0 = 10000, beta3mean = 10 and beta4mean = 5.
}
\item{options}{
    A list with the Stan model's options; the number of chains, how many iterations for each chain and the number of warmup iterations; defaults to options = list(nchains = 4, niter = 4000, nadapt = 0.8).
}
\item{auc}{
    A vector with the computed AUC values of each patient for pktox, pkcrm, pklogit and pkpop; defaults to NULL.
}
\item{deltaAUC}{
    The difference between computed individual AUC and the AUC of the population at the same dose level (defined as an average); argument for pkcov; defaults to NULL.
}
\item{p0}{
    The skeleton of CRM for pkcrm; defaults to NULL (must be defined only in the PKCRM model).
}
\item{L}{
    The AUC threshold to be set before starting the trial for pklogit, pkcrm and pktox; defaults to NULL (must be defined only in the PKCRM model).
}
\item{thetaL}{
    A second threshold of AUC; must be defined only in the PKCRM model.
}
\item{CI}{
    A logical constant indicating the estimated 95\% credible interval; defaults to TRUE.
}  
}

\value{
A list is returned, consisting of determination of the next recommended dose and estimations of the model. Objects generated by pkpop contain at least the following components:

\item{newDose}{The next maximum tolerated dose (MTD); equals to "NA" if the trial has stopped before the end, according to the stopping rules.}
\item{pstim}{The mean values of estimated probabilities of toxicity.}
\item{p_sum}{The summary of the estimated probabilities of toxicity if CI = TRUE, otherwise is NULL.}
\item{parameters}{The estimated model's parameters.}
}

\references{
Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal, <doi:10.1002/bimj.201600084>.

Toumazi, A., et al, (2018) dfpk: An R-package for Bayesian dose-finding designs using pharmacokinetics (PK) for phase I clinical trials, Computer Methods and Programs in Biomedicine, <doi:10.1016/j.cmpb.2018.01.023>.

Patterson, S., Francis, S., Ireson, M., Webber, D., and Whitehead, J. (1999) A novel bayesian decision procedure for early-phase dose-finding studies. Journal of Biopharmaceutical Statistics, 9 (4), 583-597.

Whitehead, J., Patterson, S., Webber, D., Francis, S., and Zhou, Y. (2001) Easy-to-implement bayesian methods for dose-escalation studies in healthy volunteers. Biostatistics, 2 (1), 47-61.

Whitehead, J., Zhou, Y., Hampson, L., Ledent, E., and Pereira, A. (2007) A bayesian approach for dose-escalation in a phase i clinical trial incorporating pharmacodynamic endpoints. Journal of Biopharmaceutical Statistics, 17 (6), 1117-1129.
}

\author{Artemis Toumazi \email{artemis.toumazi@gmail.com},
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
        }

\seealso{
\code{\link{pklogit}}, \code{\link{sim.data}}, \code{\link{nsim}}, \code{\link{nextDose}}
}

\examples{
    \dontrun{
        doses <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
        theta <- 0.2      # choice
        options <- list(nchains = 2, niter = 4000, nadapt = 0.8)
        AUCs <-  c(0.43, 1.4, 5.98, 7.98, 11.90, 3.45)
        x <- c(1,2,3,4,5,6)
        y <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)

        res <- pkpop(y, AUCs, doses, x, theta, options = options)
    }
}

\keyword{models}
