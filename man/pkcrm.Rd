\name{pkcrm}
\alias{pkcrm}

\title{
Dose finding method PKCRM.
}

\description{
The PKCRM model is an implementation of the Continual Reassessment Method's (CRM) model, which is constructed in order to avoid the necessary need to know the target AUC related to DLTs. Therefore, we proposed a new model called PKCRM, with power working model and normal prior on the parameter.
}

\usage{
pkcrm(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}

\arguments{
  \item{y}{
A vector of patient's toxicity outcomes; TRUE indicates a toxicity, FALSE otherwise.
}
  \item{auc}{
The AUC values of each patient.
}
  \item{doses}{
The dose levels of the drug.
}
  \item{lev}{
A vector of dose levels assigned to the patients.
}
  \item{theta}{
The toxicity (probability) target.
}
  \item{p_0}{
The skeleton of CRM; defaults to NULL. (must be defined only in the PKCRM model)
}
  \item{L}{
A threshold set before starting the trial; defaults to NULL. (must be defined only in the PKCRM model)
}
  \item{betapriors}{
A vector of the regression parameters in the model.
}
  \item{D_AUC}{
A vector specifying the difference between the AUCs and AUC_pop; defaults to NULL.
}
  \item{options}{
A list of three integers specifying the stan model's number of chains, how many iterations for each chain and the number of warmup iterations.
defaults to options <- list(nchains = 4,
                            niter = 4000,
                            nadapt = 0.8)}
}

\references{
Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).

Patterson, S., Francis, S., Ireson, M., Webber, D., and Whitehead, J. (1999) A novel bayesian decision procesure for early-phase dose-finding studies. Journal of Biopharmaceutical Statistics, 9 (4), 583-597.

Whitehead, J., Patterson, S., Webber, D., Francis, S., and Zhou, Y. (2001) Easy-to-implement bayesian methods for dose-escalation studies in healthy volunteers. Biostatistics, 2 (1), 47-61.
}

\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr},
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
        }

\seealso{
\code{\link{scenarios}}, \code{\link{nsim}}, \code{\link{nextDose}}
}

\examples{
p_0 <- c(.01,.05,.1,.2,.35,0.45)      # Skeleton of CRM
L = log(15.09)                        # Threshold set
d <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
theta <- 0.2
options <- list(nchains = 2,
                niter = 4000,
                nadapt = 0.8)
AUCs <-  c(0.43, 1.4, 5.98, 7.98, 11.90, 3.45)
x <- c(1,2,3,4,5,6)
y <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
D_AUC <- NULL

### Betapriors ###
betapriors = NULL
pkcrm(y, AUCs, d, x, theta, p_0, L, betapriors, D_AUC,options)
}

\keyword{models}
