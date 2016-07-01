\name{pklogit}
\alias{pklogit}
\title{
Dose finding method PKLOGIT
}

\description{
The PKLOGIT model is inspired by Whitehead et al. (2007) with a change that we use AUC instead of dose as covariate for a link function of posterior probability of toxicity,
\deqn{logit(p_T(z, \beta)) = -\beta_3 + \beta_4z}
with a bivariate uniform distribution as prior distribution for
\deqn{\beta = (\beta_3, \beta_4)}
}
\usage{
pklogit(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}

\arguments{
 \item{y}{
A vector of patient outcomes; TRUE indicates a toxicity, FALSE otherwise.
}
  \item{auc}{
the auc numbers of each patient.
}
  \item{doses}{
the doses levels of the drug.
}
  \item{lev}{
A vector of dose levels assigned to patients.
}
  \item{theta}{
the toxicity (probability) target.
}
  \item{p_0}{
Skeleton of CRM.
}
  \item{L}{
a threshold set before starting the trial.
}
  \item{betapriors}{
the betapriors of pklogit model; the default is NULL for this model. 
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
Moreno et al, 2016
}
\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr}
        }

\seealso{
\code{\link{scenarios}}, \code{\link{sim}}
}

\examples{
p_0 = NULL                  
L = NULL 
d <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
theta <- 0.2      # choice
options <- list(nchains = 2,
        niter = 4000,
        nadapt = 0.8)  
AUCs <-  c(0.43, 1.4, 5.98, 7.98, 11.90, 3.45)
x <- c(1,2,3,4,5,6)              
y <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
D_AUC <- c(0, 1.3, -0.34, -2.7,0.39, -2.45)

### Betapriors ###
betapriors = NULL

pklogit(y, AUCs, d, x, theta, p_0, L, betapriors,D_AUC,options)
}

\keyword{models}