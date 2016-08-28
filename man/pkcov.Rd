\name{pkcov}
\alias{pkcov}
\title{
Dose finding method PKCOV
}
\description{
The PKCOV model is a modification of the model proposed by Piantadosi and Liu (1996). As in the original paper we maintained the idea to incorporate PK as covariate of \eqn{p_T} through the logit link. This model uses \eqn{\Delta}z instead of \eqn{\Delta}AUC in order to have both the covariates on the same scale. As the original paper uniform priors have been taken for \eqn{\beta_1} and \eqn{\beta_2}, independently of each other. 
}
\usage{
pkcov(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}

\arguments{
 \item{y}{
A vector of patient outcomes; TRUE indicates a toxicity, FALSE otherwise.
}
  \item{auc}{
The AUC numbers of each patient.
}
  \item{doses}{
The doses levels of the drug.
}
  \item{lev}{
A vector of dose levels assigned to patients.
}
  \item{theta}{
The toxicity (probability) target.
}
  \item{p_0}{
Skeleton of CRM.
}
  \item{L}{
A threshold set before starting the trial.
}
  \item{betapriors}{
A vector of the regression parameters in pkcov model.
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

Piantadosi, S. and Liu, G. (1996) Improved designs for dose escalation studies using pharmacokinetic measurements. Statistics in Medicine, 15 (15), 1605-1618.

}
\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
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
param_pk <- c(2,10,100) 
omega2 <- 0.7
logit <- function(x) log(x/(1-x))
xr <- d
yr <- logit(pnorm((log(xr) - log(10.96) - log(param_pk[2]))/omega2))
coeff <- lm(yr ~ log(xr))
beta0mean <- -coeff$coefficients[1]
beta1mean <- coeff$coefficients[2]
betapriors <- c(beta0mean, beta1mean)

pkcov(y, AUCs, d, x, theta, p_0, L, betapriors,D_AUC,options)
}

\keyword{models}