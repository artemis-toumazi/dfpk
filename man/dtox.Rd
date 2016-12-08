\name{dtox}
\alias{dtox}
\title{
Dose finding method DTOX
}

\description{
The DTOX model constructed to test the behaviours of a way of estimating probabilities of toxicity. This way is to estimate posterior probability of toxicity \eqn{p_T} versus dose directly. Therefore it represents a statistical model to link \eqn{p_T} and dose. 
}

\usage{
dtox(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}


\arguments{
  \item{y}{
A vector of patient's toxicity outcomes; TRUE indicates a toxicity, FALSE otherwise.
}
  \item{auc}{
The AUC values of each patient.
}
  \item{doses}{
The doses levels of the drug.
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

}
\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
        }
        
\seealso{
\code{\link{scenarios}}, \code{\link{sim}} 
}

\examples{
### Betapriors ###
param_pk <- c(2,10,100)
beta0mean <- (log(param_pk[2])+log(10.96))/sqrt(0.7**2 + 0**2)
beta1mean <- 1 / 0.7
betapriors <- c(beta0mean, beta1mean)

p_0 = NULL              
L = NULL 
d <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
theta <- 0.2      
options <- list(nchains = 2,
        niter = 4000,
        nadapt = 0.8)  

AUCs <-  c(0.43, 1.4, 5.98, 7.98, 11.90, 3.45)
x <- c(1,2,3,4,5,6)              
y <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
D_AUC <- c(0, 1.3, -0.34, -2.7,0.39, -2.45)

dtox(y, AUCs, d, x, theta, p_0, L,betapriors,D_AUC, options)
}

\keyword{models}