\name{pkpop}
\alias{pkpop}
\title{
Dose finding method PKPOP
}
\description{
A variation of PKLOGIT model, that we call PKPOP, consists of replacing AUCs with AUC of the population (AUCpop). AUCpop is the mean value of the logarithm of AUC at dose k predicted by th hierarchical model. In other words we replaced the value of patient with the mean value of the population. In this way we passes directly to the population level. 
}
\usage{
pkpop(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}

\arguments{
 \item{y}{
A vector of patient outcomes; TRUE indicates a toxicity, FALSE otherwise
}
  \item{auc}{
the auc numbers of each patient
}
  \item{doses}{
the doses levels of the drug
}
  \item{lev}{
the levels 
}
  \item{theta}{
the target probability of toxicity
}
  \item{p_0}{
Skeleton of CRM
}
  \item{L}{
a threshold set before starting the trial.
}
  \item{betapriors}{
the betapriors of pkpop model; the default is NULL for this model.
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
\author{
Artemis Toumazi
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

pkpop(y, AUCs, d, x, theta, p_0, L, betapriors, D_AUC, options)
}

\keyword{models}