\name{scenarios}
\alias{scenarios}

\title{
Generate new data from a scenarios.
}

\description{
This function can be used to generate/simulate new data from several scenarios.
}

\usage{
scenarios(param_pk, omega2, omega_a, sigma2, preal, limit_tox, time_sampling, N)
}

\arguments{
  \item{param_pk}{
Subject's pharmacokinetic's (PK) parameters. 
}
\item{omega2}{The standard deviation of the log-normal distribution that the pharmacokinetic's (PK) parameters, clearance(CL) and volume(V), come from; it's equal to 70\% or 30\% in different scenarios. 
}
  \item{omega_a}{The standard deviation of the log-normal distribution which the sensitivity parameter \eqn{\alpha} comes from. 
}
  \item{sigma2}{
The additive or proportional error.
}
  \item{preal}{
The true probabilities of toxicity.
}
  \item{limit_tox}{
The threshold on toxicity.
}
  \item{time_sampling}{
Time sampling.
}
  \item{N}{
The total number of enrolled patients.
}
}

\references{
Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
}

\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
        }
        
\seealso{
\code{\link{nsim}}
}

\examples{
limit_tox <- 10.96                  
N <- 42                               
param_pk <- c(2,10,100)       # PK parameters ka,CL,V
omega2 <- 0.7                 # Inter-individual
omega_a <- 0                    
preal <- c(0.001,.05,0.1,.2,0.35,0.45) 
time_sampling <- seq(0,24,length.out=48)    
sigma2 <- c(rep(0.2,length(time_sampling)),rep(0.2,length(time_sampling)))           
# sigma2: Additive or proportional error

gen.scen <- scenarios(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N) 
gen.scen 
}

\keyword{models}