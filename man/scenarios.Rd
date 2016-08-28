\name{scenarios}
\alias{scenarios}
\title{
Generate New Scenarios
}
\description{
This function can be used to generate several scenarios.
}
\usage{
scenarios(param_pk, omega2, omega_a, sigma2, preal, limit_tox, time_sampling, N)
}
\arguments{
  \item{param_pk}{
Subject's pharmacokinetics(PK) parameters. 
}
\item{omega2}{The standard deviation of the log-normal distribution that the PK parameters clearance(CL) and volume(V) come from; it's equal to 70\% or 30\% in different scenarios. 
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
The threashold on toxicity.
}
  \item{time_sampling}{
Time sampling.
}
  \item{N}{
The total number of patients.
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
\code{\link{sim}}
}

\examples{
#####################################
######### Scenario Inputs  ##########
#####################################

limit_tox <- 10.96                  
N <- 42                               
# number of patients
param_pk <- c(2,10,100)   # PK parameters ka,CL,V
omega2 <- 0.7             # Inter-individual
omega_a <- 0              # IIV   
preal <- c(0.001,.05,0.1,.2,0.35,0.45)      
# true prob of toxicity
time_sampling <- seq(0,24,length.out=48)    # time sampling
sigma2 <- c(rep(0.2,length(time_sampling)),rep(0.2,length(time_sampling)))           
# sigma2: Additive or proportional error

#############################################
############ Test the scenario ##############
#############################################

gen.scen <- scenarios(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N) 
gen.scen 
}

\keyword{models}