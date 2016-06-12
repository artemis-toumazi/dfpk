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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param_pk}{
Subject's pharmacokinetics(PK) parameters 
}
  \item{omega2}{standard deviation of the log-normal distribution that the PK parameters clearance(CL) and volume(V) come from. Is equal to 70\% or 30\% in different scenarios. 
}
  \item{omega_a}{standard deviation of the log-normal distribution which the sensitivity parameter \eqn{\alpha} comes from. 
}
  \item{sigma2}{
additive or proportional error
}
  \item{preal}{
the true probabilities of toxicity
}
  \item{limit_tox}{
the threashold on toxicity
}
  \item{time_sampling}{
time sampling
}
  \item{N}{
number of patients
}
}

\references{
Moreno et al, 2016 
}
\author{
Artemis Toumazi
}

\seealso{
\code{\link{sim}}
}

\examples{
#####################################
######### Scenario Inputs  ##########
#####################################

limit_tox <- 10.96                  
N <- 42                             # number of patients
param_pk <- c(2,10,100)             # PK parameters ka,CL,V
omega2 <- 0.7                       # Inter-individual
omega_a <- 0                        # IIV   
preal <- c(0.001,.05,0.1,.2,0.35,0.45)    # true prob of toxicity
time_sampling <- seq(0,24,length.out=48)  # time sampling
sigma2 <- c(rep(0.2,length(time_sampling)),rep(0.2,length(time_sampling)))           
# sigma2: Additive or proportional error

#############################################
############ Test the scenario ##############
#############################################

gen.scen <- scenarios(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N)
gen.scen 
}

\keyword{models}