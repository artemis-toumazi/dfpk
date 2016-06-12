\name{sim}
\alias{sim}

\title{
Simulate subjects using PK measure in the dose finding design
}
\description{
The sim function can be used to simulate PK data first and then link them to toxicity. The objective is to determine the maximum tolerated dose (MTD). 
}

\usage{
sim(d, N, cohort, icon, theta, p_0, L, model, scenarios,betapriors, options)
}

\arguments{
  \item{d}{
doses' levels.
}
  \item{N}{
the number of patients enrolled. 
}
  \item{cohort}{
number of cohorts.
}
  \item{icon}{
the index of real blood sampling.
}
  \item{theta}{
the target probability of toxicity.
}
  \item{p_0}{
skeleton of CRM.
}
  \item{L}{
a threshold set before starting the trial. 
}
  \item{model}{
a name of dose-finding model.
}
  \item{scenarios}{
a scenarios with the PK data.
}
  \item{betapriors}{
the betapriors of the specific model
}
  \item{options}{
A list of three integers specifying the stan model's number of chains, how many iterations for each chain and the number of warmup iterations; defaults to options <- list(nchains = 4, niter = 4000, nadapt = 0.8).
}
}

\references{
Moreno et al, 2016
}

\author{
Artemis Toumazi 
}

\seealso{
\code{\link{scenarios}}.
}

\examples{
##############################################
############ Generate scenarios ##############
##############################################

limit_tox <- 10.96                  
N <- 42                         
param_pk <- c(2,10,100)             # PK parameters ka,CL,V
omega2 <- 0.7                       
omega_a <- 0                      
preal <- c(0.001,.05,0.1,.2,0.35,0.45)         
time_sampling <- seq(0,24,length.out=48)      
sigma2 <- c(rep(0.2,length(time_sampling)), rep(0.2,length(time_sampling)))          
# sigma2: Additive or proportional error

gen.scen <- scenarios(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N)

cohort = 1
scenarios <- gen.scen
d <- scenarios$doses
icon <- c(2:6,round(seq(9,48,((48-9)/4))))      
theta <- 0.2                                    
p_0 = NULL                                  
L = NULL

######################################
############### Models ###############
######################################

model = pktox  # Use one of the models in the dfpk package
betapriors = NULL

################################################
#### Set the stan model's options as a list ####
################################################

options <- list(nchains = 4,
                niter = 4000,
                nadapt = 0.9)   

############################################
############ Run the simulation ############
############################################

results_sim <- sim(d, N, cohort, icon, theta, p_0, L, model, gen.scen, betapriors, options)
results_sim
}