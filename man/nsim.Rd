\name{nsim}
\alias{nsim}
\title{
More than one simulation using PK measure in the dose finding design
}
\description{
nsim is used to generate simulations of phase I trial using the PK data and then link them to toxicity under a specified dose-toxicity configuration. The objective is to determine the maximum tolerated dose (MTD). 
}
\usage{
nsim(d, N, cohort, icon, theta, p_0, L, model, scenarios,betapriors, options, TR)
}
\arguments{
  \item{d}{
A vector of dose levels assigned to patients.
}
  \item{N}{
The number of enrolled patients. 
}
  \item{cohort}{
The number of cohorts.
}
  \item{icon}{
The index of real blood sampling.
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
  \item{model}{
A character string to specify the working model used in the method.
}
  \item{scenarios}{
A scenarios with the PK data.
}
  \item{betapriors}{
A vector of the regression parameters for a specific model.
}
  \item{options}{
A list of three integers specifying the stan model's number of chains, how many iterations for each chain and the number of warmup iterations; defaults to options <- list(nchains = 4, niter = 4000, nadapt = 0.8).
}
  \item{TR}{
The number of simulations.
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
\code{\link{scenarios}},
\code{\link{sim}}.
}

\examples{
  \dontrun{
##############################################
############ Generate scenarios ##############
##############################################

TR = 100
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
d <- scenarios@doses
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

results_sim <- nsim(d, N, cohort, icon, theta, p_0, L, model, gen.scen, betapriors, options, TR)
results_sim
  }
}
