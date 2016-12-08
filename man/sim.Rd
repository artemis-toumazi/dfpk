\name{sim}
\alias{sim}
\title{
A single-trial simulation using pharmacokinetic's (PK) measure in the dose finding design.
}

\description{
The sim function can be used to simulate a single phase I clinical trial using the PK data and then link them to toxicity under a specified dose-toxicity configuration. It can be used to determine the maximum tolerated dose (MTD). 
}

\usage{
sim(d, N, cohort, icon, theta, p_0, L, model, scenarios, betapriors, options)
}

\arguments{
  \item{d}{
A vector of dose levels assigned to patients.
}
  \item{N}{
The total number of enrolled patients. 
}
  \item{cohort}{
The number of cohorts in the trial.
}
  \item{icon}{
The index of real blood sampling.
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
  \item{model}{
A character string to specify the working model which will be used in the simulation.
}
  \item{scenarios}{
A generated scenario including the PK data.
}
  \item{betapriors}{
A vector of the regression parameters in the selected model. 
}
  \item{options}{
A list of three integers specifying the stan model's options: the number of chains, the number of iterations for each chain and the number of warmup iterations.
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
\code{\link{scenarios}}.
}

\examples{
  \dontrun{
limit_tox <- 10.96                  
N <- 42                         
param_pk <- c(2,10,100)             # PK parameters ka,CL,V
omega2 <- 0.7                       
omega_a <- 0                      
preal <- c(0.001,.05,0.1,.2,0.35,0.45)         
time_sampling <- seq(0, 24, length.out=48)      
sigma2 <- c(rep(0.2, length(time_sampling)), rep(0.2, length(time_sampling)))          

# Generate data from a scenario
gen.scen <- scenarios(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N) 

cohort = 1
scenarios <- gen.scen
d <- scenarios@doses
icon <- c(2:6,round(seq(9,48,((48-9)/4))))      
theta <- 0.2                                    
p_0 = 0                                  
L = 0

##############################################
############### Selected Model ###############
##############################################

model = pktox   
betapriors = 0

################################################
#### Set the Stan model's options as a list ####
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
}