#' Next dose determination of a phase I clinical trial.
#'
#' @param model A character string to specify the working model used in the method.
#' @param y A binary vector of patient's toxicity outcomes; 1 indicates a toxicity, 0 otherwise.
#' @param AUCs A vector with the computed AUC values of each patient for pktox, pkcrm, pklogit and pkpop; defaults to NULL. 
#' @param doses A vector with the doses panel.
#' @param x A vector with the dose level assigned to the patients.
#' @param theta The toxicity target.
#' @param options List with the Stan model's options.
#' @param prob The probability of toxicity for the corresponding stopping rule of the selected model; defaults to 0.9. See for details \code{\link{dtox}}, \code{\link{pkcov}}, \code{\link{pkcrm}}, \code{\link{pktox}}, \code{\link{pkpop}}, \code{\link{pklogit}}.
#' @param betapriors A vector with the value for the prior distribution of the regression parameters in the model; defaults to NULL.
#' @param thetaL A second threshold of AUC; must be defined only in the PKCRM model.
#' @param p0 The skeleton of CRM for pkcrm; defaults to NULL. (must be defined only in the PKCRM model).
#' @param L The AUC threshold to be set before starting the trial for pklogit, pkcrm and pktox; defaults to NULL. (must be defined only in the PKCRM model).
#' @param deltaAUC The difference between computed individual AUC and the AUC of the population at the same dose level (defined as an average); argument for pkcov; defaults to NULL.
#' 
#' @description
#' nextDose is used to determine the next or recommended dose level in a phase I clinical trial using Pharmacokinetics (PK).
#'
#' @return  An object of class "Dose" is returned, consisting of determination of the next recommended dose and estimations. Objects generated 
#' by nextDose contain at least the following components:
#'
#' \item{N}{The total number of enrolled patients.}
#' \item{y}{A binary vector of patient's toxicity outcomes; 1 indicates a toxicity, 0 otherwise.}
#' \item{AUCs}{A vector with the computed AUC values of each patient.}
#' \item{doses}{A vector with the doses panel.}
#' \item{x}{A vector with the dose level assigned to the patients.}
#' \item{theta}{Tocixity target.}
#' \item{options}{List with the Stan model's options.}
#' \item{newDose}{The next recommended dose level; equals to "NA" if the trial has stopped, according to the stopping rules.}
#' \item{pstim}{The mean values of estimated probabilities of toxicity.}
#' \item{pstimQ1}{The 1st quartile of estimated probabilities of toxicity.}
#' \item{pstimQ3}{The 3rd quartile of estimated probabilities of toxicity.}
#' \item{parameters}{The estimated model's parameters.}
#' \item{model}{A character string of the selected working dose-finding model.}
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#'
#' @references Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal. 
#'
#' @examples
#'   \dontrun{
#' doses <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
#' theta <- 0.2 
#' options <- list(nchains = 4, niter = 4000, nadapt = 0.9)     
#' AUCs <- c(1.208339,  5.506040,  6.879835,  3.307928,  3.642430, 
#'           10.271291,  3.885522,  3.086622,  2.537158,  5.525917,  
#'           8.522176,  4.642741, 11.048531, 10.246976,  5.226807)
#' x <- c(1, 2, 3, 4, 5, 6, 4, 4, 4, 5, 5, 4, 4, 5, 5)
#' y <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)
#' nextD <- nextDose(model = "pktox", y=y, AUCs=AUCs, doses=doses, 
#'                    x=x, theta=theta, options=options)
#' }
#'
#' @seealso \code{\link{nsim}}
#'
#' @import ggplot2
#' @import rstan
#' @useDynLib dfpk, .registration = TRUE
#' @export
nextDose <- function(model, y, AUCs, doses, x, theta, options, prob = 0.9, betapriors = NULL, thetaL=NULL, p0 = NULL, L = NULL, deltaAUC = NULL){
	model1 = NULL
	eval(parse(text = paste("model1 =", model, sep="")))
	N <- length(x)
	if (model == "pktox" & is.null(betapriors)){betapriors = c(10000, 20, 10)
	}else if(model == "pkcrm" & is.null(betapriors)){betapriors = 10000
	}else if (model == "pkpop" & is.null(betapriors)){betapriors = c(10000, 10, 5)
	}else if (model == "dtox" & is.null(betapriors)){betapriors = c(6.71, 1.43)
	}else if(model == "pkcov" & is.null(betapriors)){betapriors = c(-14.76, 3.23)
	}else if (model == "pklogit" & is.null(betapriors)){betapriors = c(10000, 20, 10)}
	m <- model1(y, AUCs, d = doses, x, theta, prob = prob, betapriors = betapriors, thetaL=NULL, options = options, p0 = p0, L = L, deltaAUC = deltaAUC)
	MTD <- m$newDose
	pstim <- m$pstim
	pstim_Q1 <- m$p_sum[,2]
    pstim_Q3 <- m$p_sum[,5]
	parameters <- m$parameters
	new("Dose", N = N, y = y, AUCs = AUCs, doses = doses, x = x, theta = theta, options = options, 
		newDose = MTD, pstim = pstim, pstimQ1 = pstim_Q1, pstimQ3 = pstim_Q3, parameters = parameters, 
		model = model)
}
