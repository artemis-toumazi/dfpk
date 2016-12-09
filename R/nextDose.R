#' Next dose determination of a phase I clinical trial.
#'
#' @param model A character string to specify the working model used in the method.
#' @param N The total number of enrolled patients.
#' @param y The toxicity outcome of each patient.
#' @param AUCs The AUCs values of each patient.
#' @param doses The dose levels of the drug.
#' @param x A vector of dose levels assigned to patients in the trial.
#' @param theta The toxicity (probability) target.
#' @param p_0 The skeleton of CRM; defaults to NULL. (must be defined only in the PKCRM model)
#' @param L A threshold set before starting the trial; defaults to NULL. (must be defined only in the PKCRM model)
#' @param betapriors A vector of the regression parameters in the model. 
#' @param D_AUC A vector specifying the difference between the AUCs and AUC_pop; defaults to NULL.
#' @param options The Stan model's options.
#'
#' @description 
#' nextDose is used to determine the next or recommended dose level in a phase I clinical trial using Pharmacokinetics (PK)
#' 
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
#' 
#' @examples
#' model <- pktox
#' N <- 15
#' p_0 = 0
#' L = 0
#' doses <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
#' theta <- 0.2
#' options <- list(nchains = 4,niter = 4000,nadapt = 0.9)
#' AUCs <- c(1.2303254, 3.3839503,4.3638522, 5.9376084,8.1720269, 9.8009405,
#' 1.5330900, 4.2166896, 5.4377306,7.3987646,10.1830398,12.2128046, 2.1126844,
#' 5.8108359 ,  7.4934992)
#' x <- c(1,1,1,2,2,2,2,2,3,3,3,3,2,3,4)
#' y <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
#' FALSE, FALSE, FALSE, FALSE ,FALSE, TRUE,FALSE, FALSE, FALSE)
#' D_AUC <- NULL
#' betapriors = NULL
#' nextDose(model, N, y, AUCs, doses, x, theta, p_0, L, betapriors, D_AUC, options)
#'
#' @seealso \code{\link{sim}}, \code{\link{nsim}}
#' 
#' @import ggplot2
#' @import rstan
#' @useDynLib dfpk, .registration = TRUE
#' @export
nextDose <- function(model, N, y, AUCs, doses, x, theta, p_0, L, betapriors, D_AUC = NULL, options){
	
	m <- model(y, AUCs, d=doses, x, theta, p_0, L, betapriors, D_AUC,options)
	MTD <- m$new_dose
	pstim <- m$pstim
	parameters <- m$parameters

	new("Dose", N = N, y = y, AUCs = AUCs, doses = doses, x = x, 
     theta = theta, options = options, new_dose=MTD, pstim=pstim, parameters=parameters)
}
