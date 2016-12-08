#' An S4 class to represent a dosefinding results.
#'
#' @slot model A character string to specify the working model used in the method.
#' @slot pid Patient ID provided in the study.
#' @slot N  The total number of patients.
#' @slot time The time sampling.
#' @slot dose The doses levels of the drug.
#' @slot conc Concentration of the drug.
#' @slot p_0  The skeleton of CRM. 
#' @slot L  A threshold set before starting the trial.
#' @slot nchains Number of chains for the stan model.
#' @slot niter Number of iterations for the stan model.
#' @slot nadapt  Number of warmup iterations for the stan model.
#' @slot new_dose  The next maximum tolerated dose (MTD).
#' @slot theta  The toxicity (probability) target.
#' @slot dose_levels A vector of dose levels assigned to patients in the trial.
#' @slot toxicity The toxicity outcome.
#' @slot AUCs The AUCs.
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export
setClass("dosefinding", slots = list(model="character", pid="numeric", N ="numeric",time="numeric", dose = "numeric", conc="numeric", 
          p_0 = "numeric", L = "numeric",  nchains = "numeric", niter = "numeric", nadapt = "numeric", new_dose = "numeric", 
          theta = "numeric", dose_levels="matrix", toxicity= "matrix", AUCs="matrix"))


#' An S4 class to represent a simulated scenarios.
#'
#' @slot param_pk The subject's PK parameters.
#' @slot n_pk The length of time sampling.
#' @slot time The time sampling.
#' @slot N  The total number of patients.
#' @slot doses The doses levels of the drug. 
#' @slot limit_tox  Threshold in the toxicity. 
#' @slot omega2  A standard deviation of CL and V.
#' @slot omega_a A standard deviation omega_a.
#' @slot conc The concentration of the drug (concentration without error).
#' @slot conc_pred The predicted concentration of the drug (concentration with error). 
#' @slot tox The toxicity outcomes.
#' @slot tab The data summary of scenarios' outcomes.
#' @slot parameters The PK parameter's estimations of each patient.
#' @slot alfa_AUC AUC with sensitivity parameter. 
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export
setClass("scen", representation(param_pk="numeric", n_pk="numeric", time="numeric", 
          N = "numeric", doses="numeric", limit_tox="numeric", omega2="numeric", 
          omega_a="numeric", conc="matrix", conc_pred="numeric",
          tox="matrix", tab="matrix", parameters="matrix", alfa_AUC="numeric"
          ))


#' An S4 class to present the next recommended dose level in an ongoing trial.
#' 
#' @slot N A total number of enrolled patients.
#' @slot y The toxicity outcome of each patient.
#' @slot AUCs The AUCs values of each patient.
#' @slot doses The doses levels of the drug.
#' @slot x A vector of dose levels assigned to patients in the trial.
#' @slot theta The toxicity (probability) target.
#' @slot options The Stan model's options.
#' @slot new_dose The next recommended dose level.
#' @slot pstim The estimated probability of toxicity.
#' @slot parameters The Stan model's estimated parameters.
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export 
setClass("Dose", slots = list(N = "numeric", y = "logical", AUCs = "numeric", doses ="numeric", x = "numeric", 
     theta = "numeric", options = "list", new_dose="numeric", pstim="numeric", parameters="numeric"
     ))


#' @export
setMethod(f = "show",
          signature ="dosefinding",
          definition = function(object) {
               cat("Today: ", date(), "\n") 
     		cat("\n","Data Summary", "\n")
     	     cat("Total number of patients in the trial:", object@N, "\n")
     	     cat("The time sampling:", object@time, "\n")
               cat("Levels of Doses:", object@dose, "\n")
               cat("Concentration of the drug:", object@conc, "\n")
               cat("Skeleton of CRM:", object@p_0, "\n")
               cat("Threshold set before starting the trial:", object@L, "\n")
               cat("\n","STAN Model's Options \n")
               cat("The Stan model runs with", object@nchains, "MCMC chains ")
               cat("which each chain has", object@niter, "iterations ")
               cat("and", object@nadapt, "warmup iterations \n")
               cat("\n","Dose-Finding Results: \n")
               cat("Next recommended dose level:", object@new_dose, "\n")
               cat("Recommendation is based on a target toxicity probability of:",object@theta, "\n")
               cat("Dose escalation in the trial:","\n", object@dose_levels,"\n")
               cat("Toxicity Outcomes (FALSE indicates no toxicity and TRUE toxicity):","\n")
               print(object@toxicity)
               cat("AUCs: \n")
               print(object@AUCs)
               cat("\n")
         }
)

#' @export
setMethod(f = "show",
          signature ="scen",
          definition = function(object) {
               cat("Today: ", date(), "\n")
               cat("\n","Scenarios Settings","\n","\n")
               cat("Total number of patients in the trial:", "\t", object@N, "\n")
               cat("The subject's PK parameters (ka, CL, V):", object@param_pk) 
               cat(" with a standard deviation of CL and V equals to:", object@omega2, "\n")
               cat("The doses of the drug:", object@doses, "\n")
               cat("Time after the drug administration (hours):", "\n", object@time, "\n")
               cat("Threshold on the toxicity:", object@limit_tox, "\n")
               cat("\n", "Simulated Scenarios \n")
               cat("\n", "Amount of drug in a given volume of plasma is (i.e. concentration):", "\n")
               print(object@conc)
               cat("\n")
               cat("\n", "AUC with the sensitivity parameter", "\n")
               print(object@alfa_AUC) 
               cat("\n")
               cat("\n", "with standard deviation omega_a=", object@omega_a, "\n")
               cat("\n", "Toxicity (FALSE indicates no toxicity and TRUE toxicity) :","\n")
               print(object@tox)
               cat("\n")
               cat("The PK parameter's estimations for each patient are:", "\n")
               print(object@parameters)
               cat("\n")
         }
) 

#' @export
setMethod(f = "show",
          signature ="Dose",
          definition = function(object) {
               cat("Today: ", date(), "\n")
               cat("Total number of enrolled patients in the trial: ", object@N, "\n")
               cat("Levels of doses: ", object@doses, "\n")
               cat("The Next Recommended Dose: ", "\n")
               print(object@new_dose)
               cat("\n")
               cat("Estimated probability of toxicity: ", "\n")
               print(object@pstim)
               cat("\n")
               cat("Estimated model's parameters: ", "\n")
               print(object@parameters)
               cat("\n")
          }
)


###########################################
################ Plots ####################
###########################################

#' The generic function of the plot.dose
#'
#' @param x A "dosefinding" object.
#' @param ... Other parameters to be passed through to plotting functions. They are ignored in this function.
#' @description 
#' A graphical representation of dose escalation for each patient in the trial.
#' 
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
#' 
#' @seealso \code{\link{plotConc}}
#' 
#' @import graphics
#' @useDynLib dfpk, .registration = TRUE
#' @export
setGeneric(name="plotDose",def = function(x,...){standardGeneric("plotDose")}
)


#' The graphical representation of dose escalation for each patient in the trial. 
#' 
#' @param x A "dosefinding" object.
#' @param ... Other parameters to be passed through to plotting functions. They are ignored in this function.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
#' 
#' @seealso \code{\link{plotConc}}
#'
#' @import methods
#' @import stats
#' @import graphics
#' @useDynLib dfpk, .registration = TRUE
#' @export 
setMethod(f ="plotDose", 
          signature ="dosefinding", 
          definition = function(x,...){
               par(las=1)
               nontox <- which(x@toxicity == "FALSE")
               plot(x@pid[nontox], x@dose_levels[nontox], pch="O", ylim=c(1,max(x@dose_levels)), xlim=c(1,length(x@pid)), xlab="Patient number", ylab="Dose level",...)
               points((1:length(x@toxicity))[-nontox],x@dose_levels[-nontox], pch="X")
               mtext("Each point represents a patient", line=2)
               mtext("A circle indicates no toxicity, a cross toxicity", line=0.5)
          }
)

#' The generic function of plot.conc
#' 
#' @param x A "scen" object.
#' @param ... Other parameters to be passed through to plotting functions. They are ignored in this function.
#' @description 
#' A graphical representation of drug's concentration in the plasma at time t.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
#' 
#' @seealso \code{\link{plotDose}}
#'
#' @import graphics
#' @useDynLib dfpk, .registration = TRUE
#' @export
setGeneric(name="plotConc",def = function(x,...){standardGeneric("plotConc")}
)


#' The graphical representation of the drug's concentration in the plasma at time t after the drug administration.
#' 
#' @param x A "scen" object.
#' @param ... Other parameters to be passed through to plotting functions. They are ignored in this function.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2016) Dose-finding methods using pharmacokinetics in small populations (under review).
#' 
#' @seealso \code{\link{plotDose}}
#'
#' @import methods
#' @import stats
#' @import graphics
#' @importFrom grDevices  rainbow
#' @useDynLib dfpk, .registration = TRUE
#' @export
setMethod(f ="plotConc" ,signature ="scen", 
     definition = function(x,...){
          colors <- rainbow(length(x@doses))
          plot(x@time, x@conc[,1], type="l", col=colors[1], xlab="Time (hours)", ylab="Concentration (mg/L)", main="Pharmacokinetics: Concentration vs Time", ylim=c(0,max(x@conc)),...)
     	    for(i in 2:length(x@doses)){
        	      lines(x@time, x@conc[,i], col=colors[i], lty=i,...)
    	          }
	}
)

