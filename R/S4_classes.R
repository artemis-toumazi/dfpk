setClassUnion("ClassNewDose", c("numeric", "logical", "NULL"))

#' An S4 class to represent a dosefinding results.
#'
#' @slot pid Patient's ID provided in the study.
#' @slot N  The total number of patients.
#' @slot time The time sampling.
#' @slot doses The dose levels of the drug.
#' @slot conc Concentration of the drug.
#' @slot p0  The skeleton of CRM. 
#' @slot L  A threshold set before starting the trial.
#' @slot nchains Number of chains for the stan model.
#' @slot niter Number of iterations for the stan model.
#' @slot nadapt  Number of warmup iterations for the stan model.
#' @slot newDose  The next maximum tolerated dose (MTD) if TR=1 else the selected % of MTD.
#' @slot MTD The vector containing all the maximum tolerated dose for each TR.
#' @slot MtD The next maximum tolerated dose (MTD).
#' @slot theta  The toxicity (probability) target.
#' @slot doseLevels A vector of dose levels assigned to patients in the trial.
#' @slot toxicity The toxicity outcome.
#' @slot AUCs A vector with the computed AUC values of each patient.
#' @slot TR The number of replicates clinical trials.
#' @slot preal The real probability of toxicity.
#' @slot pstim The estimated mean probabilities of toxicity.
#' @slot pstimQ1 The 1st quartile of estimated probability of toxicity.
#' @slot pstimQ3 The 3rd quartile of estimated probability of toxicity.
#' @slot model A character string to specify the working model used in the method.
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export
setClass("dosefinding", slots = list(pid="numeric", N ="numeric", time="numeric", doses = "numeric", conc="numeric", 
        p0 = "numeric", L = "numeric",  nchains = "numeric", niter = "numeric", nadapt = "numeric", newDose = "ClassNewDose", 
        MTD = "ClassNewDose", MtD = "numeric", theta = "numeric", doseLevels="matrix", toxicity= "matrix", AUCs="matrix", TR="numeric", 
        preal = "numeric", pstim = "list", pstimQ1 = "list", pstimQ3 = "list", model = "character"))


#' An S4 class to represent a simulated scenarios.
#'
#' @slot PKparameters The subject's PK parameters.
#' @slot nPK The length of time sampling.
#' @slot time The time sampling.
#' @slot N  The total number of patients.
#' @slot doses The doses levels of the drug. 
#' @slot preal The real probability of toxicity.
#' @slot limitTox  Threshold in the toxicity. 
#' @slot omegaIIV  A standard deviation of clearance (CL) and volume (V).
#' @slot omegaAlpha A standard deviation omegaAlpha.
#' @slot conc The concentration of the drug (concentration without error).
#' @slot concPred The predicted concentration of the drug (concentration with error). 
#' @slot tox The toxicity outcomes.
#' @slot tab The data summary of scenarios' outcomes.
#' @slot parameters The PK parameter's estimations of each patient.
#' @slot alphaAUC AUC with sensitivity parameter. 
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export
setClass("scen", slots = list(PKparameters="numeric", nPK="numeric", time="numeric", 
        N = "numeric", doses="numeric", preal = "numeric", limitTox="numeric", omegaIIV="numeric", 
        omegaAlpha="numeric", conc="matrix", concPred="numeric",
        tox="matrix", tab="matrix", parameters = "matrix", alphaAUC="numeric"))


#' An S4 class to present the next recommended dose level in an ongoing trial.
#'
#' @slot N The total number of enrolled patients.
#' @slot y The toxicity outcome of each patient.
#' @slot AUCs A vector with the computed AUC values of each patient.
#' @slot doses The doses levels of the drug.
#' @slot x A vector of dose levels assigned to patients in the trial.
#' @slot theta The toxicity (probability) target.
#' @slot options The Stan model's options.
#' @slot newDose The next recommended dose level.
#' @slot pstim The estimated mean probabilities of toxicity.
#' @slot pstimQ1 The 1st quartile of estimated probability of toxicity.
#' @slot pstimQ3 The 3rd quartile of estimated probability of toxicity.
#' @slot parameters The Stan model's estimated parameters.
#' @slot model A character string to specify the working model used in the method.
#' @import methods
#' @useDynLib dfpk, .registration = TRUE
#' @export
setClass("Dose", slots = list(N = "numeric", y = "numeric", AUCs = "numeric", doses ="numeric", x = "numeric", 
        theta = "numeric", options = "list", newDose="ClassNewDose", pstim="numeric", pstimQ1="numeric", pstimQ3="numeric", 
        parameters="numeric", model = "character"))


setGeneric("show")
#' @export 
setMethod(f = "show", signature ="dosefinding", definition = function(object)
    {
        cat("Today: ", date(), "\n") 
        cat("\n","A. Data Summary (", object@model, "model)", "\n")
        cat("Number of simulations:", object@TR, "\n")
        cat("Total number of patients in the trial:", object@N, "\n")
        cat("The time sampling:", round(object@time, digits = 3), "\n")
        n <- object@N
        cat("Levels of Doses:", round(object@doses, digits=3), "\n")
        cat("Concentration of the drug:", round(object@conc, digits = 3), "\n")
        if(object@model == "pkcrm"){
        	cat("Skeleton of CRM:", object@p0, "\n")
        	cat("Threshold set before starting the trial:", object@L, "\n")
        }
        cat("\n","B. STAN Model's Options \n")
        cat("The Stan model runs with", object@nchains, "MCMC chains ")
        cat("which each chain has", object@niter, "iterations ")
        cat("and", object@nadapt, "warmup iterations \n")

        if(object@TR == "1"){
            cat("\n","C. Dose-Finding Results: \n")
            cat("PID", "\t", "Level", "\t", "Toxicity", "\t", "AUCs", "\n")
            for (i in 1:n){
                cat(i,"\t", object@doseLevels[i],"\t", object@toxicity[i],"\t", "\t", round(object@AUCs[i], digits=3) ,"\n")
            }
            cat("\nThe prior probabilities of toxicity:", round(object@preal, digits=4), "\n")
            cat("Next recommended dose level:", object@newDose, "\n")
            cat("Recommendation is based on a target toxicity probability of:",object@theta, "\n")
        }else{
            cat("\n","C. Dose-Finding Results: \n")
            doselevels <- as.vector(object@doseLevels)
            t <- matrix(NA, nrow=4, ncol=length(object@doses)+1)
            rownames(t) <- c("Dose", "Truth Probabilities", "Dose-Allocation (%)", "Selected % MTD")
            colnames(t) <- rep("", length(object@doses)+1)
            t[1, ] <- seq(0, 6)
            t[2, ] <- c(0, round(object@preal, digits=3))
            for(i in 1:length(object@doses)){
              n_levels = length(which(doselevels == i))
              t[3, i+1] <- round(n_levels/length(doselevels), digits=2)
            }
            zeroDose <- length(which(doselevels == "NA"))
            t[3,1] <- zeroDose / length(doselevels)
            t[4, ] <- round(object@newDose, digits=2)
            print(t)
            cat("Recommendation is based on a target toxicity probability of:",object@theta, "\n")
        }
    }
)

setGeneric("show")
#' @export
setMethod(f = "show",
          signature ="scen",
          definition = function(object){
               cat("Today: ", date(), "\n")
               cat("\n","Scenarios Settings","\n","\n")
               cat("Total number of patients in the trial:", "\t", object@N, "\n")
               cat("The subject's PK parameters (ka, CL, V):", object@PKparameters)
               cat(" with a standard deviation of CL and V equals to:", object@omegaIIV, "\n")
               cat("The doses of the drug:", round(object@doses, digits=3), "\n")
               cat("The real probabilities of toxicity:", round(object@preal,digits=3), "\n")
               cat("Time after the drug administration (hours):", "\n", round(object@time, digits = 3), "\n")
               cat("Threshold on the toxicity:", object@limitTox, "\n")
               cat("\n", "Simulated Scenarios \n")
               cat("\n", "Amount of drug in a given volume of plasma is (i.e. concentration):", "\n")
               print(round(object@conc, digits = 3))
               cat("\n")
               cat("\n", "AUC with the sensitivity parameter", "\n")
               print(round(object@alphaAUC, digits = 3))
               cat("\n")
               cat("\n", "with standard deviation omega for parameter alpha=", object@omegaAlpha, "\n")
               cat("\n", "Toxicity (0 indicates no toxicity and 1 toxicity) :","\n")
               print(object@tox)
               cat("\n")
               cat("The PK parameter's estimations for each patient are:", "\n")
               print(round(object@parameters, digits=3))
               cat("\n","NB. pid = Patient's ID number \n")
         }
)

setGeneric("show")
#' @export 
setMethod(f = "show",
          signature ="Dose",
          definition = function(object) {
               cat("Today: ", date(), "\n")
               cat("Model:", object@model, "\n")
               cat("Total number of enrolled patients in the trial: ", object@N, "\n")
               cat("Levels of doses: ", object@doses, "\n")
               cat("The Next Recommended Dose: ", "\n")
               print(object@newDose)
               cat("\n")
               cat("Estimated probability of toxicity: ", "\n")
               print(round(object@pstim, digits=4))
               cat("\n")
               cat("Estimated model's parameters: ", "\n")
               print(object@parameters)
               cat("\n")
        }
)

###########################################
################ Plots ####################
###########################################

setGeneric("plot")
#' The graphical representation of dose escalation for each patient in the trial. 
#' 
#' @param x a "dosefinding" object.
#' @param y the "y" argument is not used in the plot-method for "scen" object.
#' @param TR The number of replicates clinical trials.
#' @param ask Choose plot or not.
#' @param \dots other arguments to the \code{\link[=graphics]{plot.default}} function can be passed here.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#' 
#' @references Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal.
#' 
#' @import methods
#' @import stats
#' @import graphics
#' @importFrom grDevices  rainbow
#' @useDynLib dfpk, .registration = TRUE
#' @importFrom utils menu
#' @export
setMethod(f = "plot", signature =c("dosefinding", "missing"), definition = function(x, y=NA, TR=1, ask=TRUE, ...){
    choices <- c("1: Plot trial summary", "2: Plot posterior dose response with 95% CI", "3: Boxplot of sampling dose response\n")
    if (ask == "TRUE") {
        cat("Make a plot selection (or 0 to exit)\n\n")
        for (i in 1:length(choices)) {
            cat(choices[i], "\n")
        }
        pick <- readline("Selection: ")
    } else {
        pick <- 1
    }
    num_choices <- c(0:length(choices))
    while(!(pick %in% num_choices)) {
        cat("Invalid choice. Enter an item from the menu, or 0 to exit")
        pick <- readline("Selection: ")
    }
    if (pick == 0) {
        return(invisible(x));
    } else if (pick == 1) {
        par(las=1)
        n <- x@N                    
        nontox <- which(x@toxicity[TR,] == "0")
        notNa <- which(is.na(x@doseLevels[TR,]) == "FALSE")
        if(x@MtD == 0) warning("Plot not completed! The trial has stopped according to the stopping rules! \n \n", call. = FALSE)
        plot(x@pid[nontox], x@doseLevels[TR,nontox], pch="O", ylim=c(1,max(x@doseLevels[TR,notNa])), xlim=c(1,n), 
        	 xlab="Patient number", ylab="Dose level", ...)
        points((1:length(x@toxicity[TR,]))[-nontox],x@doseLevels[TR,-nontox], pch="X")
        mtext("Each point represents a patient", line=2)
        mtext("A circle indicates no toxicity, a cross toxicity", line=0.5)
    } else if (pick == 2) {
        par(las=1)
        n <- x@N
        ndoses <- length(x@doses)
        # PropTox <- matrix(NA, ncol = 6, nrow = ndoses)
        # for(i in 1: ndoses){
        #    PropTox[i,] <-  rbind(summary(x@pstim_post[i,]))
        # }
        if (x@MtD == 0) stop("Unable to plot! The trial stopped based on the stopping rules \n \n", call. = FALSE)
        plot(1:ndoses, x@pstim[[TR]][1:ndoses,n],type="l",xlab="Dose level",ylab="Probability of toxicity", ylim=c(0,max(x@pstim[[TR]][1:ndoses,n]) + 0.15))
        points(1:ndoses,x@pstim[[TR]][1:ndoses,n], pch="X")
        lines(1:ndoses,x@preal, lty=2)
        points(1:ndoses,x@preal, pch="O")
        abline(h=x@theta, lwd=2, lty=4, col = "red")
        lines(1:ndoses,x@pstimQ1[[TR]][1:ndoses,n], lty=3, col = "blue")
        lines(1:ndoses,x@pstimQ3[[TR]][1:ndoses,n], lty=3, col = "blue")
        # lines(1:ndoses, PropTox[,2], lwd=2, lty=3, col = "orange")
        # lines(1:ndoses, PropTox[,5], lwd=2, lty=3, col = "orange") 
        mtext("Prior (dashed) and updated (solid) dose-toxicity curves", line=2)
        mtext("95% CI (dotted) of the updated dose-toxicity curve", line=0.5)
    } else {
        par(las=1)
        ndoses <- length(x@doses)
        if (x@MtD == 0) stop("Unable to plot! The trial stopped based on the stopping rules \n \n", call. = FALSE)
        PropTox <- matrix(NA, ncol = 6, nrow = ndoses)
        for(i in 1: ndoses){
            #PropTox[i,] <-  rbind(summary(x@pstim[i,]))
            PropTox[i,] <- rbind(summary(unlist(lapply(x@pstim, `[`,i,))))
        }
        d <- c(1:ndoses)
        boxplot(PropTox~d, xlab="Dose level", ylab="Probability of toxicity", ylim=c(0,max(PropTox) + 0.15))
        abline(h=x@theta, lwd=2, lty=4, col = "red")
        mtext("Boxplot dose response", line=1)
        # if (x@newDose == "NA") mtext("(Note: The trial stopped based on the stopping rules)", line=0)
    }
})

#' The graphical representation of the drug's concentration in the plasma at time t after the drug administration. 
#'  
#' @param x a "scen" object.
#' @param y the "y" argument is not used in the plot-method for "scen" object.
#' @param \dots other arguments to the \code{\link[=graphics]{plot.default}} function can be passed here.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#'
#' @references Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal.
#'
#' @import methods
#' @import stats
#' @import graphics
#' @importFrom grDevices  rainbow
#' @useDynLib dfpk, .registration = TRUE
#' @export
setMethod(f = "plot", signature =c("scen", "missing"), definition = function(x, y=NA, ...){
	colors <- rainbow(length(x@doses))
	plot(x@time, x@conc[,1], type="l", col=colors[1], xlab="Time (hours)", ylab="Concentration (mg/L)", main="Pharmacokinetics: Concentration vs Time", ylim=c(0,max(x@conc)),...)
	for(i in 2:length(x@doses)){
		lines(x@time, x@conc[,i], col=colors[i], lty=i,...)
	}
}
)

#' The graphical representation of dose escalation for each patient in the trial. 
#'
#' @param x a "Dose" object.
#' @param y the "y" argument is not used in the plot-method for "Dose" object.
#' @param ask Choose plot or not.
#' @param \dots other arguments to the \code{\link[=graphics]{plot.default}} function can be passed here.
#'
#' @author Artemis Toumazi \email{artemis.toumazi@@inserm.fr}, Moreno Ursino \email{moreno.ursino@@inserm.fr}, Sarah Zohar \email{sarah.zohar@@inserm.fr}
#'
#' @references Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal.
#'
#' @import methods
#' @import stats
#' @import graphics 
#' @importFrom grDevices  rainbow
#' @useDynLib dfpk, .registration = TRUE
#' @export
setMethod(f = "plot", signature =c("Dose", "missing"), definition = function(x, y=NA, ask=TRUE, ...){
    choices <- c("1: Plot trial summary", "2: Plot posterior dose response with 95% CI\n")
    if (ask == "TRUE") {
        cat("Make a plot selection (or 0 to exit)\n\n")
        for (i in 1:length(choices)) {
            cat(choices[i], "\n")
        }
        pick <- readline("Selection: ")
    } else {
        pick <- 1
    }
    num_choices <- c(0:length(choices))
    while(!(pick %in% num_choices)) {
        cat("Invalid choice. Enter an item from the menu, or 0 to exit")
        pick <- readline("Selection: ")
    }
    if (pick == 0) {
        return(invisible(x));
    } else if (pick == 1) {
        par(las=1)
		n <- x@N
		pid <- 1:n              
		nontox <- which(x@y == "0")
		plot(pid[nontox], x@x[nontox], pch="O", ylim=c(1,max(x@x)), xlim=c(1,n+1), xlab="Patient number", ylab="Dose level",...)
		points((1:length(x@y))[-nontox],x@x[-nontox], pch="X")
		mtext("Each point represents a patient", line=2)
		mtext("A circle indicates no toxicity, a cross toxicity", line = 0.5)
    } else {
        par(las=1)
        n <- x@N
        ndoses <- length(x@doses)
        plot(1:ndoses, x@pstim, type="l", xlab="Dose level", ylab="Probability of toxicity", ylim=c(0,max(x@pstim) + 0.15))
        points(1:ndoses,x@pstim, pch="X")
        abline(h=x@theta, lwd=2, lty=4, col = "red")
        lines(1:ndoses,x@pstimQ1, lty=3, col = "blue")
        lines(1:ndoses,x@pstimQ3, lty=3, col = "blue")
        mtext("Updated (solid) dose-toxicity curves with the", line=1.5)
        mtext("95% CI (dotted) of the updated dose-toxicity curve", line=0.5)
    }
})
