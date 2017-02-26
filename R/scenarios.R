#' @useDynLib dfpk, .registration = TRUE
#' @export
scenarios <-
function(param_pk,omega2,omega_a,sigma2,preal,limit_tox,time_sampling, N){
    
    pk.model <- function(t,dose,ka,CL,V) {
        dose*ka/V/(ka-CL/V)* (exp(-CL/V*t)-exp(-ka*t))  
    }
    
    n_pk <- length(time_sampling)       		
    doses <- exp(qnorm(preal)*sqrt(omega2^2+omega_a^2) + log(limit_tox) + log(param_pk[2]))    
    parameter <- NULL               			
    sens_AUC <- NULL                			
    alphatot <- NULL				 			
    
    tab <- c(0,time_sampling,time_sampling)	 
    npar <- length(param_pk) - 1 	             
    
    for(i in 1:N) { 
        ipar <- (param_pk[2:3])*exp(rnorm(npar,sd=omega2))
        ipar <- c(param_pk[1],ipar)
        parameter <- rbind(parameter,ipar)
        for(j in doses){
            concen <- pk.model(time_sampling, dose=j, ka = ipar[1], CL = ipar[2], V = ipar[3]) 
            conc_pred <- concen*(1+rnorm(2*n_pk,sd=sigma2))   	# real concentrations + predictions of them
            tab <- rbind(tab,c(i,conc_pred))     		
            alfa <- exp(rnorm(1,sd=omega_a))    					
            CL <- ipar[2]				   
            sens <- alfa*j/CL           
            sens_AUC <- c(sens_AUC,sens)   
            alphatot <- c(alphatot,alfa)  
        }
    }
    for(i in 1:N){
        row.names(parameter)[i] <- "pid"
        colnames(parameter) <- c("ka", "CL", "V")
    }
 
       
    #####################################
    ########## Concentration  ###########
    #####################################
    
    concentration <- NULL
    for(k in 1:length(doses)){
        conc <- pk.model(time_sampling, dose=doses[k], ka = ipar[1], CL = ipar[2], V = ipar[3])
        concentration <- cbind(concentration,conc)
    }

    tox <- matrix(sens_AUC >= limit_tox , ncol= length(doses), byrow=T)
    data <- list( n_pk=n_pk, conc=conc_pred[1:n_pk], conc_pred=conc_pred, doses=doses, tab=tab, tox=tox, parameters=parameter, alfa_AUC=sens_AUC)

    new("scen", param_pk=param_pk, n_pk= n_pk, time=time_sampling, N=N, doses=doses,
              limit_tox=limit_tox, omega2=omega2, omega_a=omega_a, conc=concentration, 
              conc_pred=conc_pred, tox=tox, tab=tab, parameters=parameter, alfa_AUC=sens_AUC
    )
}

