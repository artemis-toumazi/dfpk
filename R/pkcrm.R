#' @import ggplot2
#' @import rstan
#' @import Rcpp
#' @import dfcrm
#' @import methods
#' @import stats
#' @useDynLib dfpk, .registration = TRUE
#' @export
pkcrm <-
function(y,auc,doses,lev,theta,p_0,L,betapriors,D_AUC, options){
    num <- length(lev)        		# how many patients
    dose1 <- cbind(rep(1,num), log(doses[lev]))
    # For STAN
    data_s <- list(N=num, auc=log(auc), dose=dose1)
    sm_lrauc <- stanmodels$reg_auc2
    reg1 <- sampling(sm_lrauc, data=data_s, iter=options$niter, chains=options$nchains,
                     control = list(adapt_delta = options$nadapt))
    a1=get_posterior_mean(reg1)
    
    beta1 <- c(a1[1,options$nchains+1], a1[2,options$nchains+1])
    nu <- a1[3,options$nchains+1]
    mu <- beta1[1] + beta1[2]*log(doses)
    
    results_crm <- crm(p_0,theta,y,lev)$mtd 
    
    # Computation probability 
    p_new <- round(1-pnorm((L-mu)/sqrt(nu)),options$nchains+1)          
    result_safety <- order(abs(p_new-theta))[1]
    new_dose = min(results_crm, result_safety)
    
    list(new_dose = new_dose, pstim=p_new, parameters = c(beta1, nu))   
}
