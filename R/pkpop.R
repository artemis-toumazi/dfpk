#' @import rstan
#' @import parallel
#' @import methods
#' @import stats 
#' @import arm
#' @export
pkpop <-
function(y,auc,doses,lev,theta,p_0,L,betapriors,D_AUC, options){

    num <- length(lev)                        
    dose1 <- cbind(rep(1,num), log(doses[lev]))
    
    # For STAN model
    data_s <- list(N=num,auc=log(auc),dose=dose1)
    reg1 <- sampling(sm_lrauc, data=data_s, iter=options$niter, chains=options$nchains,
                     control = list(adapt_delta = options$nadapt))
    
    a1 = get_posterior_mean(reg1)
    beta1 <- a1[1:2,options$nchains+1]
    mu <- beta1[1] + beta1[2]*log(doses)
    nu <- a1[3,options$nchains+1]
    
    mu1 <- cbind(rep(1,num), mu[lev])
    
    # For STAN model
    data_s <- list(N=num,y=y,dose=mu1)
    reg2 <- sampling(sm_lrPkpop, data=data_s, iter=options$niter, chains=options$nchains,
                     control = list(adapt_delta = options$nadapt))
    a2 = get_posterior_mean(reg2)
    
    Beta <- a2[3:4,options$nchains+1]
    # Computation probability  
    
    p_stim = invlogit(Beta[1] + Beta[2]*mu)
    new_dose = order((abs(p_stim-theta)))[1]
    
    list(new_dose = new_dose, pstim = p_stim, parameters = c(beta1,nu,Beta), posterior1 = a1, posterior2 = a2)  
}
