#' @import ggplot2
#' @import rstan
#' @export
dtox <-
function(y, auc, doses, lev, theta, p_0, L, betapriors,D_AUC,options){  
    
    num <- length(lev)    
    dose1 <- cbind(rep(1,num), log(doses[lev]))
    
    # For STAN model
    data_s <- list(N=num,y=y,dose=dose1,beta0mean=betapriors[1], 			beta1mean=betapriors[2])
    reg1 <- sampling(sm_lrDtox, data=data_s, iter=options$niter, chains=options$nchains,control = list(adapt_delta = options$nadapt))
    a1 = get_posterior_mean(reg1)
    
    beta <- a1[3:4,options$nchains+1]
    
    beta0 <- beta[1]
    beta1 <- beta[2] 
    p_new <- pnorm(beta0 + beta1*log(doses))     
    new_dose <- order(abs(p_new-theta))[1] 
    
    list(new_dose=new_dose, pstim = p_new, parameters=beta, posterior = a1)
}
