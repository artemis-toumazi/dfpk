#' @import rstan
#' @import parallel
#' @import methods
#' @import stats 
#' @import arm
#' @export
pkcov <-
function(y,auc,doses,lev,theta,p_0,L,betapriors, D_AUC, options){

    num <- length(lev)                        
    dose1 <- cbind(rep(1,num), log(doses[lev]))
    dauc1 <- D_AUC[lev]
    beta0mean <- betapriors[1]
    beta1mean <- betapriors[2]
    
    # For STAN model
    data_s <- list(N=num, y=y, dose=dose1, dauc = dauc1, beta0mean=beta0mean, beta1mean=beta1mean)
    reg1 <- sampling(sm_lrCov, data=data_s, iter=options$niter, chains=options$nchains,
                     control = list(adapt_delta = options$nadapt))
    
    a1 = get_posterior_mean(reg1)
    
    Beta <- a1[4:6,options$nchains+1]
    
    # 5th column is the mean of all chains(we have 4 chains so its 5th column)
    
    ############################################
    ######## Computation probability ###########
    ############################################
    
    p_stim = 1 / (1 + exp(Beta[1] + Beta[2]*log(doses)))
    
    new_dose = order((abs(p_stim - theta)))[1]
    
    list(new_dose=new_dose, pstim = p_stim, parameters = Beta, posterior = a1)
}