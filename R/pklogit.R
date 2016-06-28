#' @import rstan
#' @import parallel
#' @import methods
#' @import stats 
#' @import arm
#' @export
pklogit <-
function(y, auc, doses, lev, theta, p_0, L, betapriors,D_AUC, options){
    
    num <- length(lev)                       
    dose1 <- cbind(rep(1,num), log(doses[lev]))
    # for STAN
    data_s <- list(N=num, auc=log(auc), dose=dose1)
    reg1 <- sampling(sm_lrauc, data=data_s, iter=options$niter, chains=options$nchains,
                     control = list(adapt_delta = options$nadapt))
    a1 = get_posterior_mean(reg1)
    beta1 <- a1[1:2,options$nchains+1]
    nu <- a1[3,options$nchains+1]	
    
    auc1 <- cbind(rep(1,num), log(auc))
    
    # For Stan  
    data_s <- list(N=num,y=y,dose=auc1)
    reg2 <- sampling(sm_lr, data=data_s, iter=options$niter, chains=options$nchains, control = list(adapt_delta = options$nadapt))
    a2 = get_posterior_mean(reg2)
    Beta <- a2[3:4,options$nchains+1]
    
    # Computation probability
    pstim <- NULL  
    for (o in 1:length(doses)){
        parmt = c(a1[1,options$nchains+1] + a1[2,options$nchains+1]*log(doses[o]),a1[3,options$nchains+1])
        pstim <- c(pstim, integrate(f_logit,-Inf,Inf, lambda=a2[3:4,options$nchains+1], parmt=parmt)$value)
    }
    
    MTD = order(abs(pstim-theta))[1]
    
    list(new_dose=MTD, pstim = pstim, parameters = c(beta1, nu, Beta), posterior1 = a1, posterior2 = a2)
}