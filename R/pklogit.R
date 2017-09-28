#' @import ggplot2
#' @import rstan
#' @import Rcpp
#' @import methods
#' @import stats
#' @useDynLib dfpk, .registration = TRUE
#' @export
pklogit <-
function(y, auc, doses, x, theta, prob = 0.9, options = list(nchains = 4, niter = 4000, nadapt = 0.8), 
         betapriors = c(10, 10000, 20, 10), thetaL=NULL, p0 = NULL, L = NULL, deltaAUC = NULL, CI = TRUE){
        
        checking1 <- function(x,target,error){
            sum(x>(target+error))/length(x)              ## how many x are greater than (target+error) / length(x) =  the probability
        }
        
        f_logit <- function(v,lambda,parmt){
            invlogit(-lambda[1]-lambda[2]*v)*dnorm(v,parmt[1],sqrt(parmt[2]))
        }
        
        f2_logit <- function(v, lambda1, lambda2, parmt1, parmt2){
            invlogit(-lambda1-lambda2*v)*dnorm(v,parmt1,sqrt(parmt2))
        }
        
        num <- length(x)                        # how many patients
        dose1 <- cbind(rep(1,num), log(doses[x]))
        mu1 <- seq(-log10(betapriors[1])+1, 1-1)
        
        # For STAN
        data_s <- list(N=num, auc=log(auc), dose=dose1, mu = mu1, beta0=betapriors[2])
        sm_lrauc <- stanmodels$reg_auc
        reg1 <- sampling(sm_lrauc, data=data_s, iter=options$niter, chains=options$nchains,
                         control = list(adapt_delta = options$nadapt))
        a1 = get_posterior_mean(reg1)
        sampl1 <- extract(reg1)
        
        beta1 <- a1[1:2,options$nchains+1]
        nu <- a1[3,options$nchains+1]  
        
        auc1 <- cbind(rep(1,num), log(auc))
        
        # For Stan  
        data_s <- list(N=num,y=y,dose=auc1, beta2mean = betapriors[3], beta3mean = betapriors[4])
        sm_lr <- stanmodels$logit_reg_pklogit
        reg2 <- sampling(sm_lr, data=data_s, iter=options$niter, chains=options$nchains, control = list(adapt_delta = options$nadapt))
        a2 = get_posterior_mean(reg2)
        sampl2 <- extract(reg2)
        
        Beta <- a2[3:4,options$nchains+1]
        
        # Computation probability
        pstim <- NULL  
        for (o in 1:length(doses)){
            parmt = c(a1[1,options$nchains+1] + a1[2,options$nchains+1]*log(doses[o]),a1[3,options$nchains+1])
            pstim <- c(pstim, integrate(f_logit,-Inf,Inf, lambda=a2[3:4,options$nchains+1], parmt=parmt)$value)
        }
        
        ### The posterior probabilities of toxicity
        pstim_sum <- matrix(0, ncol = options$nchains*options$niter/2, nrow = length(doses))
        p_sum <- NULL 
        if(CI == "TRUE"){
            for (o in 1:length(doses)){
                parmt1 = sampl1$b[,1] + sampl1$b[,2]*log(doses[o])
                parmt2 = sampl1$sigma
                for (i in 1:ncol(pstim_sum)){
                    pstim_sum[o,i] <- integrate(f2_logit,-Inf, Inf, lambda1=sampl2$bet1[i,1], lambda2 = sampl2$bet1[i,2], 
                                                parmt1=parmt1[i], parmt2 = parmt2[i])$value
                }
                p_sum <- rbind(p_sum, summary(pstim_sum[o,]))
            }
        }else{
            p_sum <- NULL
        }
        
        pstop <-  checking1(pstim, target=theta, error=0)
        stoptox <- (pstop >= prob)
        stoptrial <- stoptox
        
        # check if we will stop the trial or not
        
        if (stoptrial){
            newDose = NA 
            message("The trial stopped based on the stopping rule \n \n")
        }else{                                          # if we not stop
            newDose <- order((abs(pstim-theta)))[1]
        }
        
        # MTD = order(abs(pstim-theta))[1]
        parameters <- c(beta1, nu, Beta)
        names(parameters) <- c("beta0", "beta1", "nu", "beta2", "beta3")
        list(newDose=newDose, pstim = pstim, p_sum=p_sum, parameters = parameters)
    }
