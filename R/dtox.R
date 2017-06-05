#' @import ggplot2
#' @import rstan
#' @import Rcpp
#' @import methods
#' @import stats
#' @useDynLib dfpk, .registration = TRUE
#' @export
dtox <-
function(y, doses, x, theta, prob = 0.9, options=list(nchains = 4, niter = 4000, nadapt = 0.8), betapriors = c(6.71, 1.43), thetaL=NULL, auc = NULL, deltaAUC = NULL, p0 = NULL, L = NULL){
        
        checking1 <- function(x,target,error){
            sum(x>(target+error))/length(x)              ## how many x are greater than (target+error) / length(x) =  the probability
        }
        
        num <- length(x)  	# how many patients
        dose1 <- cbind(rep(1,num), log(doses[x]))
        # For STAN model
        
        data_s <- list(N=num, y=y, dose=dose1, beta0mean=betapriors[1], beta1mean=betapriors[2])
        sm_lrDtox <- stanmodels$cdf_reg_dtox
        reg1 <- sampling(sm_lrDtox, data=data_s, iter=options$niter, chains=options$nchains, control = list(adapt_delta = options$nadapt))
        a1 = get_posterior_mean(reg1)
        sampl1 <- extract(reg1)
        
        beta <- a1[3:4, options$nchains + 1]
        
        beta0 <- beta[1]
        beta1 <- beta[2]
        pnew <- pnorm(beta0 + beta1*log(doses))   
        
        Beta0 <- sampl1$bet1[,1]
        Beta1 <- sampl1$bet1[,2]
        pstim_sum <- matrix(0, ncol = options$nchains*options$niter/2, nrow = length(doses))
        p_sum <- NULL 
        for(o in 1:length(doses)){
            for(i in 1:ncol(pstim_sum)){
                pstim_sum[o,i] <- pnorm(Beta0[i] + Beta1[i]*log(doses[o])) 
            }
            p_sum <- rbind(p_sum, summary(pstim_sum[o,]))
        }
        
        
        pstop <-  checking1(pnew, target=theta, error=0)
        stoptox <- (pstop >= prob)
        stoptrial <- stoptox
        
        # check if we will stop the trial or not
        
        if (stoptrial){
            newDose = NA
            message("The trial stopped based on the stopping rule \n \n")
        }else{                                      # if we don't stopped
            newDose <- order(abs(pnew-theta))[1]
        }
        
        parameters <- beta 
        names(parameters) <- c("beta0", "beta1")
        list(newDose = newDose, pstim = pnew, p_sum = p_sum, parameters = parameters)
    }
