pkcov <-
function(y, auc, doses, x, theta, deltaAUC, prob = 0.9, options = list(nchains = 4, niter = 4000, nadapt = 0.8), 
             betapriors = c(-14.76, 3.23), thetaL=NULL, p0 = NULL, L = NULL){
        
        checking1 <- function(x,target,error){
            sum(x>(target+error))/length(x)              ## how many x are greater than (target+error) / length(x) =  the probability
        }
        
        num <- length(x)
        dose1 <- cbind(rep(1,num), log(doses[x]))
        dauc1 <- deltaAUC[x]
        beta0mean <- betapriors[1]
        beta1mean <- betapriors[2]
        
        # For STAN model
        data_s <- list(N=num, y=y, dose=dose1, dauc = dauc1, beta0mean=beta0mean, beta1mean=beta1mean)
        sm_lrCov <- stanmodels$logit_reg_pkcov
        reg1 <- sampling(sm_lrCov, data=data_s, iter=options$niter, chains=options$nchains,
                         control = list(adapt_delta = options$nadapt))  
        
        a1 = get_posterior_mean(reg1)
        sampl1 <- extract(reg1)
        
        Beta <- a1[4:6, options$nchains+1]
        
        Beta0 <- sampl1$bet1[,1]
        Beta1 <- sampl1$bet1[,2]
        Beta2 <- sampl1$bet2
        
        ############################################
        ######## Computation probability ###########
        ############################################
        
        pstim = 1 / (1 + exp(Beta[1] + Beta[2]*log(doses)))
        
        pstim_sum <- matrix(0, ncol = options$nchains*options$niter/2, nrow = length(doses))
        p_sum <- NULL
        for(o in 1:length(doses)){
            for(i in 1:ncol(pstim_sum)){
                pstim_sum[o,i] <- 1 / (1 + exp(Beta0[i] + Beta1[i]*log(doses[o])))
            }
            p_sum <- rbind(p_sum, summary(pstim_sum[o,]))
        }
        
        pstop <-  checking1(pstim, target=theta, error=0)
        stoptox <- (pstop >= prob)
        stoptrial <- stoptox
        
        # check if we will stop the trial or not
        
        if (stoptrial){
            newDose = NA
            message("The trial stopped based on the stopping rule \n \n")
        }else{                              # if we not stop
            newDose <- order((abs(pstim - theta)))[1]
        }
        
        # newDose = order((abs(pstim - theta)))[1]
        parameters <- Beta
        names(parameters) <- c("beta0", "beta1", "beta2")
        list(newDose=newDose, pstim = pstim, p_sum = p_sum, parameters = parameters)
    }
