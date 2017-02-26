#' @import rstan
#' @useDynLib dfpk, .registration = TRUE
#' @export
nsim <- 
function(d, N, cohort, icon, theta, p_0, L, model, scenarios, betapriors, options, TR){ 
    MTD = NULL
    dose_levels = NULL
    toxicity = NULL
    AUC_s = NULL
    AUCd = NULL 

    for (tr in 1:TR){
        
        ndos <- length(d)
        tox <- scenarios@tox          
        stab <- scenarios@tab 
        n_pk <- scenarios@n_pk        
        doses <- scenarios@doses
        d <- scenarios@doses
        x <- rep(1,cohort)
        y <- tox[cbind(1:length(x),x)]  
        M = N/cohort
        nd <- rep(0,length(d)) 
        
        for (i in 1:length(x)){ 
            eval(parse(text = paste("conc",i," <- as.vector(stab[((i-1)*ndos +x[i] +1), 2:(n_pk +1)])", sep= "")))
            eval(parse(text = paste("conc",i," <- conc",i,"[icon]", sep = ""))) 
            nd[x[i]] <- nd[x[i]] + 1
        }
        
        time <- as.vector(stab[1, 2:(n_pk +1)])
        time1 <- as.vector(stab[1, 2:(n_pk +1)])
        time1 <- time1[icon]
        
        AUCs <- NULL 
        for (i in 1:length(x)){
            eval(parse(text = paste("AUCs <- c(AUCs, AUC_estim(conc=conc",i,", t=time1, dose=d[x[",i,"]]))", sep="")))
        }
        
        pstim_auctox = matrix(0, length(doses)*cohort)
        
        AUCpop <- rep(0, length(d))
        for(s in which(nd!=0)){
            AUCpop[s] = mean(AUCs[which(x==s)])
        } 
        
        D_AUC <- (log(AUCs) - log(AUCpop[x]))
        
        stage1 = TRUE 
        for (i in 2:M) {
            j= (cohort*(i-1) + 1) : (cohort*i)   # position
            ### starting dose until toxicity
            if (stage1) {
                x <- c(x,rep(min((max(x)+1),length(d)), cohort))             
                y <- c(y, tox[cbind(j,x[j])])
                for (k in j) {
                    conci <- as.vector(stab[((k-1)*ndos + x[k] +1), 2:(n_pk +1)])
                    conci <- conci[icon]
                    AUCs <- c(AUCs, AUC_estim(conc=conci, t=time1, dose=d[x[k]]))
                    pstim_auctox = cbind(pstim_auctox, rep(0,length(doses)))
                    nd[x[k]] <- nd[x[k]] + 1 
                }
                
                for(s in which(nd!=0)){
                    AUCpop[s] = mean(AUCs[which(x==s)])
                }
                D_AUC <- (log(AUCs) - log(AUCpop[x]))
                
                if (any(y==TRUE)) {stage1 <- FALSE}
            } else { 
                
                results <- model(y,AUCs,d,x,theta,p_0,L,betapriors,D_AUC,options)
                newdose <- min(results$new_dose, max(x) + 1)        
                # Check on the skippimg dose
                x <- c(x,rep(newdose,cohort))
                y <- c(y, tox[cbind(j,x[j])])       
                for (k in j) {
                    conci <- as.vector(stab[((k-1)*ndos +x[k] +1), 2:(n_pk +1)])
                    conci <- conci[icon]
                    AUCs <- c(AUCs, AUC_estim(conc=conci, t=time1, dose=d[x[k]]))
                    nd[x[k]] <- nd[x[k]] + 1
                    pstim_auctox = cbind(pstim_auctox, results$pstim)
                }  
                
                for(s in which(nd!=0)){
                    AUCpop[s] = mean(AUCs[which(x==s)]) 
                }
                D_AUC <- (log(AUCs) - log(AUCpop[x]))
            }  
        }
        
        MtD = model(y,AUCs,d,x,theta,p_0,L,betapriors,D_AUC,options)$new_dose
        MTD = c(MTD, MtD) 
        dose_levels = rbind(dose_levels,x)
        toxicity = rbind(toxicity, y)
        AUC_s = rbind(AUC_s, AUCs)
        AUCd = rbind(AUCd, D_AUC)
        eval(parse(text = paste("pstim"," <- pstim_auctox", sep="")))
        nchains=options$nchains
        niter = options$niter
        nadapt = options$nadapt
        pid = c(1:N)
    }
    
    # cat("\n")
    # cat("\n")
    # cat("Dose-Finding results:", "\n")
    # cat("\n")
    # cat("Next recommended dose level:", MTD,"\n")
    # cat("Recommendation is based on a target toxicity probability of",theta,"\n")
    # cat("and dose levels:", d, "\n")
    # cat("\n")
    # cat("NOTE: Print results to see more details about your outcomes")
    # list(new_dose = MtD, pstim = results$pstim, dose_levels = dose_levels,toxicity = toxicity, AUCs = AUC_s, AUCd = AUCd, parameters=results$parameters)

    new("dosefinding", pid=pid, N=N, time = time, dose=d, conc=conci, p_0 = p_0,
         L=L,  nchains=options$nchains, niter=options$niter, nadapt=options$nadapt, new_dose=MtD, 
         theta=theta, dose_levels=dose_levels, toxicity=toxicity, AUCs=AUC_s, TR=TR)
}
