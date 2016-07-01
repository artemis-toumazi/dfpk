#' @useDynLib dfpk, .registration = TRUE
#' @export
AUC_estim <-
function(t,conc,dose){
    
    out_pk= optim(c(1,5,50), pk.estim, t=t, dose=dose, conc=conc, method = "L-BFGS-B", lower = c(0.1,0.2,1))
    dose/out_pk$par[2]
}
