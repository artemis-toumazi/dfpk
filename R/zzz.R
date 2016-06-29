.onLoad <- function(libname, pkgname) { Rcpp::loadModule("stan_fit4logit_cov2_mod", TRUE)
										Rcpp::loadModule("stan_fit4logit_reg_dtox_mod", TRUE)
										Rcpp::loadModule("stan_fit4logit_reg_pkpop_mod", TRUE)
										Rcpp::loadModule("stan_fit4logit_reg_mod", TRUE)
										Rcpp::loadModule("stan_fit4reg_auc2_mod", TRUE) }
