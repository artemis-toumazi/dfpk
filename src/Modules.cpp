#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4logit_cov2_mod) {


    class_<rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> >("model_logit_cov2")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_logit_cov2_namespace::model_logit_cov2, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4logit_reg_mod) {


    class_<rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> >("model_logit_reg")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_logit_reg_namespace::model_logit_reg, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4logit_reg_dtox_mod) {


    class_<rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> >("model_logit_reg_dtox")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_logit_reg_dtox_namespace::model_logit_reg_dtox, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4logit_reg_pkpop_mod) {


    class_<rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> >("model_logit_reg_pkpop")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_logit_reg_pkpop_namespace::model_logit_reg_pkpop, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4reg_auc2_mod) {


    class_<rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> >("model_reg_auc2")

    .constructor<SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_reg_auc2_namespace::model_reg_auc2, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
