#' @import arm
#' @importFrom arm invlogit
#' @export
f_logit <-
function(v,lambda,parmt){
    invlogit(lambda[1]+lambda[2]*v)*dnorm(v,parmt[1],sqrt(parmt[2]))
}
