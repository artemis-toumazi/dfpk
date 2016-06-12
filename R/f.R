#' @export
f <-
function(v,lambda,parmt){
    pnorm(lambda[1]+lambda[2]*v)*dnorm(v,parmt[1],parmt[2])
}
