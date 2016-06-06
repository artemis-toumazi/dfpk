\name{pktox}
\alias{pktox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pktox(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{auc}{
%%     ~~Describe \code{auc} here~~
}
  \item{doses}{
%%     ~~Describe \code{doses} here~~
}
  \item{lev}{
%%     ~~Describe \code{lev} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{p_0}{
%%     ~~Describe \code{p_0} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{betapriors}{
%%     ~~Describe \code{betapriors} here~~
}
  \item{D_AUC}{
%%     ~~Describe \code{D_AUC} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, 
    options) 
{
    num <- length(lev)
    dose1 <- cbind(rep(1, num), log(doses[lev]))
    data_s <- list(N = num, auc = log(auc), dose = dose1)
    reg1 <- sampling(sm_lrauc, data = data_s, iter = options$niter, 
        chains = options$nchains, control = list(adapt_delta = options$nadapt))
    a1 = get_posterior_mean(reg1)
    beta1 <- a1[1:2, 5]
    nu <- a1[3, 5]
    auc1 <- cbind(rep(1, num), log(auc))
    data_s <- list(N = num, y = y, dose = auc1)
    reg2 <- sampling(sm_lr, data = data_s, iter = options$niter, 
        chains = options$nchains, control = list(adapt_delta = options$nadapt))
    a2 = get_posterior_mean(reg2)
    Beta <- a2[3:4, 5]
    pstim <- NULL
    for (o in 1:length(doses)) {
        parmt = c(a1[1, 5] + a1[2, 5] * log(doses[o]), a1[3, 
            5])
        pstim <- c(pstim, integrate(f, -Inf, Inf, lambda = a2[3:4, 
            5], parmt = parmt)$value)
    }
    new_dose = order(abs(pstim - theta))[1]
    list(new_dose = new_dose, pstim = pstim, parameters = c(beta1, 
        nu, Beta))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
