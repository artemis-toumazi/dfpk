\name{pkcov}
\alias{pkcov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pkcov(y, auc, doses, lev, theta, p_0, L, betapriors, D_AUC, options)
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
    dauc1 <- D_AUC[lev]
    beta0mean <- betapriors[1]
    beta1mean <- betapriors[2]
    data_s <- list(N = num, y = y, dose = dose1, dauc = dauc1, 
        beta0mean = beta0mean, beta1mean = beta1mean)
    reg1 <- sampling(sm_lrCov, data = data_s, iter = options$niter, 
        chains = options$nchains, control = list(adapt_delta = options$nadapt))
    a1 = get_posterior_mean(reg1)
    Beta <- a1[4:6, 5]
    p_stim = 1/(1 + exp(Beta[1] + Beta[2] * log(doses)))
    new_dose = order((abs(p_stim - theta)))[1]
    list(new_dose = new_dose, pstim = p_stim, parameters = Beta, 
        posterior = a1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
