\name{scenarios}
\alias{scenarios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scenarios(param_pk, omega2, omega_a, sigma2, preal, limit_tox, time_sampling, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param_pk}{
%%     ~~Describe \code{param_pk} here~~
}
  \item{omega2}{
%%     ~~Describe \code{omega2} here~~
}
  \item{omega_a}{
%%     ~~Describe \code{omega_a} here~~
}
  \item{sigma2}{
%%     ~~Describe \code{sigma2} here~~
}
  \item{preal}{
%%     ~~Describe \code{preal} here~~
}
  \item{limit_tox}{
%%     ~~Describe \code{limit_tox} here~~
}
  \item{time_sampling}{
%%     ~~Describe \code{time_sampling} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
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
function (param_pk, omega2, omega_a, sigma2, preal, limit_tox, 
    time_sampling, N) 
{
    pk.model <- function(t, dose, ka, CL, V) {
        dose * ka/V/(ka - CL/V) * (exp(-CL/V * t) - exp(-ka * 
            t))
    }
    n_pk <- length(time_sampling)
    doses <- exp(qnorm(preal) * sqrt(omega2^2 + omega_a^2) + 
        log(limit_tox) + log(param_pk[2]))
    parameter <- NULL
    sens_AUC <- NULL
    alphatot <- NULL
    tab <- c(0, time_sampling, time_sampling)
    npar <- length(param_pk) - 1
    for (i in 1:N) {
        ipar <- (param_pk[2:3]) * exp(rnorm(npar, sd = omega2))
        ipar <- c(param_pk[1], ipar)
        parameter <- rbind(parameter, ipar)
        for (j in doses) {
            concen <- pk.model(time_sampling, dose = j, ka = ipar[1], 
                CL = ipar[2], V = ipar[3])
            conc_pred <- concen * (1 + rnorm(2 * n_pk, sd = sigma2))
            tab <- rbind(tab, c(i, conc_pred))
            alfa <- exp(rnorm(1, sd = omega_a))
            CL <- ipar[2]
            sens <- alfa * j/CL
            sens_AUC <- c(sens_AUC, sens)
            alphatot <- c(alphatot, alfa)
        }
    }
    concentration <- NULL
    for (k in 1:length(doses)) {
        conc <- pk.model(time_sampling, dose = doses[k], ka = ipar[1], 
            CL = ipar[2], V = ipar[3])
        concentration <- cbind(concentration, conc)
    }
    tox <- matrix(sens_AUC >= limit_tox, ncol = length(doses), 
        byrow = T)
    data <- list(n_pk = n_pk, conc_pred = conc_pred, concentration = concentration, 
        doses = doses, tab = tab, tox = tox, parameters = parameter, 
        alfa_AUC = sens_AUC)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
