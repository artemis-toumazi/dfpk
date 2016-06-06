\name{pk.estim}
\alias{pk.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pk.estim(par, t, dose, conc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
%%     ~~Describe \code{par} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{dose}{
%%     ~~Describe \code{dose} here~~
}
  \item{conc}{
%%     ~~Describe \code{conc} here~~
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
function (par, t, dose, conc) 
{
    ka <- par[1]
    CL <- par[2]
    V <- par[3]
    s <- dose * ka/V/(ka - CL/V) * (exp(-CL/V * t) - exp(-ka * 
        t))
    if (any(is.nan(s))) 
        cat("ka=", ka, "\n V=", V, "\n CL=", CL, "\n")
    s[s == 0] = rep(2^(-1074), length(s[s == 0]))
    sum((log(s) - log(conc))^2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
