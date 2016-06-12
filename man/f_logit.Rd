\name{f_logit}
\alias{f_logit}
\title{
Logit link function 
}
\description{
A logit function that is used in the posterior probability of toxicity of the model PKLOGIT.
}

\usage{
f_logit(v, lambda, parmt)
}
\arguments{
  \item{v}{

}
  \item{lambda}{

}
  \item{parmt}{
}
}

\references{
Moreno et al, 2016
}

\author{
Artemis Toumazi
}

\seealso{
\code{\link{pklogit}}
}

\examples{
a <- -4.1419339 
b <- 1.3873627 
c <- 0.7402598 
d <- -13.3335144
e <- 5.9044069 
doses <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)

pstim <- NULL 
for (o in 1:length(doses)){
        parmt = c(a + b*log(doses[o]),c)
        pstim <- c(pstim, integrate(f_logit,-Inf,Inf, lambda=c(d,e), parmt=parmt)$value)
	}
pstim
}
