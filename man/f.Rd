\name{f}
\alias{f}
\title{
A link function 
}
\description{
A link function f that PKTOX model uses for estimating the posterior probability of toxicity. 
}
\usage{
f(v, lambda, parmt)
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
\code{\link{pktox}}
}

\examples{
a <- -4.1752209   
b <- 1.3965865   
c <- 0.7411174 
d <- -13.4974106   
e <- 5.9645933 
doses <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)

pstim <- NULL
    for (o in 1:length(doses)){
        parmt = c(a + b*log(doses[o]),c)
        pstim <- c(pstim, integrate(f,-Inf, Inf, lambda=c(d,e), parmt=parmt)$value)
	}
pstim 
}