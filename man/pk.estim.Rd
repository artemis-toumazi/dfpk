\name{pk.estim}
\alias{pk.estim}
\title{
PK measure of exposure
}
\description{
Estimation of the PK measure of exposure. 
}
\usage{
pk.estim(par, t, dose, conc)
}

\arguments{
  \item{par}{
PK parameters.
}
  \item{t}{
the time sampling.
}
  \item{dose}{
the doses' levels of the drug.
}
  \item{conc}{
the concentration of the drug in blood plasma.
}
}

\references{
Moreno et al, 2016
}

\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr}
        }

\seealso{
\code{\link{AUC_estim}}, \code{\link{sim}}
}

\examples{
par <- c(2,10,100) 
conc <- c(20,30,40,50,60,70)
t <- seq(0,24,length.out=48)
dose <- c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
pk.estim(par,t,dose,conc)
}
