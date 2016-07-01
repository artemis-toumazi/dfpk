\name{AUC_estim}
\alias{AUC_estim}
\title{
Estimation of the area under the curve, AUC.
}

\description{
In the field of pharmacokinetics, the area under the curve (AUC) is the area under the curve (mathematically known as definite integral) in a plot of concentration of drug in blood plasma against time. AUC is computed as the doses over the second pk parameter, clearance(CL). 
}

\usage{
AUC_estim(t, conc, dose)
}

\arguments{
  \item{t}{
the time.
}
  \item{conc}{
the concentration of the drug in blood plasma.
}
  \item{dose}{
the doses' levels of the drug.
}
}

\references{
Moreno et al, 2016
}

\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr}
        }

\seealso{
\code{\link{pk.estim}}, \code{\link{sim}}
}

\examples{

dose = c(12.59972,34.65492,44.69007,60.80685,83.68946,100.37111)
conc = c(20,30,40,50,60,70)
t = seq(0,24,length.out=48)
auc_estimation <- AUC_estim(t,conc,dose)	
auc_estimation
}