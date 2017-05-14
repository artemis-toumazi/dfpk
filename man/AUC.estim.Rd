\name{AUC.estim}
\alias{AUC.estim}

\title{
Estimation of the area under the curve, AUC.
}

\description{
The AUC.estim function uses a compartmentl method or a non-compartmental method to estimate the AUC. In the field of pharmacokinetics, the area under the curve (AUC) is the area under the curve (mathematically known as definite integral) in a plot of concentration of drug in blood plasma against time. AUC is computed as the doses over the second pharmacokinetic's parameter, the clearance (CL).
}

\usage{
AUC.estim(t, conc, dose, method = 2)
}

\arguments{
  \item{t}{
A vector of the sampling time.
}
  \item{conc}{
The concentration of the drug in blood plasma.
}
  \item{dose}{
A vector of dose levels assigned to patients.
}
  \item{method}{
A string number specifying the method for calculation of AUC. Possible values are "1" for a compartmental method and "2" for non-compartmental method (default=2). 
}
}

\references{
Ursino, M., et al, (2017) Dose-finding methods for Phase I clinical trials using pharmacokinetics in small populations, Biometrical Journal. 
}

\author{Artemis Toumazi \email{artemis.toumazi@inserm.fr}, 
        Moreno Ursino \email{moreno.ursino@inserm.fr},
        Sarah Zohar \email{sarah.zohar@inserm.fr}
        }

\seealso{
\code{\link{pk.estim}}, \code{\link{nsim}}
}


\examples{
#### A Compartmental method for calculation of AUC ###

dose = c(12.59972, 34.65492, 44.69007, 60.80685, 83.68946, 100.37111)
conc = c(20, 30, 40, 50, 60, 70)
t = seq(0, 24, length.out=48)
auc_estimation <- AUC.estim(t, conc, dose, method = 1)
auc_estimation 
}