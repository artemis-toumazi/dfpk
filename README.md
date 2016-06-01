# dfpk

The **dfpk** R package provides an interface to fit Bayesian generalized (non-)linear mixed models using Stan, which is a C++ package for obtaining Bayesian inference using the No-U-turn sampler (see http://mc-stan.org/). The formula syntax is very similar to that of the R package dfcrm to provide a simple interface for performing pharmacokinetics(PK) analyses.

## Installation

### Establish Version

**dfpk** is now on CRAN and can be installed and loaded via
```{r}
install.packages("dfpk")
library(dfpk)
```
### Development Version
To install the **dfpk** package from GitHub, first make sure that you can install the **rstan** package and C++ toolchain by following these [instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). The program Rtools (available on https://cran.r-project.org/bin/windows/Rtools/) comes with a C++ compiler for Windows. On OS-X, you should install Xcode. Once **rstan** is successfully installed, you can install **dfpk** from GitHub using the **devtools** package by executing the following in R:

```{r}
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("artemis-toumazi/dfpk")
```

If installation fails, please let us know by [filing an issue](https://github.com/artemis-toumazi/dfpk/issues).

Details on formula syntax, families and link functions, as well as prior distributions can be found on the help page of the dfpk function:
```{r help.dfpk, eval=FALSE}
help(dfpk) 
``` 

More instructions on how to use **dfpk** are given in the package's vignette.
```{r vignette, eval=FALSE}
vignette("dfpk") 
``` 
# FAQ

## Can I avoid compiling models? 

Unfortunately, fitting your model with **dfpk**, there is currently no way to avoid the compilation. 

## What is the difference between dfpk and dfcrm? 

**dfcrm** is an R package similar to **dfpk** that also allows to fit Bayesian models for the dose finding methods. **dfcrm** package uses Dose-finding by the continual reassessment method. 
Contrary to **dfcrm**, **dfpk** comes with **Stan** models, it offers more flexibility in model specification than **dfcrm**. Also, a combination of PKLIM model(a modified model shown in Patterson et al(1999) and Whitehead et al, 2001) and the CRM model is currently fitted a bit more efficiently in **dfpk**. For a detailed comparison of **dfpk** with other common R packages implementing Stan models, type `vignette("dfpk")` in R. 

## What is the best way to ask a question or propose a new feature? 

You can either open an issue on [github](https://github.com/artemis-toumazi/dfpk) or write me an email (artemis.toumazi@gmail.com).
