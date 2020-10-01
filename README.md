# concreg

## Overview

performs concordance regression, a semiparametric regression method to estimate generalized odds of concordance as proposed by Dunkler, Schemper and Heinze (2010) <doi:10.1093/bioinformatics/btq035>.

## Details

If Cox's proportional hazards regression model is used in the presence of non-proportional hazards, i.e., with underlying time-dependent hazard ratios of prognostic factors, the average relative risk for such a factor is under- or overestimated and testing power for the corresponding regression parameter is reduced or type-1 error inflated. In such a situation concordance regression provides an alternative, as it summarizes a time-dependent effect into a scalar estimate that can be interpreted as log odds of concordance.

Concordance regression is conditional logistic regression on all pairs of observations. In each pair, the subject that dies earlier is assumed to be a case, and the other subject the control. Pairs with equal survival time or covariate vector are uninformative. Pairs where the shorter time is censored are also not used. To correct for the loss of information due to censoring, a weighting scheme is used that upweights eligible pairs by inverse probability of censoring, and at the same time restores the number of pairs at each failure time that would be expected if there was no censoring.

Inference is based on a robust covariance matrix similar to that of Lin and Wei (1989) proposed for the Cox model. Competing risks can be accommodated by an additional weighting of subjects who experience a competing risk. These subjects remain in the risk sets, but their weights in the analysis resemble their probability to be still under follow-up (following Fine and Gray, 1999).

Usage of concordance regression is not restricted to survival data; it can also be used as a nonparametric alternative (on the outcome side) with any type of outcome variable if distributional assumptions are in doubt.

Moreover, with a special option a 'nonparametric mode' can be invoked which allows to estimate concordance statistics.

Variance estimation is based on a robust sandwich covariance matrix.

## Installation
```r
# Install concreg from CRAN
install.packages("concreg")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("georgheinze/concreg")
```

## Usage

see the manual for usage examples.
