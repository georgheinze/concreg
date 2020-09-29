\name{concreg}
\alias{concreg}
\title{Concordance regression for survival data}
\description{
             This package implements concordance regression for survival and other outcome data types, where each summand of the log likelihood consists
             of a pair of observations. The parameter estimates are estimated log odds of concordance and straightforwardly translate into partial concordance indices.
}
\usage{
concreg(formula, 
 data,                    
 id=NULL,                              
 normalize=TRUE,
 scale.weights=1,
 offset=NULL,
 alpha=0.05,                           
 maxit=50,                             
 maxhs=5,                              
 epsilon=1e-6,                         
 maxstep=2.5,                          
 x=TRUE,                               
 y=TRUE,                               
 print=TRUE,                           
 c.risk=NULL,                          
 strata.var=NULL,                      
 trunc.weights=1,
 npar=FALSE,                      
 ...)
}
\arguments{
  \item{formula}{a formula object, with the response on the left of the  operator, and the
    model terms on the right. The response can be a survival object as returned by the 'Surv' function, or a single variable.
    }
  \item{data}{a data.frame in which to interpret the variables named in the 'formula' argument. }
  \item{normalize}{if T, weights are normalized such that their sum is equal to the number of events. May speed up or enable convergence if for some variables no weighting is used.}
  \item{alpha}{the significance level (1-\eqn{\alpha} = the confidence level), 0.05 as default.}
  \item{maxit}{maximum number of iterations (default value is 50)}
  \item{maxhs}{maximum number of step-halvings per iterations (default value is 5). 
     The increments of the parameter vector in one Newton-Rhaphson iteration step are halved, 
     unless the new likelihood is greater than the old one, maximally doing \code{maxhs} halvings.}
  \item{epsilon}{specifies the maximum allowed change in penalized log likelihood to
    declare convergence. Default value is 0.0001.}
  \item{maxstep}{specifies the maximum change of (standardized) parameter values allowed
    in one iteration. Default value is 2.5.}
  \item{id}{a vector of patient identification numbers, must be integers starting from 1. These IDs are used for computing the 
    robust covariance matrix. If id=NA (the default) the program assumes that each line of the data set refers to a distinct individual.}
   \item{offset}{specifies a variable which is included in the model but its parameter estimate is fixed at 1.}
   \item{scale.weights}{specifies a scaling factor (a multiplicative constant) for the weights}
   \item{x}{includes covariates in output object}
   \item{y}{includes outcome in output object}
   \item{print}{prints fitting information on the screen}
   \item{c.risk}{competing risk indicator: 0 for end-of-follow-up, 1 for event of interest, 2 for competing event. status variable in formula must
   be 0 for censored (by end-of-follow-up or competing event), 1 for event}
   \item{strata.var}{variable for defining strata in stratified analysis}
   \item{trunc.weights}{quantile at which weights are truncated. set to 1 for no weight truncation.}
   \item{npar}{estimation of nonparametric log odds of concordance?}
   \item{...}{further arguments}
   
}
\details{
       If Cox's proportional hazards regression model is used in the presence of non-proportional hazards, 
i.e., with underlying time-dependent hazard ratios of prognostic factors, the average relative risk 
for such a factor is under- or overestimated and testing power for the corresponding regression parameter 
is reduced or type-1 error inflated. In such a situation concordance regression provides an alternative, as
it summarizes a time-dependent effect into a scalar estimate that can be interpreted as log odds of concordance.

Concordance regression is conditional logistic regression on all pairs of observations. In each pair, the subject
that dies earlier is assumed to be a case, and the other subject the control. Pairs with equal survival time or covariate vector
are uninformative. Pairs where the shorter time is censored are also not used. To correct for the loss of information due to
censoring, a weighting scheme is used that upweights eligible pairs by inverse probability of censoring, and at the same time
restores the number of pairs at each failure time that would be expected if there was no censoring.

Inference is based on a robust covariance matrix similar to that of Lin and Wei (1989) proposed for the Cox model. Competing
risks can be accommodated by an additional weighting of subjects who experience a competing risk. These subjects remain in the risk
sets, but their weights in the analysis resemble their probability to be still under follow-up (following Fine and Gray, 1999).
      }
\value{
 \item{coefficients}{the parameter estimates}
 \item{alpha}{the significance level = 1 - confidence level}
 \item{var}{the estimated robust covariance matrix}
 \item{cov.mb}{the model-based covariance matrix}
 \item{iter}{the number of iterations needed to converge}
 \item{n}{the number of observations}
 \item{y}{the response}
 \item{x}{the covariates}
 \item{formula}{the model formula}
 \item{means}{the means of the covariates}
 \item{linear.predictors}{the linear predictors}
 \item{Wald}{the global Wald statistic}
 \item{df}{the degrees of freedom}
 \item{ci.lower}{the lower confidence limits}
 \item{ci.upper}{the upper confidence limits}
 \item{prob}{the p-values}
 \item{call}{the function call}
 \item{W}{A matrix with 3 columns and rows according to the number of uncensored failure times. The first column contains the 
 stratum numbers, the second column the failure times, the third column the weight for each pair at each failure time.}
 \item{G}{A vector containing the  probability of censoring for each observation.}
}
\references{
Dunkler D, Schemper M and Heinze G (2010). Gene selection in microarray survival studies under possibly
non-proportional hazards. \emph{Bioinformatics} 26, 784-790.

Fine JP and Gray RJ (1999). A Proportional Hazards Model for the Subdistribution of a Competing Risk. \emph{Journal of the
American Statistical Association} 94, 496-509.

Lin D and Wei L (1989). The robust inference for the Cox proportional hazards model. \emph{Journal of the American Statistical Association} 84, 1074-1078.

}
\author{Georg Heinze, Daniela Dunkler and Meinhard Ploner}
\seealso{coxph}
\examples{
# gastric cancer data set
gastric <-
  structure(list(patnr = as.integer(c(46, 1, 2, 3, 4, 5, 47, 6,
                   7, 8, 9, 48, 10, 11, 49, 12, 13, 14, 50, 15, 16, 17, 18, 19,
                   20, 51, 21, 22, 52, 23, 53, 54, 55, 24, 25, 56, 57, 58, 59, 60,
                   61, 62, 63, 64, 26, 65, 27, 66, 28, 29, 67, 68, 69, 70, 30, 71,
                   31, 72, 32, 73, 33, 34, 74, 75, 76, 77, 78, 35, 79, 36, 80, 81,
                   82, 37, 38, 39, 83, 84, 40, 85, 41, 86, 87, 88, 42, 43, 44, 89,
                   90, 45)),
                 treat = as.integer(c(0, 1, 1, 1, 1, 1, 0, 1, 1, 1,
                   1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0,
                   0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0,
                   0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
                   1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1)),
                 time = as.integer(c(1,
                   17, 42, 44, 48, 60, 63, 72, 74, 95, 103, 105, 108, 122, 125,
                   144, 167, 170, 182, 183, 185, 193, 195, 197, 208, 216, 234, 235,
                   250, 254, 262, 301, 301, 307, 315, 342, 354, 356, 358, 380, 383,
                   383, 388, 394, 401, 408, 445, 460, 464, 484, 489, 499, 523, 524,
                   528, 535, 542, 562, 567, 569, 577, 580, 675, 676, 748, 778, 786,
                   795, 797, 855, 955, 968, 977, 1174, 1214, 1232, 1245, 1271, 1366,
                   1420, 1455, 1460, 1516, 1551, 1585, 1622, 1626, 1690, 1694, 1736
                   )),
                 status = as.integer(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
                   0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0))),
            .Names = c("patnr",
              "treat", "time", "status"), class = "data.frame",
            row.names = c("1",
              "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
              "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
              "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
              "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46",
              "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57",
              "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68",
              "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
              "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90"
              ))
  
library(survival)
fit<-concreg(data=gastric, Surv(time,status)~treat)
summary(fit)

plotw(fit)

fit2<-concreg(data=gastric, Surv(time,status)~treat, trunc.weights=0.95)
summary(fit2)

# nonparametric c-index
y<-rnorm(200)
x<-rnorm(200)
dat<-data.frame(x=x, y=y)
fit3<-concreg(data=dat, y~x, npar=TRUE)
cindex(fit3)
confint(fit3, what="cindex")

# Mann-Whitney-U (AUROC) statistic
y<-rbinom(200, 1, 1-pnorm(x))
dat<-data.frame(x=x, y=y)
fit4<-concreg(data=dat, y~x, npar=TRUE)
cindex(fit4)
confint(fit4, what="cindex")
 # symmetry of the univariate nonparametric model:
fit5<-concreg(data=dat, x~y, npar=TRUE)
cindex(fit5)
confint(fit5, what="cindex")


}
\keyword{survival}
\keyword{regression}
\keyword{models}

