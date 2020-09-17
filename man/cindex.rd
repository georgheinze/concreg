\name{cindex}
\alias{cindex}
\title{
Method to extract the estimated generalized c-index from  a \code{concreg} object
}
\description{
 This method returns the estimated generalized c-indices by transforming the regression coefficients from a \code{concreg} object. }
\usage{cindex(object, confint=FALSE, level=0.95)}
\arguments{
  \item{object}{ 
  a \code{concreg} object
%%     ~~Describe \code{obj} here~~
}
\item{confint}{
  if TRUE, request confidence interval(s) for c-indices.
}
\item{level}{if \code{confint==TRUE}, specifies confidence level.} 
}
\details{
In case of a regression analysis with one explanatory variable, and with \code{npar=TRUE}, this returns the classical nonparametric c-index: P(Y_i<Y_j|X_i>X_j),
where i and j are all possible pairs of individuals.

In case of a multivariable regression, it will return the partial c-index for each variable X, conditional on the covariates Z: P(Y_i<Y_j|X_i>X_j & Z_i==Z_j).

In case that \code{npar=FALSE}, it will return the generalized (partial) concordance index as defined by Dunkler et al, Bioinformatics 2010: P(Y_i<Y_j|X_i-X_j==1).
}
\value{
 A pxp covariance matrix, where p is the number of regression coefficients. 
}
\references{
 Dunkler D, Schemper M and Heinze G (2010). Gene selection in microarray survival studies under possibly
 non-proportional hazards. \emph{Bioinformatics} 26, 784-790.
 }
\author{
Georg Heinze
}
\seealso{
\code{coef.concreg}
\code{confint.concreg}
}
\examples{
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
  

fit<-concreg(data=gastric, Surv(time,status)~treat, npar=TRUE)
cindex(fit, confint=TRUE) ### nonparametric c-index with 95% confidence interval
 }
\keyword{survival}
\keyword{regression}
\keyword{models}
