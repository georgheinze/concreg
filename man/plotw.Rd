\name{plotw}
\alias{plotw}
\title{Plot pair weights of concordance regression versus time}
\description{
             This function plots the pair weights used in a weighted Cox regression analysis against time.
}
\usage{
plotw(x,  rank=FALSE,  log=FALSE,  xlim=c(0,max(time)),
 ...)}
\arguments{
  \item{x}{a concreg object}
  \item{rank}{if set to TRUE, plots the weights against ranked time (default=F)}
  \item{log}{if set to TRUE, shows logarithm of weights (default=F)}
  \item{xlim}{limits for time axis, defaults to range of time variable}
  \item{...}{further arguments for plotting}
}
\details{
 The function plots the (optionally log-transformed) pair weights against (ranked) time, separately for each stratum.
    }
\value{
 no return value.
}
\author{Georg Heinze}
\seealso{concreg}
