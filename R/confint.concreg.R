confint.concreg<-function(object, parm, level=0.95, what="coefficients", ...){   
  if(what != "coefficients" & what !="OC" & what !="cindex") {
    what<-"coefficients"
    warning("Will produce confidence intervals for regression coefficients.\n")
  }
  if(missing(parm)) parm<-1:length(coef(object))
  se<-sqrt(diag(vcov(object)))
  ci.coef<-(coef(object)+se*cbind(qnorm((1-level)/2), qnorm(1-(1-level)/2))  )[parm,]
  if(what=="coefficients") return(ci.coef)
  else if (what=="OC") return (exp(ci.coef))
  else if (what=="cindex") return (exp(ci.coef)/(1+exp(ci.coef)))
}


"plotw" <- function                                                             #! noch anpassen
(
  x,    # object of class coxphw
  rank=FALSE,
  log=FALSE,
  xlim=c(0,max(time)),
  
  ...            # dummy
)
{
  if(rank) {
    time<-order(x$W[,2])
    label<-"Ranked time"
  }
  else {
    time<-x$W[,2]
    label<-"Time"
  }
  if(log) {
    weights<-log(x$W[,3])
    wlabel<-"Log of weight"
  }
  else {
    weights<-x$W[,3]
    wlabel<-"Weight"
  }
  #   if (is.na(xlim)) xlim=c(0,max(time))
  ltyi<-1
  for (i in unique(x$W[,1])) {
    if (ltyi==1) plot(time[x$W[,1]==i], weights[x$W[,1]==i],type="l",lty=ltyi,ylim=c(min(weights[time<=xlim[2]]),max(weights[time<=xlim[2]])), xlab=label, ylab=wlabel, xlim=xlim)
    else lines(time[x$W[,1]==i], weights[x$W[,1]==i],lty=ltyi,ylim=c(min(weights[time<=xlim[2]]),max(weights[time<=xlim[2]])), xlab=label, ylab=wlabel, xlim=xlim)
    ltyi<-ltyi+1
  }
  #   lines(time, weights,lty=2)
  #   lines(time, weights,lty=3)
  legend(min(time),0.95*max(weights[time<=xlim[2]]),unique(x$W[,1]),lty=1:(ltyi-1), lwd=1)
}