concreg.wei <- function(resp, max.strata, stratum, trunc.weights)
{
  #! function to calculate the weigth for concreg
  #! rechnet mit time.crisk
  
  data.tmp <- data.frame(resp[,c(5,3,2,4)])
  dimnames(data.tmp)[[2]][2] <- "status"
  dimnames(data.tmp)[[2]][3] <- "time.orig"
  dimnames(data.tmp)[[2]][4] <- "status012"
  data.tmp$statusfu<-(data.tmp$status==0)+0
  
  W <- rep(0, nrow(data.tmp))
  if(all(data.tmp[,2]==1)) return(rep(1, nrow(data.tmp)))
  
  for (i.max.strata in 1:max.strata)  {
    data.strata <- data.tmp[stratum==i.max.strata,]
    
    n.strata <- nrow(data.strata)
    data.strata$pat <- 1:n.strata
    
    #    attach(data.strata, warn.conflicts=FALSE)         # survfit geht sonst nicht aus irgendeinem Grund
    fit <- survfit(Surv(time.crisk, status)~1, data=data.strata)           #pseudo survival to reconstruct number of pairs
    fit.fu<-survfit(Surv(time.orig, statusfu)~1, data=data.strata)         #follow-up KM
    #    detach(data.strata)
    fsurv<-fit$surv
    
    #   if (length(fsurv) == n.strata)  {                        # keine Bindungen
    #      fsurv <- c(1, fit$surv)[-(length(fit$surv)+1)]             
    #     data.strata$surv <- fsurv
    #      data.strata$n.risk <- fit$n.risk
    #    }
    
    #   if (length(fsurv) != n.strata)  {                        # Bei Bindungen innerhalb der Strata
    data.strata$surv <- -99
    data.strata$n.risk <- -99
    data.strata$gsurv <- -99
    
    fit.n <- 1
    fsurv<-c(1,fit$surv)
    fn.risk<-c(n.strata, fit$n.risk)
    gsurv<-c(1,fit.fu$surv)
    gtime<-c(0,fit.fu$time)
    ftime<-c(0,fit$time)
    for (i.n in 1:n.strata)  {
      if (data.strata$status[i.n]==1)  indices<-sum(ftime<data.strata[i.n, "time.crisk"])
      if (data.strata$status[i.n]==0)  indices<-sum(ftime<=data.strata[i.n, "time.crisk"])
      index.fu<-sum(gtime<data.strata[i.n, "time.crisk"])
      data.strata[i.n, c("surv", "n.risk", "gsurv")] <- c((fsurv[indices]), (fn.risk[indices]), gsurv[index.fu])
      #        if (data.strata[i.n, "time.crisk"] == fit$time[fit.n]) {
      #          fit.n <- fit.n+1
    }
    #      }
    
    #    data.strata[data.strata[order(data.strata[,"surv"])[1:sum(data.strata[,"surv"] == -99)],3], c("surv", "n.risk")] <-
    #    data.strata[data.strata[order(data.strata[,"surv"])[1:sum(data.strata[,"surv"] == -99)]-1,3], c("surv", "n.risk")]
    
    
    #    W <- c(W, (n.strata*data.strata[,"surv"] / (data.strata[,"n.risk"]-1))* (data.strata[,"surv"] / (data.strata[,"n.risk"])))
    #    W <- c(W, ((n.strata*data.strata[,"surv"]-1) / (data.strata[,"n.risk"]-1))* (n.strata*data.strata[,"surv"] / (data.strata[,"n.risk"])))
    W.strata <- ((n.strata*data.strata[,"surv"]-1) / (data.strata[,"n.risk"]-1))/ data.strata[,"gsurv"]
    W[stratum==i.max.strata] <- W.strata
    
  }
  #  W <- W[-1]
  W[W==Inf] <- 1
  W[is.na(W)] <- 1
  truncat<-quantile(W, probs=trunc.weights)
  W[W>truncat]<-truncat
  W[abs(W)==Inf]<-0
  W
}