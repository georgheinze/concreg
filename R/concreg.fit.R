concreg.fit <- function(obj, id, W, G, PARMS, npar)                                   
{
  # fitter function
  
  #        k <- ncol(obj$mm1)    # number covariates w/o time-dep effects         #! in PARMS
  k <- PARMS[2]
  k2 <- k + obj$NTDE                                                      #! weg?
  #        maxid <- max(id)                                                       #! in PARMS
  maxid <- PARMS[15]
  
  ## standardize model matrix, but only in semiparametric mode
  if(!npar) {
    sd1 <- apply(as.matrix(obj$mm1),2,sd)
    sd2 <- apply(as.matrix(obj$timedata),2,sd)                                                 #! weg?
    Z.sd <- c(sd1, sd2 * sd1[obj$timeind])                                  #! weg?
    ZxZ <- as.matrix(Z.sd) %*% t(as.matrix(Z.sd)) # used often to restandardize ...    #! weg?
    obj$mm1 <- scale(obj$mm1, FALSE, sd1)
  } else {
    sd1<-1
    sd2 <- 1
    Z.sd<-1
    ZxZ <-1
  }
  
  
  ##if(ind.offset)
  obj$mm1o <- if(PARMS[16] != 0) cbind(obj$offset.values, obj$mm1) else obj$mm1
  
  #        if(is.null(CARDS))
  CARDS <- cbind(obj$mm1o, obj$resp[,c(2, 4)], W, G, id, obj$stratum)
  #          CARDS <- cbind(obj$mm1o, obj$resp, weights, obj$timedata, id)        #! was ist weights?
  
  if(!npar) obj$timedata <- scale(obj$timedata, FALSE, sd2)                         # weg?
  mmm <- cbind(obj$mm1, obj$timedata) # model matrix inc. time data       #! ohne timedata?
  ##   if (offset) {
  ##    IOARRAY[1,1]<-0    # first variable is offset
  ##    IOARRAY[2,1]<-Z.sd[1]    # first variable is offset
  ##   }
  DFBETA <- matrix(0, maxid, k2)                                          #! k2 durch k ersetzen?
  IOARRAY <- rbind(rep(1, k2), matrix(0, 2+2*k2, k2))                     #! k2 durch k ersetzen?
  if(obj$NTDE >0)                                                         #! weg?
    IOARRAY[4, (k+1):k2] <- obj$timeind                                   #! weg?
  
  ## --------------- Aufruf Fortran-Routine ????????? ------------
  storage.mode(CARDS) <- storage.mode(PARMS) <- storage.mode(IOARRAY) <- storage.mode(DFBETA) <- "double"
  #       dyn.load("concreg_dll.dll")
  #        dyn.load("D:\\WORK\\concreg_dll.dll")
  value <- .Fortran("CONCREG",                                            #! anpassen
                    cards=CARDS,
                    outpar = PARMS,
                    outtab = IOARRAY)
  #                          PACKAGE=concreg.fp)
  if(value$outpar[8])
    warning("Error in Fortran routine concreg; parms8 <> 0")
  
  coefs <- value$coefs / Z.sd                                             #! value$coefs gibt es nicht
  cov.mb <- matrix(value$outtab[4:(k2+3), ], ncol=k2) / ZxZ            # "model-based" varianz
  cov.rob <- matrix(value$outtab[(3+k2+1):(3+2*k2), ], ncol=k2) / ZxZ  # "robuste" varianz (standard)
  
  res <- list(                                                            #! anpassen
    cards=value$cards,
    outpar=value$outpar,
    outtab=matrix(value$outtab, nrow=3+2*k2),
    #                   dfbetaresid=value$dfbetaresid,
    coef.orig=value$outtab[3,  ],
    coefs=value$outtab[3,  ] / Z.sd, # coefficients
    cov.rob=cov.rob,                 # covariances
    cov.mb=cov.mb,
    Z.sd=Z.sd,
    ZxZ=ZxZ,                                                    #! weg?
    mmm=mmm                          # model matrix
  )
  res
}