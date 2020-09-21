concreg <- function                                                 
(
 formula,         # formula
 data,                     #
 id=NULL,                               # identifier: numeric or character or factor
 normalize=TRUE,
 scale.weights=1,
 offset=NULL,
 alpha=0.05,                            # confidence limit
 maxit=50,                              # max. iterations
 maxhs=5,                               # half steps
 epsilon=1e-6,                          #
 maxstep=2.5,                           #
 x=TRUE,                                # for output
 y=TRUE,                                # for output
 print=TRUE,                            # print fitting information on screen
 c.risk=NULL,                           # Vector encoding competing risk: 0=censored, 1=event, 2=competing risk
 strata.var=NULL,                       # strata variable name
 trunc.weights=1,                       # quantile for weight truncation: all weights greater than that quantile will be truncated to that value
 npar=FALSE,                            # nonparametric estimation?
 ...
 ){
### by MP und GH, 2008
  
  call <- match.call()
  mf <- match.call(expand.dots =FALSE)
  m <- match(c("formula","data"), names(mf), 0L)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  
 alpha.fp=c(0.20, 0.05)
 fp.iter=10                            # maximum number of iterations of large <fp> loop
 n <- nrow(data)

  ## generate or reorder id's such that values are within 1:n
  if (is.null(id)) { id <- 1:n } else id <- as.numeric(as.factor(id))
  maxid <- max(id)
  
  ## here only ONCE the full model matrix is spanned with all possible fp-effects
  obj.full <- decomposeSurv(formula, data, sort=FALSE, offset)

  # change Daniela Competing Risk
  if (is.null(c.risk)) {
    crisk <- obj.full$resp[,3]
    obj.full$resp <- cbind(obj.full$resp, crisk)

  }

  if (is.null(c.risk)==FALSE) {
    crisk <- obj.full$resp[,3]
    c.risk[c.risk==1] <- 2
    crisk[crisk==0] <- c.risk[crisk==0]
    obj.full$resp <- cbind(obj.full$resp, crisk)
  }

  ## stratagruppen bilden
  if (is.null(strata.var)[1]) {
    obj.full$stratum <- rep(1, n)
  }

  if (is.null(strata.var)[1] == FALSE) {
    mult.strata <- 1
    if (length(strata.var) > 1)  {
      for (l in 1:length(strata.var)) { mult.strata <- c(mult.strata, mult.strata[length(mult.strata)]*10) }
    }

    daten.tmp <- data.frame(data[,strata.var])
    for (l in 1:length(strata.var)) {
      anz.strata <- length(table(data[,strata.var[l]]))

      for (l1 in 1:anz.strata)  {                                               # Strata neu durchnummerieren
        daten.tmp[data[,strata.var[l]] == as.numeric(names(table(data[,strata.var[l]])))[l1], l] <- l1
      }

     daten.tmp[, l] <- daten.tmp[, l]*mult.strata[l]
    }

    if (length(strata.var)==1)  { daten.tmp$tmp <- daten.tmp  }
    if (length(strata.var)> 1)  { daten.tmp$tmp <- apply(daten.tmp[, strata.var], 1, sum) }
    tab <- table(daten.tmp$tmp)

#    if (sum(tab==1)!=0) { stop("number of observations is in at least 1 stratum 1")}     #! ev. einfach diese Daten weglassen
    anz.strata <- length(tab)
    for (l2 in 1:anz.strata)  {
      daten.tmp$stratum[daten.tmp$tmp == as.numeric(names(tab)[l2])] <- l2
    }
    obj.full$stratum <- daten.tmp$stratum
  }

  max.strata <- max(obj.full$stratum)

  # change Daniela Competing Risk Weights (getrennt nach Strata)
  # geht nicht so einfach, Problem mit ties (gleiche Zeiten nur 1 mal in der Liste)
  if(!is.null(c.risk))   #### Georg 110517
  {
   vgl <- G <- data.frame(cbind(obj.full$resp[,2], obj.full$stratum, -99))
   G <- unique(G)
   for (i.strata in 1: max.strata) {
    g.tmp <- rep(NA, times=sum(obj.full$stratum==i.strata))
    g.tmp[obj.full$resp[obj.full$stratum==i.strata,3]==0] <- 1
    g.tmp[obj.full$resp[obj.full$stratum==i.strata,3]==1 | obj.full$resp[obj.full$stratum==i.strata,3]==2] <- 0

    G[G$X2==i.strata,3] <- survfit(Surv(obj.full$resp[obj.full$stratum==i.strata,2], g.tmp)~1)$surv
  }

  if (nrow(G) != nrow(vgl))  {                        # Bei Bindungen innerhalb der Strata
    G.n <- 1
    for (i.n in 1:n)  {
      if (sum(vgl[i.n, 1:2] == G[G.n, 1:2])==2) {
        vgl[i.n, 3] <- G[G.n,3]
        G.n <- G.n+1
      }
    }

    vgl[order(vgl[,3])[1:sum(vgl[,3] == -99)],3] <- vgl[order(vgl[,3])[1:sum(vgl[,3] == -99)]-1,3]
    G <- vgl
  }
  G <- G[,3]

  } else G<-rep(1,nrow(obj.full$resp))     #### Georg 110517
  # new time variable for competing risk
  time.crisk <- obj.full$resp[,2]
  time.crisk[obj.full$resp[,"crisk"]==2] <- max(obj.full$resp[,2])+1
  obj.full$resp <- cbind(obj.full$resp, time.crisk)


  # calculate weights
  W <- concreg.wei(resp=obj.full$resp, max.strata=max.strata, stratum=obj.full$stratum, trunc.weights=trunc.weights)


        obj <- obj.full

        kk <- ncol(obj.full$mm1) # should be k2 - NTDE                          
        obj$mm1 <- obj.full$mm1[, obj$ind[1:kk], drop=FALSE]
        obj$covnames <- obj.full$covnames[obj$ind]

        obj$timeind  <- obj.full$timeind[obj$ind[-(1:kk)]]                      #! weg?
        obj$timedata <- obj.full$timedata[, obj$ind[-(1:kk)], drop=FALSE]       #! weg?
        ## re-index $timeind
        obj$timeind <- match(obj$timeind, (1:kk)[obj$ind[1:kk]])                #! weg?
        NTDE <- obj$NTDE <- length(obj$timeind)                                 #! weg?


        ind.offset <- sum(length(offset) != 0)

        k <- ncol(obj$mm1)    # number covariates w/o time-dep effects          # Anzahl Variablen
        k2 <- k + NTDE                                                          #! weg?
        if (npar) nonpar<-1
        else nonpar<-0


        PARMS <- c(n, k, 0, maxit, maxhs, maxstep, epsilon,  0, 0, 0, 0, 0, 0, max.strata, maxid, ind.offset, nonpar)


  # alles sortieren (obj, W, G, id ist sortiert)
  ord <- order(obj.full$stratum, obj.full$resp[,"time.crisk"],(1-obj.full$resp[,3]))
  obj$mm1     <- obj$mm1[ord,]                                                  
  obj$resp    <- obj$resp[ord,]
  obj$stratum <- obj$stratum[ord]
  G           <- G[ord]
  W           <- W[ord]
  id          <- id[ord]

        ##   if (offset) {
        ##    IOARRAY[1,1]<-0    # first variable is offset
        ##    IOARRAY[2,1]<-Z.sd[1]    # first variable is offset
        ##   }
        ## **************** fit model *********************
        value0 <- concreg.fit(obj=obj, id=id, W=W, G=G, PARMS=PARMS, npar=npar)            #! Aufruf der Funktion mit dem Fortran Aufruf

#        cov.ls <- value0$cov.ls
#        cov.lw <- cov.j <- dfbeta.resid <- NULL

#        if(robust) {
#                cov.lw <- matrix(value0$outtab[(k2+4):(3+2*k2), ], ncol=k2) / value0$ZxZ
#                dfbeta.resid <- value0$dfbetaresid / matrix(value0$Z.sd, maxid, k2, byrow=TRUE)
#                covs <- cov.lw
#                cov.method <- "Lin-Wei"
#
#        } else if(jack) {
#                dfbeta.resid <- matrix(0, maxid, k2)
#                nc <- ncol(value0$cards)
#                for(iid in 1:maxid) {
#                        cardsJ <- value0$cards[id != iid,]
#                        cardsJ[id[id!=iid]>iid,nc] <- cardsJ[id[id!=iid]>iid,nc]-1
#                        n.jack <- nrow(cardsJ)
#                        PARMS <- c(n.jack, k, 0, maxit, maxhs,maxstep,epsilon, 0, 0, 0, 0, 0, W$NGV, NTDE, maxid-1, ind.offset)
#                        valueJ <- coxphw.fit(obj, id, W$weights, PARMS, cardsJ) # fit model jack(i) *******
#                        dfbeta.resid[iid,] <- value0$coef.orig - valueJ$coef.orig
#                }
#                cov.j <- (maxid-1) / maxid * crossprod(dfbeta.resid) / value0$ZxZ
#                dfbeta.resid <- dfbeta.resid / matrix(value0$Z.sd, maxid, k2, byrow=TRUE)
#                covs <- cov.j
#                cov.method <- "Jackknife"
#
#        } else {
#                cov.method <- "Lin-Sasieni"
#                covs <- cov.ls
#        }

        vars <- as.matrix(diag(value0$cov.rob))
#        dimnames(vars) <- list(obj$covnames, obj$covnames)

        ## DECIDE THE MODEL: USE PVALS ##############
        probs <- 1 - pchisq((value0$coefs^2/vars), 1)

        ## ########## NOW ONLY FINAL MODEL IS CONSIDERED ############
        names(value0$coefs) <- obj$covnames
        if(value0$outpar[10]>=maxit)
          cat("No convergence attained in ", value0$outpar[10], " iterations.\n", sep="")

        Means <- colMeans(value0$mmm)

        ## return object
        fit <- list(coefficients = value0$coefs,     # coefficients of the fit
                    cards    = value0$cards,         #
                    parms    = value0$outpar,
                    ioarray  = value0$outtab,
#                    dfbeta.resid = dfbeta.resid,
                    alpha    = alpha,                # significance level
                    var      = value0$cov.rob,                 # covariance matrix
                    df       = k2,                   # degrees of freedom (k + NTDE)
                    iter     = value0$outpar[10],
                    method.ties = "no",         #
                    n = n,                           # number observations
                    ##terms = terms(formula),
                    y = obj$resp,                    # responses
                    formula = formula,               # original formula
                    exit.code=value0$outpar[8],

  #                  fpind=obj$fpind,
  #                  PTcoefs=obj.full$PTcoefs,                                   #! weg?
  #                  ind=obj$ind,                                                #! was ist das?

                    call    = match.call(),
                    cov.mb  = value0$cov.mb,
#                    cov.j   = cov.j,              # jackknife covariance matrix
#                    cov.lw  = cov.lw,             #
#                    cov.ls  = cov.ls,             #
#                    cov.method=cov.method,
#                    w.matrix= W$w.matrix,         # weight matrix
#                    Wald    = value0$outpar[9],
                    Wald =  (t(value0$coefs) %*% solve(value0$cov.rob)) %*% value0$coefs,
                    means   = Means,               # means of <X> (model matrix)
                    linear.predictors= as.vector(scale(value0$mmm, Means, scale=FALSE) %*% value0$coefs),
                    method  = "Weighted Estimation",
                    method.ci= "Wald",             #
                    ci.lower= exp(value0$coefs + qnorm(alpha/2) * vars^0.5), #
                    ci.upper= exp(value0$coefs + qnorm(1 - alpha/2) * vars^0.5), #
                    prob    = probs,               # p-values
                    G       = G,
#                    W       = W,
                    W       = cbind(obj$stratum,obj$resp[,2],W)[obj$resp[,3]==1,],
                    offset.values= obj$offset.values, #
                    x       = if(x) obj$mm1 else NA,   # return original model matrix if requested
                    npar = npar
                    )

#        ## if all weights are the same ...
#        if(W$const) {
#                fit$loglik <- value0$outpar[12:11]
#                fit$score <- value0$outpar[7]
#        }

        ##        if(offset) {
        ##          obj$covnames[1]<-as.character(paste("offset(",obj$covnames[1],")"))
        ##          fit$Wald <- t(coefs[2:k2]) %*% solve(covs[2:(k2),2:(k2)]) %*% coefs[2:(k2)]
        ##          if(x) {
        ##           fit$x <- mm1.orig[,2:k2]
        ##           fit$offset <- mm1.orig[,1]
        ##          }
        ##        }
        names(fit$prob) <- names(fit$ci.upper) <- names(fit$ci.lower) <- obj$covnames
        attr(fit, "class") <- c("concreg")                 
        fit
}




