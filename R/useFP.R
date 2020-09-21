useFP <- function
(
  obj,
  data
)
  ### alternative idea to get plot fp terms etc ... missing a lot ...
  ### 2008-11
{
  browser()
  obj.full <- decomposeSurv(obj$formula, data, sort=FALSE)
  ## -> $fac,resp,mm1,NTDE,timedata,timeind,NFP,fpnames,fpind,PTcoefs,covnames,ind,offset.values
  
  ## take obj.full$mm1[, obj$ind]
  
  
  CODE <- paste("(I(PT(\\1)) + powM2(PT(\\1)) + powM1(PT(\\1)) + powM0.5(PT(\\1)) + log(PT(\\1)) + ",
                "sqrt(PT(\\1)) + pow2(PT(\\1)) + pow3(PT(\\1)) + ",
                "RI(PT(\\1)) + RpowM2(PT(\\1)) + RpowM1(PT(\\1)) + RpowM0.5(PT(\\1)) + ",
                "Rlog(PT(\\1)) + Rsqrt(PT(\\1)) + Rpow2(PT(\\1)) + Rpow3(PT(\\1)) )")
  sub3 <- gsub("fp\\(([a-zA-Z0-9]*)\\)", CODE, obj$formula[3])
  formula <- as.formula(paste(as.character(obj$formula)[2], "~", sub3))
  
  ## define simple transformations
  powM2 <- function(z) z^(-2)
  powM1 <- function(z) z^(-1)
  powM0.5 <- function(z) z^(-0.5)
  pow2 <- function(z) z^2
  pow3 <- function(z) z^3
  
  ## define repeated powers
  RI <- function(z) z * log(z)
  RpowM2 <- function(z) z^(-2) * log(z)
  RpowM1 <- function(z) z^(-1) * log(z)
  RpowM0.5 <- function(z) z^(-0.5) * log(z)
  Rlog <- function(z) log(z) * log(z)
  Rsqrt <- function(z) sqrt(z) * log(z)
  Rpow2 <- function(z) z^2 * log(z)
  Rpow3 <- function(z) z^3 * log(z)
  
  ##
  browser()
  
}