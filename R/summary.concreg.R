summary.concreg <- function                                                 #! noch anpassen
(
  object,              # object of class coxphf
  ...                  # dummy
)
  ### MP and GH
  ### 2007-07
{
  print(object$call)
  cat("\nModel fitted by", object$method, "\n\n")
  if(object$npar) cat("Nonparametric estimation \n")
  ##cat("Confidence intervals and p-values by", object$method.ci, "\n\n")
  se<-diag(object$var)^0.5
  c.index<-exp(object$coefficients)/(1+exp(object$coefficients))
  c.index.lower<-object$ci.lower/(1+object$ci.lower)
  c.index.upper<-object$ci.upper/(1+object$ci.upper)
  out <- cbind(object$coefficients, se,
               exp(object$coefficients),
               object$ci.lower, object$ci.upper,
               c.index, c.index.lower, c.index.upper, object$coefficients/se, object$prob)
  dimnames(out) <- list(names(object$coefficients),
                        c("coef", "se(coef)", "exp(coef)",
                          paste(c("lower", "upper"), 1 - object$alpha), "c", "c (lower)", "c (upper)", "z", "p"))
  if (object$method.ci != "Wald")
    dimnames(out)[[2]][6] <- "Chisq"
  
  print(out)
  
  if("loglik" %in% names(object)) {
    LL <- 2 * diff(object$loglik)
    cat("Likelihood ratio test=", LL, " on ", object$df,
        " df, p=", 1 - pchisq(LL, object$df), ", n=", object$n, "\n", sep = "")
    cat("\nScore test=", object$score, " on ", object$df,
        " df, p=", 1 - pchisq(object$score, object$df), ", n=", object$n, "\n", sep = "")
  }
  #        wald.z <- t(coef(object)) %*% solve(object$var) %*% coef(object)
  cat("Wald test =", object$Wald, "on", object$df, " df, p =",
      1 - pchisq(object$Wald, object$df))
  cat("\n\nCovariance-Matrix:\n")
  print(object$var)
  
  invisible(object)
}