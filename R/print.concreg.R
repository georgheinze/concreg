print.concreg <- function                                                   #! noch anpassen
(
  x,     # object of class coxphw
  ...            # dummy
)
  ### MP and GH
  ### overall test is score test
  ### 2007-07
{
  print(x$call)
  cat("Model fitted by", x$method, "\n\n")
  if(x$npar) cat("Nonparametric estimation \n")
  se<- diag(x$var)^0.5
  out <- cbind(x$coefficients, se, exp(x$coefficients),
               x$ci.lower, x$ci.upper, x$coefficients/se, x$prob)
  dimnames(out) <- list(names(x$coefficients),
                        c("coef", "se(coef)", "exp(coef)",
                          paste(c("lower", "upper"), 1 - x$alpha), "z", "p"))
  
  if (x$method.ci != "Wald")
    dimnames(out)[[2]][6] <- "Chisq"
  print(out)
  
  #        if("loglik" %in% names(x)) cat("\nScore test=", x$score, " on ", x$df,
  #            " df, p=", 1 - pchisq(x$score, x$df), ", n=", x$n, "", sep = "")
  cat("\nWald test=", x$Wald, " on ", x$df, "df, p=", 1 - pchisq(x$Wald, x$df), ", n=", x$n, "\n\n", sep = "")
  
  invisible(x)
}