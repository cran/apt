print.ciTarFit <- function(x, ...)
{
  cat("    Results of Long Run Regression\n")
  cat("==================================================================\n")
  print(summary(x$LR))

  cat("    Results of Threshold Cointegration Regression\n")
  cat("==================================================================\n")
  print(summary(x$CI))

  cat("    Hypothesis 1: No cointegration between the two variables \n")
  cat("==================================================================\n")
  print(x$f.phi)

  cat("    Hypothesis 2: Symmetric adjustment in the long run equilibrium\n")
  cat("==================================================================\n")
  print(x$f.apt)    
} 