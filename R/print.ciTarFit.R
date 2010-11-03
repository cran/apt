print.ciTarFit <-
function (x,...)
{
    if (!(class(x) == "ciTarFit")) {
        stop("\nPlease provide an object of class 'ciTarFit'.\n")
    }

    cat("\n==================================================================\n")
    cat("    Results of Long Run Regression\n")
    cat("==================================================================\n")
    print(summary(x$LR))

    cat("\n==================================================================\n")
    cat("    Results of Threshold Cointegration Regression\n")
    cat("==================================================================\n")
    print(summary(x$CI))

    cat("\n==================================================================\n")
    cat("    Hypothesis 1: No cointegration between the two variables \n")
    cat("==================================================================\n")
    print(x$f.phi)
    
    cat("\n==================================================================\n")
    cat("    Hypothesis 2: Symmetric adjustment in the long run equilibrium\n")
    cat("==================================================================\n")
    print(x$f.apt)    
}

