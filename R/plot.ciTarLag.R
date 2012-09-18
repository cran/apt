plot.ciTarLag <- function(x, ...)
{
  xlabel <- paste("Lag value (model = ", x$out[1,2], 
      ", threshold = ", x$out[3, 2], ")")
  par(mfrow=c(2,1))
  plot(aic~lag, data=x$path, type="l", col="green", xlab=xlabel, ylab="AIC")
  plot(bic~lag, data=x$path, type="l", col="red", xlab=xlabel, ylab="BIC")
}