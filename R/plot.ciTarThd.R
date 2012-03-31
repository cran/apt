plot.ciTarThd <-
function (x,...)
{
    if (!(class(x) == "ciTarThd")) {
        stop("\nPlease provide an object of class 'ciTarThd'.\n") }

    par(mfrow=c(2,2))
    plot(path.sse~path.thr, data=x$path, type="l", col="blue", 
       ylab="SSE", xlab=paste("threshold value for", x$model))

    plot(path.aic~path.thr, data=x$path, type="l", col="green",
       xlab=paste("threshold value for", x$model))

    plot(path.bic~path.thr, data=x$path, type="l", col="red",
       xlab=paste("threshold value for", x$model))
}

