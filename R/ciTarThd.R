ciTarThd <-
function(y, x, model=c("tar","mtar"), lag, th.range=0.15, digits=3,... )
{
    if(!is.ts(y) | !is.ts(x) ) {stop("Please provide time series data.\n")}
    if (!identical(tsp(y), tsp(x))) {stop("Properties of y and x are different.\n")}
    model <- match.arg(model)
  
    Q <- ciTarFit(y=y, x=x, model=model, lag=lag, thresh=0)
    if(model=="tar")  { thresh.test <- sort(as.vector(Q$lz)) } 
    if(model=="mtar") { thresh.test <- sort(as.vector(Q$ldz)) }
    obs.tot <- NROW(y); obs.CI  <- nrow(Q$data.CI)
    a <- ceiling(obs.CI*th.range)
    b <- obs.CI - floor(obs.CI*th.range)

    path.num <- path.thr <- path.sse <- path.aic <- path.bic <- NULL
    sse.lowest <- thresh.final <- 10^20
    for(i in a:b) {
        H <- ciTarFit(y=y, x=x, model=model, lag=lag, thresh=thresh.test[i])
        path.num[i-a+1] <- i-a+1
        path.thr[i-a+1] <- thresh.test[i]
        path.sse[i-a+1] <- as.numeric(H$sse)
        path.aic[i-a+1] <- as.numeric(H$aic)
        path.bic[i-a+1] <- as.numeric(H$bic)
        if ( sse.lowest >  H$sse ) {
             sse.lowest <- H$sse
             thresh.final <- thresh.test[i]
        }
    }
    path <- data.frame(path.num, path.thr, path.sse, path.aic, path.bic)
    Item <- c("lag", "thresh final", "thresh range", "sse.lowest", 
       "Total obs", "CI obs", "Lower obs", "Upper obs")
    Value <- round(c(lag, thresh.final, th.range, sse.lowest,
        obs.tot, obs.CI, a, b), digits=digits)
    basic <- data.frame(Item, Value) 
    colnames(basic)[2] <- model
    
    result <- list(model=model, lag=lag, th.range=th.range, th.final=thresh.final, 
        ssef=sse.lowest, obs.tot=obs.tot, obs.CI=obs.CI, 
        basic=basic, path=path)
    class(result) <- "ciTarThd"
    return(result)
}

