ciTarFit <- function(y, x, model = c("tar","mtar"), lag, thresh, small.win, ...)
{
  if(!is.ts(y) | !is.ts(x) ) {stop("Please provide time series data.\n")}
  if (!identical(tsp(y),tsp(x))) {stop("y and x have different properties.\n")}
  model <- match.arg(model)
  
  A <- start(y); B <- end(y); Q <- tsp(y)[3]
  name.y <- deparse(substitute(y)); name.x <- deparse(substitute(x))
  data.LR <- data.frame(cbind(y, x)); colnames(data.LR) <- c(name.y, name.x)
  formula.LR <- as.formula(paste(name.y, "~", name.x, sep=""))
  LR <- lm(formula.LR, data = data.LR) 
  z <- ts(residuals(LR), start = A, end = B, frequency = Q)  
  lz <- lag(z, k = -1); dz <- diff(z); ldz <- lag(dz, k = -1)
  
  if(model=="tar")  { ind <- ifelse( lz  >= thresh, 1, 0) }
  if(model=="mtar") { ind <- ifelse( ldz >= thresh, 1, 0) }
  pos <- lz * ind
  neg <- lz * (1 - ind)
  
  xx <- bsLag(h = dz, lag = lag, var.name = "diff.resid")
  if(tsp(xx)[1] >= tsp(pos)[1]) {sa <- start(xx)} else {sa <- start(pos)}       
  data.CI <- window(cbind(pos, neg, xx), start=sa, end=B, frequency=Q)
  colnames(data.CI) <- c("pos.resid.t_1", "neg.resid.t_1", colnames(xx)) 
  
  if (!missing(small.win)){
    data.CI <- window(data.CI, start = small.win, end = B, frequency = Q)
  }
  
  CI <- lm(diff.resid.t_0 ~ 0 + ., data=data.CI)
  f.phi <- linearHypothesis(CI, c("pos.resid.t_1 = 0", "neg.resid.t_1 = 0")) 
  f.apt <- linearHypothesis(CI, "pos.resid.t_1 = neg.resid.t_1")
  sse <- deviance(CI)
  aic <- AIC(CI, k = 2)
  bic <- AIC(CI, k = log(nrow(data.CI)))    
  
  result <- list(y=y, x=x, model=model, lag=lag, thresh=thresh, 
      data.LR=data.LR, data.CI=data.CI, z=z, lz=lz, ldz=ldz, 
      LR=LR, CI=CI, f.phi=f.phi, f.apt=f.apt, sse=sse, aic=aic, bic=bic)
  class(result) <- "ciTarFit"
  return(result)
} 

print.ciTarFit <- function(x, ...)
{
  cat("=== Long Run Regression\n") ; print(summary(x$LR))
  cat("=== Threshold Cointegration Regression\n"); print(summary(x$CI))
  cat("=== H1: No cointegration b/w two variables\n"); print(x$f.phi)
  cat("=== H2: Symmetric adjustment in the long run\n"); print(x$f.apt)    
} 