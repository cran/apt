### R code from vignette source 'apt.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ")
options(SweaveHooks = list(
  cex = function() par(cex.lab = 1.3, cex.axis = 1.3)))


###################################################
### code chunk number 2: apt.Rnw:155-160
###################################################
library(apt)
data(daVich)
daVi <- y <- daVich[, 1] 
daCh <- x <- daVich[, 2]
bsStat(daVich)


###################################################
### code chunk number 3: apt.Rnw:165-168
###################################################
library(urca)
adf.xa <- ur.df(daCh,  type=c("trend"), lags=3)
adf.xa


###################################################
### code chunk number 4: apt.Rnw:172-173
###################################################
plot(adf.xa)


