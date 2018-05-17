# Author: tim
###############################################################################
setwd("/home/tim/git/MacroShape/MacroShape")
# 1)
# generic relationship conditioned on a particular age and time.
# y = m*x+b
set.seed(1)
b <- .1
m <- .6
error <- rnorm(30) * .1
x <- runif(30)
y <- m * x + b + error
ba <- lm(y~x)$coef
xnew <- c(.05,.95)
ynew <- ba[1] + xnew * ba[2]

pdf("DR/Manuscript/Figures/GenericLinear.pdf",width=3,height=2)
par(mai = c(.2,.2,.2,.2))
plot(NULL, type = "n", xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", axes = FALSE)
points(x,y,pch=16,cex=.7)
segments(xnew[1],ynew[1],xnew[2],ynew[2],lwd=3)
segments(min(x),0,max(x),0,lwd=2)
segments(0,min(y),0,max(y),lwd=2)
dev.off()
