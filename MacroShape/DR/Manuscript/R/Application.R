
# Author: tim
###############################################################################
setwd("/home/tim/git/MacroShape/MacroShape/")
library(data.table)
source("DR/Manuscript/R/ApplicationFunctions.R")
Dat          <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))

Dat$SD       <- sqrt(Dat$Var)

Dat          <- Dat[Dat$Age %% 5 == 0 & Dat$Age < 105, ]

Dat          <- Dat[Dat$Year >= 1950, ]
Dat$Year5    <- Dat$Year - Dat$Year %% 5
Dat$Age5     <- Dat$Age - Dat$Age %% 5

Dat          <- data.table(Dat)
DatMod       <- Dat[,get_brr_dt(.SD,xvar="ex",yvar="SD"),by=list(Sex, Year5, Age)]
head(DatMod)

# no need to scale b: 45 degrees = 1:1
#summary(DatMod$xrange)
# DatMod$xrange
DatMod <- data.frame(DatMod)
Fem    <- DatMod[DatMod$Sex == "f", ]
Mal    <- DatMod[DatMod$Sex == "m", ]

head(Fem)
x <- Fem[1,]

xr <- 65
yr <- 105
scalef <- .1
width <- xr * scalef + .4
height <- yr * scalef + .4


pdf("DR/Manuscript/Figures/FigApp1.pdf", width = width, height = height)
par(mai=c(.3,.3,.1,.1))
plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2015,105,border = NA,col = gray(.92)),
				segments(seq(1950,2015,by=5),0,seq(1950,2015,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2015,seq(0,105,by=5),col="white"),
				text(seq(1950,2010,by=10),0,seq(1950,2010,by=10),pos=1,xpd=TRUE,cex=1),
				text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1)))
#plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1)
for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = 5,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = 1, # default meaning = interval - (2*pad)
			pad = .5,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  1)
}
dev.off()

pdf("DR/Manuscript/Figures/FigApp2.pdf", width = width, height = height)
par(mai=c(.3,.3,.1,.1))
plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2015,105,border = NA,col = gray(.92)),
				segments(seq(1950,2015,by=5),0,seq(1950,2015,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2015,seq(0,105,by=5),col="white"),
				text(seq(1950,2010,by=10),0,seq(1950,2010,by=10),pos=1,xpd=TRUE,cex=1),
				text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1)))
#plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1)
for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = 5,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = x$rsq, # default meaning = interval - (2*pad)
			pad = 0,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  1)
}
dev.off()

grayrange <- c(0,.7)

		
pdf("DR/Manuscript/Figures/FigApp3.pdf",width=width,height=height)
par(mai=c(.3,.3,.1,.1))
plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2015,105,border = NA,col = gray(.92)),
				segments(seq(1950,2015,by=5),0,seq(1950,2015,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2015,seq(0,105,by=5),col="white"),
				text(seq(1950,2010,by=10),0,seq(1950,2010,by=10),pos=1,xpd=TRUE,cex=1),
                text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1)))
for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = 5,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = x$iqr/3, # default meaning = interval - (2*pad)
			pad = .05,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  2,
			col = gray(grayrange[2]-x$rsq*diff(grayrange)+grayrange[1]),
			lwd = .5 + 2*x$rsq,
			xpd=TRUE)
}
dev.off()

