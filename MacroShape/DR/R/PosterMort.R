# Author: tim
###############################################################################

setwd("/home/tim/git/MacroShape/MacroShape/")
library(data.table)
library(reshape2)
library(RColorBrewer)
library(colorspace)
source("DR/Manuscript/R/ApplicationFunctions.R")


# Data for background surface:
Dat          <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))
Dat          <- Dat[Dat$Age < 105, ]
Dat          <- Dat[Dat$Year >= 1950, ]
Dat          <- data.table(Dat)
DatCV        <- Dat[,mean(CV),by=list(Sex, Year, Age)]
CV           <- acast(DatCV[Sex=="f" & Year < 2013], Age~Year,value.var ="V1")
breaks       <- pretty(CV, 25)


# Data for field, comes from same place...

# field grid size
gs           <- 3

Dat          <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))
Dat$SD       <- sqrt(Dat$Var)
Dat          <- Dat[Dat$Age %% gs == 0 & Dat$Age < 105, ]
Dat          <- Dat[Dat$Year >= 1950, ]
Dat$Year5    <- Dat$Year - Dat$Year %% gs
Dat$Age5     <- Dat$Age - Dat$Age %% gs
Dat          <- data.table(Dat)



DatMod       <- Dat[,get_brr_dt(.SD,xvar="ex",yvar="SD"),by=list(Sex, Year5, Age)]


# no need to scale b: 45 degrees = 1:1
#summary(DatMod$xrange)
# DatMod$xrange
DatMod <- data.frame(DatMod)
Fem    <- DatMod[DatMod$Sex == "f", ]

# now


grayrange    <- c(0,.7)
surfcols     <- sequential_hcl(length(breaks)-1, "Dark Mint", rev=TRUE)
ramp         <- colorRampPalette(brewer.pal(9,"YlOrRd"),space="Lab")
xr     <- 2012-1950-1
yr     <- 105
scalef <- .1
width  <- xr * scalef + 1
height <- yr * scalef + 1

pdf("DR/PAA/Mortality.pdf",width=width,height=height)
par(mai=c(.9,.9,.9,.1),xaxs="i",yaxs="i")
plot(NULL, 
		type = "n", 
		xlim = c(1950,2015), 
		ylim = c(0,105), 
		axes = FALSE, 
		xlab = "", 
		ylab = "", 
		asp = 1,
		las=1)

image(1950.5:2012.5, .5:104.5,
		t(CV),
		breaks = breaks,
		col = surfcols,
		xlim=c(1950,2013),
		ylim=c(0,105),
		asp=1,
		add=TRUE)
rect(1950,0,2013,105)
axis(1,pos=0,at=seq(1950,2010,by=10))
axis(2,pos=1950,at=seq(0,100,by=10),las=1)
contour(1950.5:2012.5, .5:104.5,
		t(CV),
		breaks = pretty(CV,10),
		xlim=c(1950,2013),
		ylim=c(0,105),
		asp=1,
		add=TRUE,
		col = "#00000070",
		labcex=.6)

for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = gs,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = x$iqrdiag/3, # default meaning = interval - (2*pad)
			pad = .1,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  2,
			col = gray(grayrange[2]-abs(x$rp)*diff(grayrange)+grayrange[1]),
			lwd = .5 + 2*abs(x$rp),
			xpd=TRUE)
}
dev.off()
