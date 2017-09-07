
Dat <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))
# for JM
#Dat <- get(load("C:/Users/jmaburto/Documents/Riffe&Aburto_macroshapedata/HMDresults.Rdata"))

Dat$SD       <- sqrt(Dat$Var)
source("/home/tim/git/MacroShape/MacroShape/R/LexisFieldFunctions.R")

# for JM
#source("C:/Users/jmaburto/Documents/GitHub/MacroShape/MacroShape/R/LexisFieldFunctions.R")

library(RColorBrewer)
library(data.table)
library(reshape2)
# ------------------------------------
# a slicker way to calculate these slopes

DT       <- data.table(Dat)
DT$Year5 <- DT$Year - DT$Year %% 5
# this does the same as get_field
DT_exsd  <- DT[,list(slope = get_slope(.SD,xvar = "ex", yvar = "SD")), by = list(Age, Year5, Sex)]
DT_exsd  <- as.data.frame(DT_exsd)
# slopes within single ages, over 5 calendar years, all avail countries and years
# in this particular version of HMD...

llm <- DT_exsd[DT_exsd$Sex == "m" & DT_exsd$Year >= 1950 & DT_exsd$Age %% 5 == 0 & DT_exsd$Age < 100,]
#llf <- DT_exsd[DT_exsd$Sex == "f" & DT_exsd$Year >= 1950 & DT_exsd$Age %% 5 == 0 & DT_exsd$Age < 100,]
lim <- 0.9



# this code puts all scatterplots on same coordinate range.
# I don't know if that makes sense or not. The purpose is to 
# compare relationship slopes, but the scatterplots themselves
# get smallish for individual age/year ranges
# scatterplot of all years, age 0. For that reason I put a box around
# the point range for each selection.

# ------------------------------------

# same Lexis field, grid removed for clarity.
# point out age bands with major changes over time.
# for example, sign switching over time in ages 40-50
# less steep in age 0 over time, less steep in age 80
# over time, etc
pdf(file="/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/Fig9.pdf",width=65/20+1,height=6)
#dev.new(width=65/20+1,height=6)
par(xaxs = 'i', yaxs = 'i',mai=c(.8,.8,.2,.2))
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "", ylab = "",
		#main = list("Slope in Life expectancy by SD at different ages",cex=1.5),
		asp = 1,
		las = 1
)
draw_field(llm,
		slopevar="slope",
		yearvar = "Year5",
		agevar = "Age",
		lim=lim,
		shrink=.5,
		col = "black",
		N = 5,
		pad = .5)
text(1980,-10,"Year",xpd=TRUE,font=2)
text(1940,50,"Age",xpd=TRUE,font=2,srt=90)
dev.off()
# ------------------------------------
# cells faded w crossover:
# find crossover in each year for males
llm1 <- DT_exsd[DT_exsd$Sex == "m" & DT_exsd$Year >= 1950 & DT_exsd$Age < 100,]

# what about slope contours?
get_crossover_chunk <- function(ll, sl = 0,variable = "slope", agevar = "Age", spar = .3){
	ll <- data.frame(ll)
	mod <- smooth.spline(x = ll[, variable], y = ll[, agevar], spar = spar)
	predict(mod, x = sl)$y
}
crossovers <- data.table(llm1)[, list(crossover = get_crossover_chunk(.SD,sl=0,spar = .3)), by = list(Year5)]

pdf(file="/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/Fig10.pdf",width=65/20+1,height=6)
par(xaxs = 'i', yaxs = 'i',mai=c(.8,.8,.2,.2))
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "", ylab = "",
		#main = list("Slope in Life expectancy by SD at different ages",cex=1.5),
		asp = 1,
		las = 1
)
draw_field(llm,
		slopevar="slope",
		yearvar = "Year5",
		agevar = "Age",
		lim=lim,
		shrink=.5,
		col = gray(.6),
		N = 5,
		pad = .5)
# this is crossover in slope. Maybe compare with a different crossover?
lines(crossovers$Year5 + 2.5, crossovers$crossover+2.5, col = "red", lwd = 2)
text(1980,-10,"Year",xpd=TRUE,font=2)
text(1940,50,"Age",xpd=TRUE,font=2,srt=90)
dev.off()



pdf(file="/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/Fig11.pdf",width=65/20+1,height=6)
par(xaxs = 'i', yaxs = 'i',mai=c(.8,.8,.2,.2))
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "", ylab = "",
		#main = list("Slope in Life expectancy by SD at different ages",cex=1.5),
		asp = 1,
		las = 1
)
breaks <- seq(-1,1,by=.2)
image(seq(1952.5,2012.5,by=5),seq(2.5,97.5,by=5),
		t(acast(llm,Age~Year5,value.var = "slope")),xlim = c(1950,2015),ylim=c(0,100),
		add = TRUE,breaks=breaks,col=rev(RColorBrewer::brewer.pal(10,"RdBu")))
#contour(seq(1952.5,2012.5,by=5),seq(2.5,97.5,by=5),
#		t(acast(llm,Age~Year5,value.var = "slope")),xlim = c(1950,2015),ylim=c(0,100),
#		add = TRUE,breaks=breaks)

draw_field(llm,
		slopevar="slope",
		yearvar = "Year5",
		agevar = "Age",
		lim=lim,
		shrink=.5,
		col = gray(.6),
		N = 5,
		pad = .5)
# this is crossover in slope. Maybe compare with a different crossover?
for (sl in seq(-.2,.8,by=.2)){
	crossovers <- data.table(llm1)[, list(crossover = get_crossover_chunk(.SD,sl=sl,spar = .5)), by = list(Year5)]
	lines(crossovers$Year5 + 2.5, 
			crossovers$crossover+2.5, 
			col = ifelse(zapsmall(sl)==0,"red","black"), lwd = ifelse(zapsmall(sl)==0,2,1))
	text(2016,rev(crossovers$crossover)[1]+2.5,sl,xpd=TRUE)
}
text(1980,-10,"Year",xpd=TRUE,font=2)
text(1940,50,"Age",xpd=TRUE,font=2,srt=90)
dev.off()

pdf(file="/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/Fig12.pdf",width=65/20+1,height=6)
par(xaxs = 'i', yaxs = 'i',mai=c(.8,.8,.2,.2))
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "", ylab = "",
		#main = list("Slope in Life expectancy by SD at different ages",cex=1.5),
		asp = 1,
		las = 1
)
breaks <- seq(-1,1,by=.2)
image(seq(1952.5,2012.5,by=5),seq(2.5,97.5,by=5),
		t(acast(llm,Age~Year5,value.var = "slope")),xlim = c(1950,2015),ylim=c(0,100),
		add = TRUE,breaks=breaks,col=rev(RColorBrewer::brewer.pal(10,"RdBu")))

# this is crossover in slope. Maybe compare with a different crossover?
for (sl in seq(-.2,.8,by=.2)){
	crossovers <- data.table(llm1)[, list(crossover = get_crossover_chunk(.SD,sl=sl,spar = .5)), by = list(Year5)]
	lines(crossovers$Year5 + 2.5, 
			crossovers$crossover+2.5, 
			col = ifelse(zapsmall(sl)==0,"red","black"), lwd = ifelse(zapsmall(sl)==0,2,1))
	text(2016,rev(crossovers$crossover)[1]+2.5,sl,xpd=TRUE)
}
text(1980,-10,"Year",xpd=TRUE,font=2)
text(1940,50,"Age",xpd=TRUE,font=2,srt=90)
dev.off()





