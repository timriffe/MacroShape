

# 
Dat <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))
Dat$SD       <- sqrt(Dat$Var)
source("/home/tim/git/MacroShape/MacroShape/R/LexisFieldFunctions.R")


# following code just does figs for males.
# need to experiment w dims for Lexis fields.

# ------------------------------------
# a slicker way to calculate these slopes
library(data.table)
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
head(llm)


# this code puts all scatterplots on same coordinate range.
# I don't know if that makes sense or not. The purpose is to 
# compare relationship slopes, but the scatterplots themselves
# get smallish for individual age/year ranges
# scatterplot of all years, age 0. For that reason I put a box around
# the point range for each selection.
Dat <- as.data.frame(Dat)
ind_basic <- Dat$Sex == "m" & Dat$Age == 0
plot(Dat[ind_basic,"ex"], Dat[ind_basic,"SD"], pch = 16, col = "#11111130",
		xlim = c(0,80), ylim = c(0,35),
		xlab = "e(x)", ylab = "sd(x)",
		las = 1)
abline(lm(Dat[ind_basic,"SD"]~ Dat[ind_basic,"ex"]),col = "red",lwd=3)
rect(min(Dat[ind_basic,"ex"], na.rm = TRUE), min(Dat[ind_basic,"SD"], na.rm = TRUE),
		max(Dat[ind_basic,"ex"], na.rm = TRUE),max(Dat[ind_basic,"SD"], na.rm = TRUE))

# just years since 1950, age 0
ind_1950 <- Dat$Sex == "m" & Dat$Age == 0 & Dat$Year >= 1950
plot(Dat[ind_1950,"ex"], Dat[ind_1950,"SD"], pch = 16, col = "#11111130",
		xlim = c(0,80), ylim = c(0,35),
		xlab = "e(x)", ylab = "sd(x)",
		las = 1, 
		panel.first = list(
				abline(lm(Dat[ind_basic,"SD"]~ Dat[ind_basic,"ex"]), col = gray(.7), lwd = 1)))
abline(lm(Dat[ind_1950,"SD"]~ Dat[ind_1950,"ex"]), col = "red", lwd = 3)
rect(min(Dat[ind_1950,"ex"]), min(Dat[ind_1950,"SD"]),
		max(Dat[ind_1950,"ex"]),max(Dat[ind_1950,"SD"]))

# just years 1950-54, age 0
ind_1954 <- Dat$Sex == "m" & Dat$Age == 0 & Dat$Year >= 1950 & Dat$Year <= 1954
plot(Dat[ind_1954,"ex"], Dat[ind_1954,"SD"], pch = 16, col = "#11111130",
		xlim = c(0,80), ylim = c(0,35),
		xlab = "e(x)", ylab = "sd(x)",
		las = 1,
		panel.first = list(
				abline(lm(Dat[ind_basic,"SD"]~ Dat[ind_basic,"ex"]), col = gray(.7), lwd = 1),
				abline(lm(Dat[ind_1950,"SD"]~ Dat[ind_1950,"ex"]), col = gray(.7), lwd = 1)))
abline(lm(Dat[ind_1954,"SD"]~ Dat[ind_1954,"ex"]), col = "red", lwd = 3)
rect(min(Dat[ind_1954,"ex"]), min(Dat[ind_1954,"SD"]),
		max(Dat[ind_1954,"ex"]),max(Dat[ind_1954,"SD"]))

# just years 1950-54, age 80
ind_1954_80 <- Dat$Sex == "m" & Dat$Age == 80 & Dat$Year >= 1950 & Dat$Year <= 1954
plot(Dat[ind_1954_80,"ex"], Dat[ind_1954_80,"SD"], pch = 16, col = "#11111130",
		xlim = c(0,80), ylim = c(0,35),
		xlab = "e(x)", ylab = "sd(x)",
		las = 1,
		panel.first = list(
				abline(lm(Dat[ind_basic,"SD"]~ Dat[ind_basic,"ex"]), col = gray(.7), lwd = 1),
				abline(lm(Dat[ind_1950,"SD"]~ Dat[ind_1950,"ex"]), col = gray(.7), lwd = 1),
				abline(lm(Dat[ind_1954,"SD"]~ Dat[ind_1954,"ex"]), col = gray(.7), lwd = 1)))
abline(lm(Dat[ind_1954_80,"SD"]~ Dat[ind_1954_80,"ex"]), col = "red", lwd = 3)
rect(min(Dat[ind_1954_80,"ex"]), min(Dat[ind_1954_80,"SD"]),
		max(Dat[ind_1954_80,"ex"]),max(Dat[ind_1954_80,"SD"]))

# or here's an animation, jumping every 10 ages in the 1950-1954 window. Can then hop to the Lexis
# grid from here. You could save these as pdf frames if you wanted.
for (i in seq(0,100,by=10)){
	ind_i <- Dat$Sex == "m" & Dat$Age == i & Dat$Year >= 1950 & Dat$Year <= 1954
	
	plot(Dat[ind_i,"ex"], Dat[ind_i,"SD"], pch = 16, col = "#11111130",
			xlim = c(0,80), ylim = c(0,35), type = "n", las = 1,xlab = "e(x)", ylab = "sd(x)")
	if (i > 0){
		for (j in seq(0,i-10,by = 10)){
			ind_j <- Dat$Sex == "m" & Dat$Age == j & Dat$Year >= 1950 & Dat$Year <= 1954
			abline(lm(Dat[ind_j,"SD"]~ Dat[ind_j,"ex"]), col = gray(.7), lwd = 1)
		}
	}
	points(Dat[ind_i,"ex"], Dat[ind_i,"SD"], pch = 16, col = "#11111130")
	abline(lm(Dat[ind_i,"SD"]~ Dat[ind_i,"ex"]), col = "red", lwd = 3)
	
	rect(min(Dat[ind_i,"ex"]), min(Dat[ind_i,"SD"]),
			max(Dat[ind_i,"ex"]),max(Dat[ind_i,"SD"]))
	text(min(Dat[ind_i,"ex"]),max(Dat[ind_i,"SD"]),i,pos=3,cex=2,font=2)
	Sys.sleep(.4)
}

# ---------------------------------
# Now say you want to do this for years after 1950 too
# in order to see how the relationship changes over time
# as well as age. Take previous set of slopes and display
# on standard scale on the Lexis grid. Note: each slope y axis
# is on a non-age scale, but the slope is drawn centered on the
# age group
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "Year", ylab = "Age",
		asp = 1,
		las = 1,
		panel.first = list(
				rect(1950,0,2015,100,col=gray(.8), border = NA),
				abline(v=seq(1950,2015,by=5),col="white"),
                abline(h=seq(0,100,by=5),col="white"))

)
for (i in 1:20){
	draw_slope(
			slope = llm[i, "slope"],
			year = llm[i, "Year5"],
			age = llm[i,"Age"],
			lim = lim * .5,
			pad = .5)
}
# ------------------------------------
# A full Lexis field (on same grid)
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "Year", ylab = "Age",
		asp = 1,
		las = 1,
		panel.first = list(
				rect(1950,0,2015,100,col=gray(.8), border = NA),
				abline(v=seq(1950,2015,by=5),col="white"),
				abline(h=seq(0,100,by=5),col="white"))
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

# same Lexis field, grid removed for clarity.
# point out age bands with major changes over time.
# for example, sign switching over time in ages 40-50
# less steep in age 0 over time, less steep in age 80
# over time, etc
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "Year", ylab = "Age",
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
# ------------------------------------
# cells faded w crossover:
# find crossover in each year for males
llm1 <- DT_exsd[DT_exsd$Sex == "m" & DT_exsd$Year >= 1950 & DT_exsd$Age < 100,]

get_crossover_chunk <- function(ll, variable = "slope", agevar = "Age", spar = .3){
	ll <- data.frame(ll)
	mod <- smooth.spline(x = ll[, variable], y = ll[, agevar], spar = spar)
	predict(mod, x = 0)$y
}
crossovers <- data.table(llm1)[, list(crossover = get_crossover_chunk(.SD,spar = .3)), by = list(Year5)]

par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "Year", ylab = "Age",
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
lines(crossovers$Year5 + 2.5, crossovers$crossover, col = "red", lwd = 2)


# ------------------------------
# code for first expiriments:
# prelims:
#N  <- 5
#y  <- seq(0,80-N,N)
#x  <- seq(1850,2015-N,N)
#pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_m_ex_sdx.pdf",width=8,height=5)
#lim <- max(abs(pretty(ll$slope,20)))
#par(xaxs = 'i', yaxs = 'i')
#plot(NULL, 
#		type = "n", 
#		xlim = c(min(x), max(x) + N),
#		ylim = c(min(y), max(y) + N),
#		xlab = "Year", ylab = "Age",
#		asp = 1,
#		main = "Lexis field
#sd vs mean of remaining lifespan, males (HMD)")
#draw_field(ll,"slope",lim=lim,col = "red")
#draw_field(ll,"slope_avg",lim=lim,col = "blue")
#legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
#dev.off()
#
#
## repeat for females
#ll <- expand.grid(year=x,age=y)
#ll$slope <- NA
#ll$slope_avg <- NA
#Dat$SD <- sqrt(Dat$Var)
## get slopes
#for (i in 1:nrow(ll)){
#	chunk <- get_chunk(year = ll$year[i],
#			age = ll$age[i],
#			sex = "f",
#			N = N,
#			Dat = Dat)
#	ll$slope[i]     <- get_slope(chunk,yvar="SD")
#	ll$slope_avg[i] <- get_avg_slope(chunk,yvar="SD")
#}
#pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_f_ex_sdx.pdf",width=8,height=5)
#par(xaxs = 'i', yaxs = 'i')
#plot(NULL, 
#		type = "n", 
#		xlim = c(min(x), max(x) + N),
#		ylim = c(min(y), max(y) + N),
#		xlab = "Year", ylab = "Age",
#		asp = 1,
#		main = "Lexis field
#				sd vs mean of remaining lifespan, females (HMD)")
#draw_field(ll,"slope",lim=lim,col = "red")
#draw_field(ll,"slope_avg",lim=lim,col = "blue")
#legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
#dev.off()
#
## --------------------------------------
#
#head(Dat)
#ll <- expand.grid(year=x,age=y)
#ll$mslope <- NA
#ll$mslope_avg <- NA
#ll$fslope <- NA
#ll$fslope_avg <- NA
#Dat$SD <- sqrt(Dat$Var)
## get slopes
#for (i in 1:nrow(ll)){
#	chunk <- get_chunk(year = ll$year[i],
#			age = ll$age[i],
#			sex = "m",
#			N = N,
#			Dat = Dat)
#	ll$mslope[i]     <- get_slope(chunk,yvar="Skurt")
#	ll$mslope_avg[i] <- get_avg_slope(chunk,yvar="Skurt")
#	chunk <- get_chunk(year = ll$year[i],
#			age = ll$age[i],
#			sex = "f",
#			N = N,
#			Dat = Dat)
#	ll$fslope[i]     <- get_slope(chunk,yvar="Skurt")
#	ll$fslope_avg[i] <- get_avg_slope(chunk,yvar="Skurt")
#}
#
#lim <- max(abs(pretty(ll$mslope,20)))
#graphics.off()
#dev.new(width=8,height=5)
#par(xaxs = 'i', yaxs = 'i')
#plot(NULL, 
#		type = "n", 
#		xlim = c(min(x), max(x) + N),
#		ylim = c(min(y), max(y) + N),
#		xlab = "Year", ylab = "Age",
#		asp = 1,
#		main = "Lexis field
#				kurt vs mean of remaining lifespan, females (HMD)")
#draw_field(ll,"fslope",lim=lim,shrink=.4,col = "red")
#draw_field(ll,"fslope_avg",lim=lim,shrink=.4,col = "blue")
#legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
#
## skew vs kurt
#ll <- expand.grid(year=x,age=y)
#ll$slope <- NA
#head(Dat)
#
## get slopes
#for (i in 1:nrow(ll)){
#	chunk <- get_chunk(year = ll$year[i],
#			age = ll$age[i],
#			sex = "m",
#			N = N,
#			Dat = Dat)
#	ll$slope[i]     <- get_slope(chunk,xvar = "Sskew", yvar="Skurt")
#}
#
#lim <- max(abs(pretty(ll$slope,20)))
#graphics.off()
#dev.new(width=8,height=5)
#pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_m_sk_kurt.pdf",height=5,width=8)
#par(xaxs = 'i', yaxs = 'i')
#plot(NULL, 
#		type = "n", 
#		xlim = c(min(x), max(x) + N),
#		ylim = c(min(y), max(y) + N),
#		xlab = "Year", ylab = "Age",
#		asp = 1,
#		main = "Lexis field
#				skew vs kurt of remaining lifespan, females (HMD)")
#draw_field(ll,"slope",lim=lim,shrink=.4,col = "red")
##legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
#dev.off()
#






