

# 
Dat <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))
source("/home/tim/git/MacroShape/MacroShape/R/LexisFieldFunctions.R")

# prelims:
N  <- 5
y  <- seq(0,80-N,N)
x  <- seq(1850,2015-N,N)

# lower left corner of each grid cell
ll           <- expand.grid(year=x,age=y)
ll$slope     <- NA
ll$slope_avg <- NA
Dat$SD       <- sqrt(Dat$Var)
# get slopes
for (i in 1:nrow(ll)){
	chunk <- get_chunk(year = ll$year[i],
			age = ll$age[i],
			sex = "m",
			N = N,
			Dat = Dat)
	ll$slope[i]     <- get_slope(chunk,yvar="SD")
	ll$slope_avg[i] <- get_avg_slope(chunk,yvar="SD")
}


graphics.off()
pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_m_ex_sdx.pdf",width=8,height=5)
lim <- max(abs(pretty(ll$slope,20)))
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(min(x), max(x) + N),
		ylim = c(min(y), max(y) + N),
		xlab = "Year", ylab = "Age",
		asp = 1,
		main = "Lexis field
sd vs mean of remaining lifespan, males (HMD)")
draw_field(ll,"slope",lim=lim,col = "red")
draw_field(ll,"slope_avg",lim=lim,col = "blue")
legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
dev.off()


# repeat for females
ll <- expand.grid(year=x,age=y)
ll$slope <- NA
ll$slope_avg <- NA
Dat$SD <- sqrt(Dat$Var)
# get slopes
for (i in 1:nrow(ll)){
	chunk <- get_chunk(year = ll$year[i],
			age = ll$age[i],
			sex = "f",
			N = N,
			Dat = Dat)
	ll$slope[i]     <- get_slope(chunk,yvar="SD")
	ll$slope_avg[i] <- get_avg_slope(chunk,yvar="SD")
}
pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_f_ex_sdx.pdf",width=8,height=5)
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(min(x), max(x) + N),
		ylim = c(min(y), max(y) + N),
		xlab = "Year", ylab = "Age",
		asp = 1,
		main = "Lexis field
				sd vs mean of remaining lifespan, females (HMD)")
draw_field(ll,"slope",lim=lim,col = "red")
draw_field(ll,"slope_avg",lim=lim,col = "blue")
legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
dev.off()

# --------------------------------------

head(Dat)
ll <- expand.grid(year=x,age=y)
ll$mslope <- NA
ll$mslope_avg <- NA
ll$fslope <- NA
ll$fslope_avg <- NA
Dat$SD <- sqrt(Dat$Var)
# get slopes
for (i in 1:nrow(ll)){
	chunk <- get_chunk(year = ll$year[i],
			age = ll$age[i],
			sex = "m",
			N = N,
			Dat = Dat)
	ll$mslope[i]     <- get_slope(chunk,yvar="Skurt")
	ll$mslope_avg[i] <- get_avg_slope(chunk,yvar="Skurt")
	chunk <- get_chunk(year = ll$year[i],
			age = ll$age[i],
			sex = "f",
			N = N,
			Dat = Dat)
	ll$fslope[i]     <- get_slope(chunk,yvar="Skurt")
	ll$fslope_avg[i] <- get_avg_slope(chunk,yvar="Skurt")
}

lim <- max(abs(pretty(ll$mslope,20)))
graphics.off()
dev.new(width=8,height=5)
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(min(x), max(x) + N),
		ylim = c(min(y), max(y) + N),
		xlab = "Year", ylab = "Age",
		asp = 1,
		main = "Lexis field
				kurt vs mean of remaining lifespan, females (HMD)")
draw_field(ll,"fslope",lim=lim,shrink=.4,col = "red")
draw_field(ll,"fslope_avg",lim=lim,shrink=.4,col = "blue")
legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)

# skew vs kurt
ll <- expand.grid(year=x,age=y)
ll$slope <- NA
head(Dat)

# get slopes
for (i in 1:nrow(ll)){
	chunk <- get_chunk(year = ll$year[i],
			age = ll$age[i],
			sex = "m",
			N = N,
			Dat = Dat)
	ll$slope[i]     <- get_slope(chunk,xvar = "Sskew", yvar="Skurt")
}

lim <- max(abs(pretty(ll$slope,20)))
graphics.off()
dev.new(width=8,height=5)
pdf("/home/tim/git/MacroShape/MacroShape/Figures/LexF_m_sk_kurt.pdf",height=5,width=8)
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(min(x), max(x) + N),
		ylim = c(min(y), max(y) + N),
		xlab = "Year", ylab = "Age",
		asp = 1,
		main = "Lexis field
				skew vs kurt of remaining lifespan, females (HMD)")
draw_field(ll,"slope",lim=lim,shrink=.4,col = "red")
#legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
dev.off()

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
llf <- DT_exsd[DT_exsd$Sex == "f" & DT_exsd$Year >= 1950 & DT_exsd$Age %% 5 == 0 & DT_exsd$Age < 100,]
lim <- 0.9
head(llm)
par(xaxs = 'i', yaxs = 'i')
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "Year", ylab = "Age",
		asp = 1,
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

# cells faded w crossover:
# find crossover in each year for males
llm1 <- DT_exsd[DT_exsd$Sex == "m" & DT_exsd$Year >= 1950 & DT_exsd$Age < 100,]
plot(llm1$Age[llm1$Year5 == 1950],llm1$slope[llm1$Year5 == 1950])
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
