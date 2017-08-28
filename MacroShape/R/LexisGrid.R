

# 
Dat <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDresults.Rdata")))


# function to select subset:
get_chunk <- function(year, age, sex = "m", N = 10, Dat){
   ind <- Dat$Year >= year & Dat$Year < (year + N) &
		 # Dat$Age >= age & Dat$Age < (age + N) & 
			Dat$Age == age &
		  Dat$Sex == sex
   Dat[ind, ]
}
get_slope <- function(chunk, xvar = "ex", yvar = "CV"){
	
	xrange  <- range(chunk[,xvar], na.rm = TRUE)
	mod     <- lm(paste0(yvar,"~", xvar), data = chunk)
	#xout    <- seq(xrange[1], xrange[2], length = 20)
	#newdata <- data.frame(xout)
	#colnames(newdata) <- xvar
	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
	#cbind(newdata,out)
	mod$coef[2]
}

get_avg_slope <- function(chunk, xvar = "ex", yvar = "CV", control = "CNTRY"){
	
	xrange  <- range(chunk[,xvar], na.rm = TRUE)
	
	mod     <- lm(paste0(yvar,"~", xvar, "+", control), data = chunk)
	#xout    <- seq(xrange[1], xrange[2], length = 20)
	#newdata <- data.frame(xout)
	#colnames(newdata) <- xvar
	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
	#cbind(newdata,out)
	mod$coef[2]
}

draw_slope <- function(slope, year, age, N = 10, pad = 0, lim = .065, ...){
	
	side <- lim * 2
	yl   <- ((lim - slope) / side ) * N + age
	yr   <- ((lim + slope) / side ) * N + age
	segments(year + pad, yl, year + N - pad, yr, ...)
}

draw_field <- function(ll, slopevar = "slope",...){
	for (i in 1:nrow(ll)){
		draw_slope(
				slope = ll[i, slopevar],
				year = ll$year[i],
				age = ll$age[i],
				N = N,
				pad = .5,
				lim = lim*.6,
				...)
	}
}



# get range of slopes, calc over ll

ll$slope <- NA
for (i in 1:nrow(ll)){
	chunk <- get_chunk(year = ll$year[i],
			           age = ll$age[i],
					   sex = "m",
					   N = 10,
					   Dat = Dat)
	ll$slope[i]    <-  get_slope(chunk)
}

# prelims:
N  <- 5
y  <- seq(0,80-N,N)
x  <- seq(1850,2015-N,N)

# lower left corner of each grid cell
ll <- expand.grid(year=x,age=y)
ll$slope <- NA
ll$slope_avg <- NA
Dat$SD <- sqrt(Dat$Var)
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
draw_field(ll,"slope",col = "red")
draw_field(ll,"slope_avg",col = "blue")
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
draw_field(ll,"slope",col = "red")
draw_field(ll,"slope_avg",col = "blue")
legend(1850,-12,lty=1,col=c("red","blue"),legend=c("slope of all points","controlling for country"),bty="n",xpd=TRUE)
dev.off()


