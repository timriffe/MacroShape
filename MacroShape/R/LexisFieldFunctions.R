 
# Author: tim
###############################################################################


# function to select subset:
get_chunk <- function(year, age, sex = "m", N = 10, Dat){
	ind <- Dat$Year >= year & Dat$Year < (year + N) &
			# Dat$Age >= age & Dat$Age < (age + N) & 
			Dat$Age == age &
			Dat$Sex == sex
	Dat[ind, ]
}

get_slope <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk   <- data.frame(chunk)
	xrange  <- range(chunk[,xvar], na.rm = TRUE)
	mod     <- lm(paste0(yvar,"~", xvar), data = chunk)
	#xout    <- seq(xrange[1], xrange[2], length = 20)
	#newdata <- data.frame(xout)
	#colnames(newdata) <- xvar
	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
	#cbind(newdata,out)
	mod$coef[2]
}

get_field <- function(years, ages, sex = "m", N = 5, Dat, xvar = "ex", yvar = "SD"){
	ll       <- expand.grid(year = years, age = ages)
	ll$slope <- NA
	for (i in 1:nrow(ll)){
		chunk <- get_chunk(
				year = ll$year[i],
				age = ll$age[i],
				sex = sex,
				N = N,
				Dat = Dat)
		ll$slope[i]     <- get_slope(chunk, xvar = xvar, yvar = yvar)
	}
	ll
}

#get_avg_slope <- function(chunk, xvar = "ex", yvar = "CV", control = "CNTRY"){
#	
#	xrange  <- range(chunk[,xvar], na.rm = TRUE)
#	
#	mod     <- lm(paste0(yvar,"~", xvar, "+", control), data = chunk)
#	#xout    <- seq(xrange[1], xrange[2], length = 20)
#	#newdata <- data.frame(xout)
#	#colnames(newdata) <- xvar
#	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
#	#cbind(newdata,out)
#	mod$coef[2]
#}

draw_slope <- function(slope, year, age, N = 5, pad = 0, lim = .065, ...){
	
	side <- lim * 2
	yl   <- ((lim - slope) / side ) * N + age
	yr   <- ((lim + slope) / side ) * N + age
	segments(year + pad, yl, year + N - pad, yr, ...)
}

draw_field <- function(ll, slopevar = "slope",yearvar = "year",agevar ="age",lim=.065,shrink=.6,...){
	for (i in 1:nrow(ll)){
		draw_slope(
				slope = ll[i, slopevar],
				year = ll[i, yearvar],
				age = ll[i, agevar],
				lim = lim * shrink,
				...)
	}
}


