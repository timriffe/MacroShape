 
# Author: tim
###############################################################################


# function to select subset:
get_chunk <- function(year, age, sex = "m", N = 10, Dat){
	ind <- Dat$Year >= year & Dat$Year < (year + N) &
			Dat$Age >= age & Dat$Age < (age + N) & 
			#Dat$Age == age &
			Dat$Sex == sex
	Dat[ind, ]
}


get_coefs <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk   <- data.frame(chunk)
	#xrange  <- range(chunk[,xvar], na.rm = TRUE)
	mod     <- lm(paste0(yvar,"~", xvar), data = chunk)
	#xout    <- seq(xrange[1], xrange[2], length = 20)
	#newdata <- data.frame(xout)
	#colnames(newdata) <- xvar
	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
	#cbind(newdata,out)
	mod$coef
}

get_slope <- function(chunk, xvar = "ex", yvar = "CV"){
	get_coefs(chunk=chunk,xvar=xvar,yvar=yvar)[2]
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


draw_vector <- function(slope, len, year, age, N = 5, arrow = FALSE, headlength=.025,...){
	# assuming square Lexis cells, these are the centroid coords
	cx  <- year + N / 2
	cy  <- age + N / 2
	
	# convert slope to radians
	rad <- atan(slope)
	
	if (missing(len)){
		len <- N
	}
	# now get origin-centered coords corresponding to unit circle
	# positions of rad and rad+pi, then rescaled so that distance 
	# between points = len
	x1  <- cos(rad + pi) * len / 2 + cx
	y1  <- sin(rad + pi) * len / 2 + cy
	
	x2  <- cos(rad) * len / 2 + cx
	y2  <- sin(rad) * len / 2 + cy
	
    if (arrow){
		arrows(x1,y1,x2,y2,length=headlength,...)
	} else {
		segments(x1,y1,x2,y2,...)
	}
	
}

get_vector_field <- function(years, ages, sex = "m", N = 5, Dat, xvar = "ex", yvar = "SD", lvar = "xrange"){
	ll       <- expand.grid(year = years, age = ages)
	ll$slope <- NA
	ll$len   <- NA
	for (i in 1:nrow(ll)){
		chunk <- get_chunk(
				year = ll$year[i],
				age = ll$age[i],
				sex = sex,
				N = N,
				Dat = Dat)
		chunk <- data.frame(chunk)
		mod     <- lm(paste0(yvar, "~", xvar), data = chunk)
		if (lvar == "xrange"){
			len <- diff(range(chunk[,xvar],na.rm=TRUE))
		} 
		ll$slope[i] <- mod$coef[2]
		ll$len[i]   <- len
	}
	ll
}

draw_vector_field <- function(ll, slopevar = "slope",lengthvar = "len", yearvar = "year",agevar ="age",arrow=TRUE,...){
	for (i in 1:nrow(ll)){
		draw_vector(
				slope = ll[i, slopevar],
				len = ll[i, lengthvar],
				year = ll[i, yearvar],
				age = ll[i, agevar],
				N = 5,
				arrow=arrow,
				...)
	}
}

#for (i in 1:nrow(ll)){
#	draw_vector(
#			slope = ll[i, slopevar],
#			.len = ll[i, lengthvar],
#			year = ll[i, yearvar],
#			age = ll[i, agevar],
#			N = 5,
#			arrow=arrow,
#			length=.05)
#}