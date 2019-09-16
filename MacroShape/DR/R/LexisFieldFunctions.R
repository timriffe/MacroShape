 
# Author: tim
###############################################################################

# contents

# draw_field()
# draw_field_element()
# draw_slope()
# draw_vector()
# draw_vector_field()
# get_chunk()
# get_coefs()
# get_field()
# get_slope()
# get_vector_field()


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

# added 22-May, more flexible, no curvature, though
draw_field_element <- function(
		age = 0,       # lower bound of cell age
		year = 2000,   # lower bound of cell year
		interval = 5,  # cell dimension
		slope = 1,  # slope of regression or whatever
		length = 1, # default meaning = interval - (2*pad)
		pad = .1,   # edge pad if length = 1 and slope = 0 or Inf
		lambda = 1, # to expand or contract all slopes by same amount
		...){
	
	# radius default 
	rad.def <- interval / 2
	
	# cell centroid
	xc      <- year + rad.def 
	yc      <- age + rad.def
	
	# now shrink in in case there's edge padding.
	rad.def <- rad.def - pad
	
	# effective slope:
	eslope  <- slope * lambda
	rad1    <- atan(eslope)
	x1      <- cos(rad1) * rad.def
	x2      <- -x1
	y1      <- sin(rad1) * rad.def
	y2      <- -y1
	
	# now move to centroid
	x1      <- xc + x1
	x2      <- xc + x2
	y1      <- yc + y1
	y2      <- yc + y2
	
	# and draw it
	segments(x1, y1, x2, y2, ...)
	
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