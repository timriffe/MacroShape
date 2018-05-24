
# Author: tim
###############################################################################


# function to select subset:

# N = cell size
get_chunk <- function(year, age, sex = "m", N = 10, Dat){
	ind  <- Dat$Year >= year & Dat$Year < (year + N) &
			Dat$Age >= age & Dat$Age < (age + N) & 
			#Dat$Age == age &
			Dat$Sex == sex
	Dat[ind, ]
}

get_mod <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk   <- data.frame(chunk)
	#xrange  <- range(chunk[,xvar], na.rm = TRUE)
	mod     <- lm(paste0(yvar,"~", xvar), data = chunk)
	#xout    <- seq(xrange[1], xrange[2], length = 20)
	#newdata <- data.frame(xout)
	#colnames(newdata) <- xvar
	#out     <- predict(mod,newdata,level=.95, interval = "prediction")
	#cbind(newdata,out)
	mod
}
get_coefs <- function(chunk, xvar = "ex", yvar = "CV"){
	mod <- get_mod(chunk, xvar = xvar, yvar = yvar)
	mod$coef
}
# just to get b and r square
# and xrange
get_brr <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk <- data.frame(chunk)
	xr    <- diff(range(chunk[, xvar]))
	mod   <- get_mod(chunk, xvar = xvar, yvar = yvar)
	c(b = mod$coef[2], rsq = summary(mod)$r.s, xrange = xr)
}

get_brr_dt <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk <- data.frame(chunk)
	xr    <- diff(range(chunk[, xvar]))
	iqrx   <- diff(quantile(chunk[, xvar],c(.25,.75)))
	iqry   <- diff(quantile(chunk[, yvar],c(.25,.75)))
	iqrdiag <- sqrt(iqrx^2 + iqry^2)
	mod   <- get_mod(chunk, xvar = xvar, yvar = yvar)
	rp    <- cor(chunk[,xvar],chunk[,yvar], method = "pearson")
	# total least square version:
	v     <- prcomp(chunk[,c(xvar,yvar)],rank. = 1)$rotation
	beta  <- v[-ncol(v),ncol(v)] / v[ncol(v),ncol(v)]
	data.frame(b=mod$coef[2], rsq = summary(mod)$r.s, xrange = xr, iqrx = iqrx, iqrdiag = iqrdiag,rp = rp, tlsb = beta)
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
	rad.def <- (interval / 2)
	
	# cell centroid
	xc      <- year + rad.def 
	yc      <- age + rad.def
	
	# now shrink in in case there's edge padding.
	rad.def <- rad.def * length - pad
	
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
	segments(x0=x1, y0=y1, x1=x2, y1=y2,...)
	#segments(x1,y1,x2,y2)
	
}
