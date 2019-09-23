
# Author: tim
###############################################################################

# contents:
# draw_field_element()
# get_brr()
# get_brr_dt()
# get_chunk()
# get_coefs()
# get_field()
# get_mod()
# get_slope()

# function to select subset:

# subset to Lexis square of interval N, where year and age 
# are lower bounds.
get_chunk <- function(year, age, sex = "m", N = 10, Dat){
	subset(Dat,Year >= year & 
	         Year < (year + N) & 
	         Age >= age & 
	         Age < (age + N) & 
	         Sex == sex)
}

# fits OLS to arbitrary data chunk, in our case a subset
# of age and year
get_mod <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk   <- data.frame(chunk)
	mod     <- lm(paste0(yvar,"~", xvar), data = chunk)
	mod
}

# fit OLS model to chunk and return 3 variables
# 1) slope coef, 2) r^2 3) IQR diag (box diagonal)
get_brr_dt <- function(chunk, xvar = "ex", yvar = "CV"){
	chunk   <- data.frame(chunk)
	iqrx    <- diff(quantile(chunk[, xvar],c(.25,.75)))
	iqry    <- diff(quantile(chunk[, yvar],c(.25,.75)))
	iqrdiag <- sqrt(iqrx^2 + iqry^2)
	mod     <- get_mod(chunk, xvar = xvar, yvar = yvar)
	data.frame(b = mod$coef[2], 
	           rsq = summary(mod)$r.s, 
	           iqrdiag = iqrdiag)
}


# Flexible pointer rendering,
# no curvature.
draw_field_element <- function(
		age = 0,       # lower bound of cell age
		year = 2000,   # lower bound of cell year
		interval = 5,  # cell dimension
		slope = 1,     # slope of regression or whatever
		length = 1,    # default meaning = interval - (2*pad)
		pad = .1,      # edge pad if length = 1 and slope = 0 or Inf
		lambda = 1,    # to expand or contract all slopes by same amount
		...){
	
	# radius default (zero pad)
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
	segments(x0 = x1, y0 = y1, x1 = x2, y1 = y2, ...)
	#segments(x1,y1,x2,y2)
	
}
