# ----------------------------------------------
#install.packages("fda.usc")
library(fda.usc)
library(HMDHFDplus)
library(reshape2)
library(RColorBrewer)
SWE <- readHFDweb("SWE","asfrRR",username=us,password=pw)
fx  <- acast(SWE, Age~Year, value.var = "ASFR")

get_di <- function(mat,which = "accel"){
	ages         <- as.integer(rownames(mat))
	years        <- as.integer(colnames(mat))
	
	this_fdata   <- fdata(t(mat), argvals = ages)
	
	if (which %in% c("accel","decel")){
		deriv_i      <- fdata.deriv(this_fdata, nderiv = 2)
	}
	if (which == "mode"){
		deriv_i      <- fdata.deriv(this_fdata, nderiv = 1)
	}
	
	# same for all three:
	a  <- contourLines(years,ages,deriv_i$data,levels=c(0))
	a0 <- lapply(a, function(aa,years){
				if (all(range(years) == range(aa$x)) ){
					thisdf <- data.frame(Year = aa$x, Age = aa$y)
					return(thisdf)
				} else {
					return(NULL)
				}
			},years)
	a0[sapply(a0, is.null)] <- NULL
	
	if (which == "mode"){
		return(a0[[1]])
	} 
	
	pick  <- unlist(lapply(a0,function(aa){
						mean(aa$Age)
					}))
	if (which == "accel"){
		return(a0[[which.min(pick)]])
	} 
	if (which == "decel"){
		return(a0[[which.max(pick)]])
	}
	
}
# --- 
# and cohort contours?
get_mode_coh <- function(mat){
	ages         <- as.integer(rownames(mat))
	years        <- as.integer(colnames(mat))
	
	long         <- melt(mat,varnames = c("Age","Year"), value.name = "ASFR") 
	
	# this will be off a bit, sorry
	long$Cohort  <- long$Year - long$Age
	
	# now we have an AC matrix
	matc         <- acast(long, Age ~ Cohort, value.var = "ASFR")
	
	
	deriv_1      <- apply(matc, 2, function(y, ages){
				f1  <- splinefun(x=ages,y=y, method = "fmm")
				out <- f1(ages, deriv = 1)
				out[is.na(y)]  <- NA
				out
			}, ages = ages)
	
	
	# same for all three:
	a <- contourLines(as.integer(colnames(deriv_1)), ages, t(deriv_1), levels = 0)
	lengths <- sapply(a,function(x){
				length(x$x)
			})
	model <- a[[which.max(lengths)]]
	out <- data.frame(Cohort = model$x, Age = model$y)
	# now move back to AP coords:
# bring back to period
	out$Year <- out$Cohort + out$Age
	return(out)
	
}


my_ramp   <- colorRampPalette(brewer.pal(9, "Greens"),space = "Lab")
my_breaks <- pretty(fx,n=15)

accel <- get_di(fx,"accel")
decel <- get_di(fx,"decel")
mode  <- get_di(fx,"mode")
modec <- get_mode_coh(fx)
MAC <- colSums(fx*ages)/colSums(fx) + .5
#png("/home/tim/workspace/Other/Figures/ReviewJRSS.png", width = 1600,height = 800)
image(1891:2014, 12:55, t(fx), asp = 1, breaks = my_breaks, col = my_ramp(length(my_breaks)-1),
		xlab = "", ylab = "Age", 
		main = "Swedish ASFR\nall parities combined, ages 12-55, years 1891-2014 (HFD)",
		sub = "filled contours are a standard Lexis surface\n black contours over age in period perspective, while blue contour is cohort perspective mode")
contour(1891:2014, 12:55, t(fx), breaks = seq(0,.24,by=.04), lwd = .5, col = gray(.5), add = TRUE)
lines(mode, lwd = 2)
lines(accel)
lines(decel)

lines(years, MAC, col = "magenta")
text(1900,MAC[years == 1900], "MAC", col = "magenta",cex = 1.2,pos = 3)

text(mode[50, ], "mode", pos = 3, cex = 1.2)
text(accel[50, ], "maximum acceleration", pos = 1, cex = 1.2)
text(decel[50, ], "maximum deceleration", pos = 3, cex = 1.2)



# what would a cohort model look like imposed on the AP surf?
lines(modec$Year+.5, modec$Age, col = "blue", lwd =2)
text(1920,38,"cohort mode", cex = 1.2, col = "blue")
#dev.off()


this_fdata   <- fdata(t(fx), argvals = ages)
deriv_1      <- fdata.deriv(this_fdata, nderiv = 1)$data
deriv_2      <- fdata.deriv(this_fdata, nderiv = 2)$data

image(deriv_2)
image(deriv_1)
range(deriv_1)
hot_cold <- colorRampPalette(brewer.pal(11,"RdBu"),space="Lab")
hcbreaks <- seq(-.03,.03,by=.0025)

#png("/home/tim/workspace/Other/Figures/ReviewJRSS2.png", width = 1600,height = 800)
image(1891:2014, 12:55, deriv_1, asp = 1, breaks = hcbreaks, col = rev(hot_cold(length(hcbreaks)-1)),
		xlab = "", ylab = "Age", 
		mai = "Swedish ASFR first derivatives\nall parities combined, ages 12-55, years 1891-2014 (HFD)",
		sub = "filled contours are a standard Lexis surface\n black contours over age in period perspective, while blue contour is cohort perspective mode")

contour(1891:2014, 12:55, t(fx), breaks = seq(0,.24,by=.04), lwd = .5, col = gray(.5), add = TRUE)

lines(mode, lwd = 2)
lines(accel)
lines(decel)

lines(years, MAC, col = "magenta")
text(1900,MAC[years == 1900], "MAC", col = "magenta",cex = 1.2,pos = 3)

text(mode[50, ], "mode", pos = 3, cex = 1.2)
text(accel[50, ], "maximum acceleration", pos = 1, cex = 1.2)
text(decel[50, ], "maximum deceleration", pos = 3, cex = 1.2)

#dev.off()

#deriv_1
#deriv_2
#
#plot(deriv_1[1,],deriv_2[1,])

# get radian expansion factor to move to unit circle
radf                      <- 1 / sqrt(deriv_1^2 + deriv_2^2)
# and rescale coords
x1                        <- deriv_1 * radf
y1                        <- deriv_2 * radf
# now get radian translation
rad                       <- atan2(y1,x1)
# now how do we map to color? Use unit circle angle
# to map to hue, and magnitude (uncalsed radius?) to opacity?
# hard to say what makes sense.

radm    <- radp <- rad
radp[radp <= 0] <- NA
radm[radm >= 0] <- NA
#png("/home/tim/workspace/Other/Figures/ReviewJRSS3.png", width = 1600,height = 800)
contour(1891:2014, 12:55,
		radp, 
		asp = 1, 
		col = "#FF0000A0", 
		levels = seq(0,pi,by=pi/9), 
		drawlabels=FALSE, las = 1)
contour(1891:2014, 12:55,radm,
		asp=1, 
		col = "#0000FFA0", 
		add = TRUE, 
		levels = seq(-pi,0,by=pi/9), 
		drawlabels=FALSE)
lines(mode, lwd = 2)
lines(accel, lwd = 2)
lines(decel, lwd = 2)
text(mode[50, ], "mode", pos = 3, cex = 1.2)
text(accel[50, ], "maximum acceleration", pos = 1, cex = 1.2)
text(decel[50, ], "maximum deceleration", pos = 3, cex = 1.2)
#dev.off()


# phase plot in cohort dim
deriv_1c      <- apply(matc, 2, function(y, ages){
			f1  <- splinefun(x=ages,y=y, method = "fmm")
			out <- f1(ages, deriv = 1)
			out[is.na(y)]  <- NA
			out
		}, ages = ages)
deriv_2c      <- apply(matc, 2, function(y, ages){
			f1  <- splinefun(x=ages,y=y, method = "fmm")
			out <- f1(ages, deriv = 2)
			out[is.na(y)]  <- NA
			out
		}, ages = ages)
#plot(deriv_1c[, 80], deriv_2c[, 80 ],type='l')

# TODO:

# turn into Lexis field, where slope segments are either 
# 1) angle for given age, with arrowhead, length proportional to radius?
# 2) make angle that of tangent
# 3) make curled arrow instead over 3 ages? Gotta think on that one.
# 4) period/cohort problem?