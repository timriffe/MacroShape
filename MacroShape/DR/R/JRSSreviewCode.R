# ----------------------------------------------
#install.packages("fda.usc")
library(fda.usc)
library(HMDHFDplus)
library(reshape2)
library(RColorBrewer)
SWE <- readHFDweb("SWE","asfrRR",username=us,password=pw)
fx  <- acast(SWE, Age~Year, value.var = "ASFR")
ages <- as.integer(rownames(fx))
years <- as.integer(colnames(fx))
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


SWE$Cohort <- SWE$Year - SWE$Age
matc <- acast(SWE, Age~Cohort, value.var = "ASFR")
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
image(t(deriv_1c))
image(t(deriv_2c))


# TODO:

# turn into Lexis field, where slope segments are either 
# 1) angle for given age, with arrowhead, length proportional to radius?
# 2) make angle that of tangent
# 3) make curled arrow instead over 3 ages? Gotta think on that one.
# 4) period/cohort problem?







# bezier curves not enough







plot(deriv_1[60,],deriv_2[60,],asp=1,type='b')

points(deriv_1[60,9],deriv_2[60,9],pch=16,col = "yellow")
points(deriv_1[60,10],deriv_2[60,10],pch=16,col = "red")
points(deriv_1[60,11],deriv_2[60,11],pch=16,col = "blue")

x <- deriv_1[60,9:11]
y <- deriv_2[60,9:11]
bez <- function(x, y, evaluation=100) {
	if(missing(y)) {
		y <- x[[2]]
		x <- x[[1]]
	}
	
	n <- length(x)
	X <- Y <- single(evaluation)
	Z <- seq(0, 1, length=evaluation)
	X[1] <- x[1];
	X[evaluation] <- x[n]
	Y[1] <- y[1];
	Y[evaluation] <- y[n]
	for(i in 2:(evaluation - 1)) {
		z <- Z[i]
		xz <- yz <- 0
		const <- (1 - z) ^ (n - 1)
		for(j in 0 : (n - 1)) {
			xz <- xz + const * x[j + 1]
			yz <- yz + const * y[j + 1]
			const <- const* (n - 1 - j) / (j + 1) * z / (1 - z)
			if(is.na(const)) prn(c(i, j, z))
		}
		
		X[i] <- xz; Y[i] <- yz
	}
	
	list(x=as.numeric(X), y=as.numeric(Y))
}

lines(as.numeric(X), as.numeric(Y))
a <- splinefun(deriv_2[60,9:11]~deriv_1[60,9:11])
d1 <- seq(deriv_1[60,11],deriv_1[60,9],length=100)
lines(d1, a(d1), col = "blue")


a <- splinefun(deriv_2[60,5:7]~deriv_1[60,5:7])
d1 <- seq(deriv_1[60,5],deriv_1[60,7],length=100)
lines(d1, a(d1), col = "blue")

bez <- function(A,B,C,w=2,ti=seq(0,1,length=100)){
	#ti <-seq(0,1,length=100)
	(1-ti)^w*A + 
			w*ti*(1-ti)*B + 
			ti^w*C
}

bez_min <- function(bw,A,B,C,w=2){
	(B - bez(A,B*bw,C,w=w,ti=.5))^2
}

bez3 <- function(x,y,n=100){
	bwx <- optimize(bez_min,lower=-100,upper=100,A=x[1],B=x[2],C=x[3])$minimum
	bwy <- optimize(bez_min,lower=-100,upper=100,A=y[1],B=y[2],C=y[3])$minimum 
	
	list(x = bez(A=x[1],B=bwx*x[2],C=x[3],2,ti=seq(0,1,length=n)),
		 y = bez(A=y[1],B=bwy*y[2],C=y[3],2,ti=seq(0,1,length=n)))
}

png("/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/phaseexample.png")
plot(deriv_1[60,],deriv_2[60,],asp=1,type='c',xlab = "d1", ylab = "d2")
text(deriv_1[60,],deriv_2[60,],12:55,cex=.7)
dev.off()


for (i in 2:43){
	lines(bez3(deriv_1[60,(i-1):(i+1)],deriv_2[60,(i-1):(i+1)]), col = "red")
}

lines(bez3(deriv_1[60,5:7],deriv_2[60,5:7]), col = "red")
lines(bez3(deriv_1[60,6:8],deriv_2[60,6:8]), col = "blue")
lines(bez3(deriv_1[60,7:9],deriv_2[60,7:9]), col = "blue")

curl1x1 <- function(x,y,a,p,cex=1,arr = TRUE,...){
	xy  <- bez3(x,y,100)
	xy$x <- (xy$x - x[2]) * cex + p + .5
	xy$y <- (xy$y - y[2]) * cex + a + .5
	lines(xy,...)
	if (arr){
		arrows(xy$x[99],xy$y[99],xy$x[100],xy$y[100], ...)
	}
}

colnames(deriv_1) <- ages
colnames(deriv_2) <- ages


pdf("/home/tim/git/MacroShape/MacroShape/DR/Proposal/Figures/LexisPhase.pdf",width=14,height=7)
image(1891:2014, 12:55, t(fx), asp = 1, breaks = my_breaks, col = my_ramp(length(my_breaks)-1),
		xlab = "", ylab = "Age", 
		main = "Swedish ASFR\nall parities combined, ages 12-55, years 1891-2014 (HFD)",
		sub = "filled contours are a standard Lexis surface\n black contours over age in period perspective, while blue contour is cohort perspective mode")
contour(1891:2014, 12:55, t(fx), breaks = seq(0,.24,by=.04), lwd = .5, col = gray(.5), add = TRUE)
agesi <- 13:54
yrsi  <- 1892:2013
for (a in agesi[agesi%%2 == 0]){
	for (p in yrsi[yrsi%%2 == 0]){
		ai <- as.character((a-1):(a+1))
		pt <- as.character(p)
		curl1x1(x = deriv_1[pt,ai], 
				y = deriv_2[pt,ai],
				a = a, p = p, cex = 200, lwd = .5, 
				length = .05)
	}
}
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

dev.off()




