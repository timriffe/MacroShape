#######################################################################
library(data.table)
library(reshape2)
library(here)
library(RColorBrewer)
library(colorspace)
# hcl_palettes(plot=TRUE)
rm(list = ls(all.names = TRUE))
source(here("R","ApplicationFunctions.R"))

Dat          <- readRDS(here("Data","HMDresults.rds"))

Dat$SD       <- sqrt(Dat$Var)

Dat          <- Dat[Dat$Age %% 5 == 0 & Dat$Age < 105, ]

Dat          <- Dat[Dat$Year >= 1950, ]
Dat$Year5    <- Dat$Year - Dat$Year %% 5
Dat$Age5     <- Dat$Age - Dat$Age %% 5

Dat          <- data.table(Dat)
DatMod       <- Dat[, 
                    get_brr_dt(.SD, xvar="ex", yvar = "SD"), 
                    by = list(Sex, Year5, Age)]


# no need to scale b: 45 degrees = 1:1
Fem    <- DatMod[DatMod$Sex == "f"]
Mal    <- DatMod[DatMod$Sex == "m"]

xr     <- 65   # width in years, used for pdf dims
yr     <- 105  # height in years, used for pdf dims
scalef <- .1
width  <- xr * scalef + .4 # .4 inch total margins 
height <- yr * scalef + .4


pdf(here("Figures","FigApp1.pdf"), width = width, height = height)
par(mai=c(.3,.4,.1,0))
plot(NULL, type = "n", xlim = c(1950,2020), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2020,105,border = NA,col = gray(.92)),
				segments(seq(1950,2020,by=5),0,seq(1950,2020,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2020,seq(0,105,by=5),col="white"),
				text(seq(1950,2015,by=10),0,seq(1950,2015,by=10),pos=1,xpd=TRUE,cex=1.2),
				text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1.2)))
#plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1)
for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = 5,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = 1, # default meaning = interval - (2*pad)
			pad = .5,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  1)
}
text(1944, 55, "Age",srt = 90, cex = 1.5, xpd=TRUE)
text(1985,-5,"Year",cex = 1.5, xpd=TRUE)
dev.off()



# ----------------------------------------
# Figure 3
grayrange <- c(0,.7)

pdf(here("Figures","FigApp2.pdf"),width=width,height=height)
par(mai=c(.3,.4,.1,0))
plot(NULL, type = "n", xlim = c(1950,2020), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2020,105,border = NA,col = gray(.92)),
				segments(seq(1950,2020,by=5),0,seq(1950,2020,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2020,seq(0,105,by=5),col="white"),
				text(seq(1950,2015,by=10),0,seq(1950,2015,by=10),pos=1,xpd=TRUE,cex=1.2),
                text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1.2)))
for (i in 1:nrow(Fem)){
	x <- Fem[i,]
	draw_field_element(
			age = x$Age,    # lower bound of cell age
			year = x$Year5,   # lower bound of cell year
			interval = 5,  # cell dimension
			slope = x$b,  # slope of regression or whatever
			length = x$iqrdiag/3, # default meaning = interval - (2*pad)
			pad = .05,   # edge pad if length = 1 and slope = 0 or Inf
			lambda =  2,
			col = gray(grayrange[2]-abs(x$rsq)*diff(grayrange)+grayrange[1]),
			lwd = .5 + 2*abs(x$rsq),
			xpd=TRUE)
}
text(1944, 55, "Age",srt = 90, cex = 1.5, xpd=TRUE)
text(1985,-5,"Year",cex = 1.5, xpd=TRUE)
dev.off()

# ----------------------------------------
# Figure 4
# what about where the background is the mean CV?
Dat          <- readRDS(here("Data","HMDresults.rds"))
Dat          <- Dat[Dat$Age < 105]
Dat          <- Dat[Dat$Year >= 1950]
Dat          <- data.table(Dat)

# Datdx        <- Dat[, mean(dx/1e5), by = list(Sex, Year, Age)]
# dx           <- acast(Datdx[Sex == "f"], Age~Year,value.var ="V1")
# breaks       <- pretty(dx, 25)

Datlx        <- Dat[, mean(lx/1e5), by = list(Sex, Year, Age)]
lx           <- acast(Datlx[Sex == "f"], Age~Year,value.var ="V1")
breaks       <- pretty(lx, 25)
# DatCV        <- Dat[, mean(CV), by = list(Sex, Year, Age)]
# CV           <- acast(DatCV[Sex == "f" & Year < 2013], Age~Year,value.var ="V1")
# breaks       <- pretty(CV, 25)

#ramp         <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")), space = "Lab")
cols         <- sequential_hcl(length(breaks)-1,"PinkYl")

pdf(here("Figures", "FigApp3.pdf"), 
    width = width, 
    height = height)
par(mai=c(.3,.4,.1,0))
plot(NULL, type = "n", xlim = c(1950,2020), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
		panel.first = list(
				rect(1950,0,2020,105,border = NA,col = gray(.92)),
				segments(seq(1950,2020,by=5),0,seq(1950,2020,by=5),105,col="white"),
				segments(1950,seq(0,105,by=5),2015,seq(0,105,by=5),col="white"),
				text(seq(1950,2015,by=10),0,seq(1950,2015,by=10),pos=1,xpd=TRUE,cex=1.2),
				text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1.2)))
image(1950.5:2018.5, .5:104.5,
		 t(lx),
		 breaks = breaks,
		 col = cols,
		 xlim=c(1950,2018),
		 ylim=c(0,105),
		 asp=1,
		 add=TRUE)
contour(1950.5:2018.5, .5:104.5,
		t(lx),
		breaks = pretty(lx,10),
		xlim=c(1950,2018),
		ylim=c(0,105),
		asp=1,
		add=TRUE,
		labcex = 1)
for (i in 1:nrow(Fem)){
  x <- Fem[i,]
  draw_field_element(
    age = x$Age,    # lower bound of cell age
    year = x$Year5,   # lower bound of cell year
    interval = 5,  # cell dimension
    slope = x$b,  # slope of regression or whatever
    length = x$iqrdiag/3, # default meaning = interval - (2*pad)
    pad = .05,   # edge pad if length = 1 and slope = 0 or Inf
    lambda =  2,
    col = gray(grayrange[2]-abs(x$rsq)*diff(grayrange)+grayrange[1]),
    lwd = .5 + 2*abs(x$rsq),
    xpd=TRUE)
}
text(1944, 55, "Age",srt = 90, cex = 1.5, xpd=TRUE)
text(1985,-5,"Year",cex = 1.5, xpd=TRUE)
dev.off()

# ----------------------------------------
# Deprecated
# ----------------------------------------
# 
# pdf(here("Figures","FigApp2.pdf"), width = width, height = height)
# par(mai=c(.3,.3,.1,.1))
# plot(NULL, type = "n", xlim = c(1950,2020), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
#      panel.first = list(
#        rect(1950,0,2020,105,border = NA,col = gray(.92)),
#        segments(seq(1950,2020,by=5),0,seq(1950,2020,by=5),105,col="white"),
#        segments(1950,seq(0,105,by=5),2020,seq(0,105,by=5),col="white"),
#        text(seq(1950,2015,by=10),0,seq(1950,2015,by=10),pos=1,xpd=TRUE,cex=1.2),
#        text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1.2)))
# #plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1)
# for (i in 1:nrow(Fem)){
#   x <- Fem[i,]
#   draw_field_element(
#     age = x$Age,    # lower bound of cell age
#     year = x$Year5,   # lower bound of cell year
#     interval = 5,  # cell dimension
#     slope = x$b,  # slope of regression or whatever
#     length = abs(x$rp), # default meaning = interval - (2*pad)
#     pad = 0,   # edge pad if length = 1 and slope = 0 or Inf
#     lambda =  1)
# }
# dev.off()



# ------------------------------------------------------
# Deprecated
# ------------------------------------------------------
# # experiment to see how fine the grid can be
# Dat          <- data.table(Dat) 
# Dat$SD       <- sqrt(Dat$Var)
# DatMod       <- Dat[,get_brr_dt(.SD,xvar="ex",yvar="SD"),by=list(Sex, Year, Age)]
# DatMod       <- data.frame(DatMod)
# Fem          <- DatMod[DatMod$Sex == "f", ]

# par(mai=c(.3,.3,.1,.1))
# plot(NULL, type = "n", xlim = c(1950,2015), ylim = c(0,105), axes = FALSE, xlab = "", ylab = "", asp = 1, 
# 		panel.first = list(
# 				rect(1950,0,2015,105,border = NA,col = gray(.92)),
# 				segments(seq(1950,2015,by=5),0,seq(1950,2015,by=5),105,col="white"),
# 				segments(1950,seq(0,105,by=5),2015,seq(0,105,by=5),col="white"),
# 				text(seq(1950,2010,by=10),0,seq(1950,2010,by=10),pos=1,xpd=TRUE,cex=1.2),
# 				text(1950,seq(0,100,by=10),seq(0,100,by=10),pos=2,xpd=TRUE,cex=1.2)))
# # optional background- more crowded out here
# #image(1950.5:2012.5, .5:104.5,
# #		t(CV),
# #		breaks = breaks,
# #		col = ramp(length(breaks)-1),
# #		xlim=c(1950,2013),
# #		ylim=c(0,105),
# #		asp=1,
# #		add=TRUE)
# #contour(1950.5:2012.5, .5:104.5,
# #		t(CV),
# #		breaks = pretty(CV,10),
# #		xlim=c(1950,2013),
# #		ylim=c(0,105),
# #		asp=1,
# #		add=TRUE)
# Int <- 2# so that segment midpoint crosses cell centroid, even though the cell size for rendering
# # has been expanded.
# for (i in 1:nrow(Fem)){
# 	x <- Fem[i,]
# 	draw_field_element(
# 			age = x$Age - (Int-1)/2,     # lower bound of implied cell age
# 			year = x$Year - (Int-1)/2,   # lower bound of implied cell year
# 			interval = Int,  # cell dimension
# 			slope = x$b,  # slope of regression or whatever
# 			length = x$iqr/3, # default meaning = interval - (2*pad)
# 			pad = 0,   # edge pad if length = 1 and slope = 0 or Inf
# 			lambda =  2,
# 			col = gray(grayrange[2]-x$rsq*diff(grayrange)+grayrange[1]),
# 			lwd = .5 + x$rsq,
# 			xpd=TRUE)
# }
#c(0.872,     0.829,     0.432,     0.286)
#
#sum(cumprod(c(0.872,     0.829,     0.432,     0.286)))
#
#barplot(cumprod(c(0.872,     0.829,     0.432,     0.286)))
