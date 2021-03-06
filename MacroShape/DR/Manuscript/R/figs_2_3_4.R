#######################################################################

Dat          <- readRDS(here("Data","HMDresults.rds"))

# sqrt gives us standard deviation
Dat$SD       <- sqrt(Dat$Var)

# Here only select every 5th single age up to 100.
Dat          <- Dat[Dat$Age %% 5 == 0 & Dat$Age < 105, ]

# years after and including 1950
Dat          <- Dat[Dat$Year >= 1950, ]

# Age and year bins used for selection
Dat$Year5    <- Dat$Year - Dat$Year %% 5
Dat$Age5     <- Dat$Age - Dat$Age %% 5

# data.table for quick calcs
Dat          <- data.table(Dat)

# get values required to construct field:
DatMod       <- Dat[, 
                    get_brr_dt(.SD, xvar="ex", yvar = "SD"), 
                    by = list(Sex, Year5, Age)]


# no need to scale b: 45 degrees = 1:1
Fem    <- DatMod[DatMod$Sex == "f"]


# global dimension parameters
xr     <- 65   # width in years, used for pdf dims
yr     <- 105  # height in years, used for pdf dims
scalef <- .1
width  <- xr * scalef + .4 # .4 inch total margins 
height <- yr * scalef + .4

# ----------------------------------------
cat("creating figure 2\n")
# Figure 2
pdf(here("Figures","Fig2.pdf"), width = width, height = height)
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
cat("creating figure 3\n")
pdf(here("Figures","Fig3.pdf"),width=width,height=height)
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
cat("creating figure 4\n")

Dat          <- readRDS(here("Data","HMDresults.rds"))
Dat          <- Dat[Dat$Age < 105]
Dat          <- Dat[Dat$Year >= 1950]
Dat          <- data.table(Dat)

Datlx        <- Dat[, mean(lx/1e5), by = list(Sex, Year, Age)]
lx           <- acast(Datlx[Sex == "f"], Age~Year,value.var ="V1")
breaks       <- pretty(lx, 25)

cols         <- sequential_hcl(length(breaks)-1,"PinkYl")

pdf(here("Figures", "Fig4.pdf"), 
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

cat("See figures in Figures/ folder\nAll done.")

# end