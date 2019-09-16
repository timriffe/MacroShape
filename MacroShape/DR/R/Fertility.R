
# Author: tim
###############################################################################

setwd("/home/tim/git/MacroShape/MacroShape/DR")

data.path <- "/home/tim/Data/Dudel/ASFRs"

library(data.table)
library(reshape2)
sopurce("R/LexisFieldFunctions.R")

Country <- "Sweden"

# read males
file.i      <- paste0("ASFR_",Country,"_m.txt")
path.i      <- file.path(data.path,file.i)
M           <- read.table(path.i,sep="\t",header=TRUE)
# read females
file.i      <- paste0("ASFR_",Country,"_f.txt")
path.i      <- file.path(data.path,file.i)
FF          <- read.table(path.i,sep="\t",header=TRUE)


# cut ages to 15-50
M           <- M[M$Age <= 50, ]

# 


M    <- as.data.table(M)
FF <- as.data.table(FF)

# rename fert columns
setnames(M,"ASFR","ASFRm")
setnames(FF,"ASFR","ASFRf")

# merge
DAT       <- FF
DAT$ASFRm <- M$ASFRm
Data      <- as.data.table(DAT)
# log(F/M)
DAT$logFM <- log(DAT$ASFRf/DAT$ASFRm)
DAT$logFM[is.infinite(DAT$logFM)] <- NA

ASFRf  <- acast(DAT, Age~Year, value.var = "ASFRf")
logFM  <- acast(DAT, Age~Year, value.var = "logFM")

# bound the cells: an extra age/yr at end
age    <- 15:51
yrs    <- 1968:2016
#ramp1  <- colorRampPalette(brewer.pal(9,"Greens"),space="Lab")

breaks <- seq(0,.17,by=.01)
sequential_hcl(17, "BurgYl")
ASFRf[ASFRf==0] <- NA

pdf("PAA/FertilityFM.pdf",width=8.717660,height= 7.466169)
# heat map
image(yrs,age,t(ASFRf),
		asp=1,breaks=breaks,
		col=sequential_hcl(length(breaks)-1, 
				"BurgYl", rev=TRUE),
		las=1, xlab="", ylab="")
# labelled contours, redundant with color
contour(1968.5:2015.5,15.5:50.5,t(ASFRf),add=TRUE, labcex=.6,col="#00000070")

# add single contour for M==F
contour(1968.5:2015.5,15.5:50.5,t(logFM),add=TRUE, labcex=1,col="#FFFFFFAA",lwd=2,levels=0,drawlabels=FALSE)

# draw field
pad <- .3
gs  <- 2
for (a in seq(15,49,by=gs)){
	for (y in seq(1968,2015,by=gs)){
		mr <- mean(DAT[Age >= a & Age < (a+gs) &
								Year >= y & Year < (y+gs) &
								Country == "Sweden"]$logFM)
		draw_vector(slope = mr, len = gs-(pad*2), year=y,age=a,N=gs,col="#FFFFFF90")
	}
}

# end
dev.off()
.69 * 100





## To be deprecated
#
#files <- list.files(data.path)
#
#XYZ <- unique(unlist(lapply(strsplit(split="_",files),function(x){
#							x[2]
#						})))
#
## year ranges
#YR <- sapply(XYZ,function(xyz){
#			file.i <- paste0("ASFR_",xyz,"_m.txt")
#			path.i <- file.path(path,file.i)
#			A      <- read.table(path.i,sep="\t",header=TRUE)
#			range(A$Year)
#		})
#
#
#ord <- order(diff(YR))
#YR <- YR[,ord]
#XYZ <- XYZ[ord]
#par(mai=c(1,.1,.1,1.5))
#plot(NULL, type = "n", xlim=c(1968,2016),ylim=c(0,length(XYZ)+1),axes=FALSE, ann = FALSE)
#text(2016,1:length(XYZ),XYZ,pos=4,xpd=TRUE)
#segments(YR[1, ],1:length(XYZ),YR[2,],1:length(XYZ))
#points(YR[1, ],1:length(XYZ), pch = 16)
#points(YR[2, ],1:length(XYZ), pch = 16)
#axis(1)
#
#ly <- 1975
#ry <- 2014
#
#keep <- c("Australia","Spain","Canada","Hungary","USA","Sweden")
#
#Males <- do.call("rbind",lapply(keep, function(xyz,ly,ry){
#					file.i     <- paste0("ASFR_",xyz,"_m.txt")
#					path.i     <- file.path(path,file.i)
#					A          <- read.table(path.i,sep="\t",header=TRUE)
#					A          <- A[A$Year >= ly & A$Year <= ry, ]
#					A$Country  <- xyz
#					A$Sex      <- "M"
#					A
#				},ly=ly,ry=ry))
#
#Females <- do.call("rbind",lapply(keep, function(xyz,ly,ry){
#					file.i <- paste0("ASFR_",xyz,"_f.txt")
#					path.i <- file.path(path,file.i)
#					A      <- read.table(path.i,sep="\t",header=TRUE)
#					A <- A[A$Year >= ly & A$Year <= ry, ]
#					A$Country <- xyz
#					A$Sex <- "F"
#					A
#				},ly=ly,ry=ry))
#
#
#
## Compare ages 15-49 strictly
#
#Females <- Females[Females$Country %in% keep & Females$Age < 50, ]
#Males   <- Males[Males$Country %in% keep & Males$Age < 50, ]
#
#max(Males$ASFR)
#head(Males)
#
#
#library(reshape2)
#age   <- 15:49
#yrs   <- 1975:2014
#ASFRM <- acast(Males[Males$Country == "Hungary",], Age~Year, value.var = "ASFR")
#ASFRF <- acast(Females[Females$Country == "Hungary",], Age~Year, value.var = "ASFR")
#
#
#
#library(RColorBrewer)
#ramp1 <- colorRampPalette(brewer.pal(9,"Greens"),space="Lab")
#breaks <- seq(0,.21,by=.01)
#image(yrs,age,t(ASFRM),asp=1,breaks=breaks,col=ramp1(length(breaks)-1))
#image(yrs,age,t(ASFRF),asp=1,breaks=breaks,col=ramp1(length(breaks)-1))
#
#
#LR <- log(ASFRF / ASFRM)
#LR[is.infinite(LR)] <- NA
#
#LR[LR < -2] <- -2
#LR[LR > 2] <- 2
#range(LR,na.rm=TRUE)
#breaksl <- seq(-2,2,by=.1)
#ramp2   <- colorRampPalette(rev(brewer.pal(11,"RdBu")),space="Lab")
#
#image(yrs,age,t(LR),asp=1,breaks=breaksl,col=ramp2(length(breaksl)-1))
#contour(yrs,age,t(LR),asp=1,breaks=breaksl,add=TRUE)
#
#library(data.table)
#Males   <- as.data.table(Males)
#Females <- as.data.table(Females)
#
#setnames(Males,"ASFR","ASFRm")
#setnames(Females,"ASFR","ASFRf")
#
#DAT       <- Females
#DAT$ASFRm <- Males$ASFRm
#rc <- function(x){
#	rev(cumsum(rev(x)))
#}
#DAT <- DAT[,list(ASFRf = identity(ASFRf),
#				ASFRm = identity(ASFRm),
#				CFRf = cumsum(ASFRf), 
#				CFRm = cumsum(ASFRm), 
#				FCFRf = rc(ASFRf), 
#				FCFRm = rc(ASFRm), 
#				logFM = log(ASFRf/ASFRm),
#				Age = identity(Age)),
#		by = list(Country, Year)]
#DAT$logFM[is.infinite(DAT$logFM)] <- NA
#
#
#plot(NULL, type= "n", xlim = c(1975,2015),ylim=c(15,50), asp = 1)
#for (xyz in keep){
#	LR <- ASFRF <- acast(DAT[DAT$Country == xyz,], Age~Year, value.var = "logFM")
#	contour(yrs,age,t(LR),asp=1,level=c(0),add=TRUE, labels = "")
#}
#
#
#matplot(age,acast(DAT, Age~Year+Country, value.var = "CFRf"), type='l', col = "#00000050",lty=1)
#matplot(age,acast(DAT, Age~Year+Country, value.var = "CFRm"), type='l', col = "#00000050",lty=1)
#
#plot(acast(DAT, Age~Year+Country, value.var = "CFRf"),
#		acast(DAT, Age~Year+Country, value.var = "FCFRm"),
#		asp=1, pch=16, col = "#00000050")
#
#image(yrs,age,t(LR),asp=1,breaks=breaksl,col=ramp2(length(breaksl)-1))
#
## try base ASFR with arrows for F~M relationshio
#
#
#ASFRf  <- acast(Females[Females$Country == "Sweden",], Age~Year, value.var = "ASFRf")
#age    <- 15:49
#yrs    <- 1975:2014
#ramp1  <- colorRampPalette(brewer.pal(9,"Greens"),space="Lab")
#breaks <- seq(0,.21,by=.01)
#image(yrs,age,t(ASFRf),asp=1,breaks=breaks,col=ramp1(length(breaks)-1))
#
#
#pad <- .25
#gs  <- 2
#for (a in seq(15,44,by=gs)){
#	for (y in seq(1975,2014,by=gs)){
#		mr <- mean(DAT[Age >= a & Age < (a+gs) &
#								Year >= y & Year < (y+gs) &
#								Country == "Sweden"]$logFM)
#		
#		
##		xm <- y + 1
##		ym <- a + 1
##		
##		xl <- y + pad
##		xr <- y + gs - pad
##		
##		yd <- sl * (gs-pad*2)
##		segments(xl,ym-yd/2,xr,ym+yd/2)
#		draw_vector(slope = mr, len = 1.8, year=y,age=a,N=2)
#	}
#}
