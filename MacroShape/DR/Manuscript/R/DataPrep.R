

TF <- "HMDHFDplus" %in% rownames(installed.packages())
if(!TF){
  install.packages("HMDHFDPlus")
}
TF <- "DistributionTTD" %in% rownames(installed.packages())
if(!TF){
  library(devtools)
  install_github("timriffe/DistributionTTD", 
                 subdir = "/DistributionTTD/R/DistributionTTD") 
}
library(here)
library(HMDHFDplus)
library(data.table)
library(DistributionTTD)

download_HMD <- FALSE
if (download_HMD){
Countries <- getHMDcountries() # returns vector of HMD country codes

# us <- "HMD username" # change to yours: you need to be registered at www.mortality.org
# pw <- "HMD password" # change to yours
# makes long data frame of HMD lifetables, takes several minutes to run
LT <- do.call(rbind,lapply(Countries, function(XYZ, us, pw){
  # downloads just like yours, but fixes some column classes.
  cat(XYZ,"\n")
  LTM      <- readHMDweb(XYZ, "mltper_1x1", username = us, password = pw)   
  LTF      <- readHMDweb(XYZ, "fltper_1x1", username = us, password = pw) 
  LTM$Sex  <- "m"
  LTF$Sex  <- "f"
  LT       <- rbind(LTM, LTF)
  LT$CNTRY <- XYZ
  LT
}, us = us, pw = pw))
# order columns (no worries, Age is integer)
LT   <- LT[with(LT, order(CNTRY, Sex, Year, Age)),]
# convert to data.table

LT   <- data.table(LT)

# for 'Date Accessed' entry in HMD reference. 
# Re-Run this script for the final version to regenerate clean results on latest data.
attr(LT,"timestamp") <- Sys.Date()
# save out for repeated use
saveRDS(LT, file = here("Data","HMDltper.rds"))
}
# -----------------------------------------------------------------
# L-moment-related: "getB0b_ta", "getB1b_ta", "getB2b_ta", "getL2b_ta", 
#                   "getL3b_ta", "getLCV_ta", "getLSkew_ta"
# General distribution-related: "da2fya", "momentN", "getMedian", 
#                   "getMode", "getQuantile", "getSkewst", "getKurtst" 
# ------------------------------------------------------
LT <- readRDS(here("MacroShape","Data","HMDresults.rds"))

# These calcs might take a while.
LT[, Lskew := getLSkew_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
LT[, LCV   := getLCV_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, Lmad  := getB0b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, L2    := getL2b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, L3    := getL3b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
## standard skew and kurtosis (from stanardized moments)
LT[, Sskew := getSkewst(dx,ax),    by = list(CNTRY, Sex, Year)]
LT[, Var   := momentN(dx,2,ax),    by = list(CNTRY, Sex, Year)]
LT[, ex2   := getex(dx,ax),        by = list(CNTRY, Sex, Year)]
LT$CV <- sqrt(LT$Var) / LT$ex2 # CV quicker to calculate this way
LT[, Skurt := getKurtst(dx,ax),    by = list(CNTRY, Sex, Year)]
# last ones take a while because of MANY splines being fit...
LT[, q25   := getQuantile(dx,.25), by = list(CNTRY, Sex, Year)]
LT[, q50   := getQuantile(dx,.5),  by = list(CNTRY, Sex, Year)] # er, median
LT[, q75   := getQuantile(dx,.75), by = list(CNTRY, Sex, Year)]
LT[, Mode  := getMode(dx),         by = list(CNTRY, Sex, Year)]

saveRDS(LT, file = here("Data","HMDresults.rds"))
#head(LT)
#str(LT)
print(object.size(LT),units="Mb") # 185.1 Mb 
