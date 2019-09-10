# Author: tim
###############################################################################

Dat <- readRDS(here("MacroShape","Data","HMDresults.rds"))

Dat$SD       <- sqrt(Dat$Var)
source(here("MacroShape","DR","R","LexisFieldFuncions.R"))

# for JM
#source("C:/Users/jmaburto/Documents/GitHub/MacroShape/MacroShape/R/LexisFieldFunctions.R")

library(RColorBrewer)
library(data.table)
library(reshape2)
# ------------------------------------
# a slicker way to calculate these slopes


Dat       <- Dat[Dat$Age %% 5 == 0, ]

years     <- seq(1950, 2010,by = 5)
ages      <- seq(0, 100, by = 5)

ll <- get_vector_field(years, 
		         ages, 
				 sex = "m", 
				 N = 5, 
				 Dat, 
				 xvar = "ex", 
				 yvar = "SD", 
				 lvar = "xrange")
hist(ll$len)
ll$len <- ll$len / 4

par(xaxs = 'i', yaxs = 'i',mai=c(.8,.8,.2,.2))
plot(NULL, 
		type = "n", 
		xlim = c(1950, 2015),
		ylim = c(0,100),
		xlab = "", ylab = "",
		#main = list("Slope in Life expectancy by SD at different ages",cex=1.5),
		asp = 1,
		las = 1
)
draw_vector_field(ll,headlength=.06)

