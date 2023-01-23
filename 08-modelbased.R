#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 8: Model-based methods
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


## ----robstarsmm, message=FALSE, warning=FALSE, echo=FALSE----------------------------------------------------------
library(robustbase)
lmrob1 <- lmrob(log.light ~ log.Te, data = starsCYG)


data(ethanol, package = "lattice")
ethanol_na <- ethanol
set.seed(12) 
ethanol_na$NOx[sample(1:nrow(ethanol_na), 10)] <- NA
par(mfrow = c(1,2), mar = c(4,4,1.5,0.5))
imp <- mice(ethanol_na, printFlag = F, m = 1, method = "pmm") 
imp <- complete(imp)
plot(NOx ~ E, data = ethanol_na)
w <- is.na(ethanol_na$NOx)
points(imp$E[w], imp$NOx[w], col = "red", pch = 20, cex = 1.5) 
points(ethanol$E[w], ethanol$NOx[w],
       col = "grey", pch = 20, cex = 1.5) 
segments(x0 = ethanol$E[w], x1 = ethanol$E[w],
         y0 = ethanol$NOx[w], y1 = imp$NOx[w], col = "grey") 
mtext("mice::mice (PMM)")
imp <- mice(ethanol_na, printFlag = FALSE, m = 1,method = "midastouch") 
imp <- complete(imp)
plot(NOx ~ E, data = ethanol_na)
w <- is.na(ethanol_na$NOx)
points(imp$E[w], imp$NOx[w], col = "red", pch = 20, cex = 1.5) 
points(ethanol$E[w], ethanol$NOx[w],
       col = "grey", pch = 20, cex = 1.5) 
segments(x0 = ethanol$E[w], x1 = ethanol$E[w],
         y0 = ethanol$NOx[w], y1 = imp$NOx[w], col = "grey") 
mtext("mice::mice (midastouch)")

## explanations on mice::mice(..., method = "midastouch)
## can be found in the book

## explanations on VIM::irmi
## can be found in the book