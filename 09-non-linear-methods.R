#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 9: Non-linear methods
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3



## ----treeexample, fig.cap='Left: simple decision tree for the Animals data. Right: split regions and group average shown with arrows. Since this a bit tricky because of log-transformed data, the code is shown as well.', tidy=TRUE, echo=TRUE, fig.pos='!htp', tidy=FALSE----
data(Animals, package = "MASS")
library(rpart)
library(rattle)
mytree <- rpart(log(brain) ~ log(body), data = Animals)
par(mfrow=c(1,2), mar = c(4,4,2,0.2))
fancyRpartPlot(mytree)
options(scipen = 15)
plot(Animals, log = "xy")
abline(v = c(exp(1.6),exp(4.1)))
arrows(x0 = exp(1.6), x1 = 0.02, y1 = exp(1.3), 
       y0 = exp(1.3), col = "red")
arrows(x0 = exp(4.1), x1 = 0.02, y1 = exp(5), 
       y0 = exp(5), col = "red", lty = 2)
segments(x0 = exp(4.1), x1 = exp(1.6), y1 = exp(5), 
         y0 = exp(5), col = "red", lty = 1)
arrows(x0 = 1000000, x1 = 0.02, y1 = exp(6.1), 
       y0 = exp(6.1), col = "red", lty = 2)
segments(x0 = 1000000, x1 = exp(4.1), y1 = exp(6.1), 
         y0 = exp(6.1), col = "red", lty = 1)



## ----load_Animals_na, message=FALSE, warning=FALSE, echo=TRUE, eval=FALSE------------------------------------------
data(Animals_na, package = "VIM")


## ----missForestimp, message=FALSE, warning=FALSE, tidy=FALSE-------------------------------------------------------
library(missRanger)
animals_imp_rf <- missRanger(Animals_na, verbose = 0)
# equivalent:
# animals_imp_rf <- VIM::rangerImpute(lbrain ~ lbody, 
#   data = Animals_na, imp_var = FALSE)


## ----mixgbimp, message=FALSE, warning=FALSE, tidy=FALSE------------------------------------------------------------
library(mixgb)
animals_imp_rf <- mixgb(Animals_na, m = 5)



## ----irisall, echo=FALSE, eval=TRUE, cache=FALSE, results='hide', message=FALSE, warning=FALSE---------------------
library(VIM) 
library(mice) 
library(missForest)
library(missRanger)
data(iris) 
iris_orig <- iris_imp <- iris
set.seed(122)
iris_imp$Sepal.Length[sample(1:nrow(iris), 10)] <- NA
iris_imp$Sepal.Width[sample(1:nrow(iris), 10)] <- NA
iris_imp$Petal.Width[sample(1:nrow(iris), 10)] <- NA
iris_imp$Species[sample(1:nrow(iris), 10)] <- NA
m <- is.na(iris_imp) 
iris_imp_kNN <- kNN(iris_imp, imp_var = FALSE)
iris_imp_irmi <- irmi(iris_imp, imp_var = FALSE, robust = TRUE)
iris_imp_mice <- complete(mice(iris_imp, m = 1))
iris_imp_rf <- missForest(iris_imp)$ximp
iris_imp_ranger <- missRanger(iris_imp)
evaluation(iris_orig, iris_imp_kNN, m = m, vartypes = c(rep("numeric", 4), "factor"))$error
evaluation(iris_orig, iris_imp_irmi, m = m, vartypes = c(rep("numeric", 4), "factor"))$error
evaluation(iris_orig, iris_imp_mice, m = m, vartypes = c(rep("numeric", 4), "factor"))$error
evaluation(iris_orig, iris_imp_rf, m = m, vartypes = c(rep("numeric", 4), "factor"))$error
evaluation(iris_orig, iris_imp_ranger, m = m, vartypes = c(rep("numeric", 4), "factor"))$error


## ----mixgbhist, fig.cap="Built in diagnostic tools for multiple imputed data sets with package mixgb.", fig.pos="H"----
library(mixgb)
imp_xg <- mixgb(iris_imp, m = 5)
plot_hist(imp_xg, "Sepal.Length", iris_imp)




## ----strethanol, echo=TRUE-----------------------------------------------------------------------------------------
data("ethanol", package = "lattice")
str(ethanol)


## ----ethanolplot, fig.align='center', fig.height=7, echo=FALSE, eval=FALSE-----------------------------------------
## data("ethanol", package = "lattice")
## plot(NOx ~ E, data = ethanol)


## ----rsqeth, echo = TRUE-------------------------------------------------------------------------------------------
lm1 <- lm(NOx ~ E, data = ethanol)
summary(lm1)$r.squ


## ----ethhand, echo = TRUE, tidy=FALSE------------------------------------------------------------------------------
q1 <- quantile(ethanol$E,0.25)  
q2 <- quantile(ethanol$E,0.75)
ethanol$E2 <- ethanol$E^2
ethanol$E3 <- ethanol$E^3
ethanol$E31 <- ifelse((ethanol$E - q1)^3 > 0,
                      (ethanol$E - q1)^3, 0)
ethanol$E32 <- ifelse((ethanol$E - q2)^3 > 0,
                      (ethanol$E - q2)^3, 0)
lm2 <- lm(NOx ~ E + E2 + E3 + E31 + E32, data = ethanol)
summary(lm2)$r.squ


## ----handsp, echo = TRUE, fig.align='center', out.width='70%', fig.cap='Cubic spline fit with two knots.', tidy=FALSE----
plot(NOx ~ E, data = ethanol); SEQ <- seq(0.53, 1.24, 0.02)
pred <- predict(lm2, newdata = data.frame("E" = SEQ,
                                          "E2" = SEQ^2, "E3"=SEQ^3,
                                          "E31"= ifelse((SEQ - q1)^3 > 0, (SEQ - q1)^3, 0),
                                          "E32"= ifelse((SEQ - q2)^3 > 0, (SEQ - q2)^3, 0)))
lines(SEQ, pred); abline(v = c(q1, q2), lty = 2)


## ----handimpute, tidy=FALSE----------------------------------------------------------------------------------------
predict(lm2, newdata = data.frame("E" = 0.7,
                                  "E2" = 0.7^2, "E3"=0.7^3,
                                  "E31"= ifelse((0.7 - q1)^3 > 0, (0.7 - q1)^3, 0),
                                  "E32"= ifelse((0.7 - q2)^3 > 0, (0.7 - q2)^3, 0))) + 
  rnorm(1, 0, sd(resid(lm2))) # imputation uncertainty


## ----funbootsplines, tidy=FALSE------------------------------------------------------------------------------------
fun <- function(){
  n <- nrow(ethanol)
  lm2 <- lm(NOx ~ E + E2 + E3 + E31 + E32, 
            data = ethanol[sample(1:n, size = n, 
                                  replace = TRUE), ]) 
  predict(lm2, newdata = data.frame("E" = 0.7,
                                    "E2" = 0.7^2, "E3"=0.7^3,
                                    "E31"= ifelse((0.7 - q1)^3 > 0, (0.7 - q1)^3, 0),
                                    "E32"= ifelse((0.7 - q2)^3 > 0, (0.7 - q2)^3, 0))) + 
    rnorm(1, 0, sd(resid(lm2))) # imputation uncertainty
}


## ----misplines, tidy=FALSE-----------------------------------------------------------------------------------------
imps <- replicate(10, fun())
imps


## ----plotmisplines, tidy=FALSE, fig.cap='10 multiple imputations (in red) for E equal to 0.7 using piecewise polynomials / splines of degree 3.'----
plot(NOx ~ E, data = ethanol)
points(rep(0.7,10), imps, col = "red", pch = 1, cex = 1.5)
abline(v = c(q1, q2), lty = 2)
abline(v = 0.7, lty = 3, col = "grey")



## ----laodmgcv, echo=TRUE-------------------------------------------------------------------------------------------
data("ethanol", package = "lattice")
library("mgcv")


## ----eth, fig.align='center', echo=TRUE, fig.cap='Fit of a FAM to the ethanol data using a cubic regression spline with default parameter setting.', out.width='70%'----
g1 <- gam(NOx ~ s(E, bs = "cr"), data = ethanol)
plot(NOx ~ E, data = ethanol)
SEQ <- seq(0.5, 1.3, 0.01)
lines(SEQ, predict(g1, newdata = data.frame("E" = SEQ)))


## ----gamstest, echo=TRUE, tidy=FALSE-------------------------------------------------------------------------------
g1 <- gam(NOx ~ s(E, bs = "cr"), data = ethanol) # default 
g2 <- gam(NOx ~ s(E, sp = 10, bs = "cr"), 
          data = ethanol) # very smooth
g3 <- gam(NOx ~ s(E, sp = 0, bs = "cr"), 
          data = ethanol) # wriggling
g4 <- gam(NOx ~ s(E, sp = 0.5, bs = "cr"), 
          data = ethanol) # smooth
g5 <- gam(NOx ~ s(E, k = 50, bs = "cr"), 
          data = ethanol) # more wriggling
g6 <- gam(NOx ~ s(E, sp = 0, k = 83, bs = "cr"), 
          data = ethanol) # very wriggling


## ----ethtp, fig.align='center', echo=TRUE, fig.cap='GAM fit pf the ethanol data set using thin plate regression splines.', out.width='70%'----
g1 <- gam(NOx ~ s(E, bs = "tp"), data = ethanol)
plot(NOx ~ E, data = ethanol); SEQ <- seq(0.5, 1.3, 0.01)
lines(SEQ, predict(g1, newdata = data.frame("E" = SEQ)))


## ----gamstest2, echo=TRUE------------------------------------------------------------------------------------------
g1 <- gam(NOx ~ s(E), data = ethanol) # default optimization
g2 <- gam(NOx ~ s(E, sp = 10), data = ethanol) # smoothnest
g3 <- gam(NOx ~ s(E, sp = 0), data = ethanol) # wiggly
g4 <- gam(NOx ~ s(E, sp = 0.5), data = ethanol) # smooth
g5 <- gam(NOx ~ s(E, k = 50), data = ethanol) # wiggly
g6 <- gam(NOx ~ s(E, sp = 0, k = 83), data=ethanol)# wiggliest


## ----ubre, echo=TRUE, tidy=FALSE-----------------------------------------------------------------------------------
c(g1$gcv.ubre, g2$gcv.ubre, g3$gcv.ubre, g4$gcv.ubre,
  g5$gcv.ubre, g6$gcv.ubre)


## ----summarygam, echo=TRUE-----------------------------------------------------------------------------------------
summary(g1) 


## ----gamstart, warning=FALSE, message=FALSE------------------------------------------------------------------------
library(mgcv)
data(ethanol, package = "lattice")
n <- nrow(ethanol)
# set some missings in y, to demonstrate imputation
ethanol_na <- ethanol
set.seed(123)
ethanol_na$NOx[sample(1:nrow(ethanol_na), 10)] <- NA


## ----gamboot, warning=FALSE, message=FALSE-------------------------------------------------------------------------
boot_idx <- sample(1:nrow(ethanol_na), size = n, replace = TRUE)
boot_dat <- ethanol_na[boot_idx, ]


## ----gamfit, warning=FALSE, message=FALSE--------------------------------------------------------------------------
# GAM with thin plate regression splines
fit_gam <- gam(NOx ~ s(E), data = boot_dat)


## ----coefgam-------------------------------------------------------------------------------------------------------
beta <- coef(fit_gam)
V <- vcov(fit_gam)


## ----cholesky, tidy=FALSE------------------------------------------------------------------------------------------
Cv <- chol(V)
set.seed(123)
nus <- rnorm(length(beta))
beta_sims <- beta + 
  t(Cv) %*% 
  matrix(nus, nrow = length(beta), ncol = 1)
beta_sims


## ----covvarsim, tidy=FALSE-----------------------------------------------------------------------------------------
covar_sim <- predict(fit_gam, newdata = boot_dat, 
                     type = "lpmatrix")
linpred_sim <- covar_sim %*% beta_sims


## ------------------------------------------------------------------------------------------------------------------
invlink <- function(x) x
exp_val_sim <- invlink(linpred_sim)


## ----fitgamnormal, tidy=FALSE--------------------------------------------------------------------------------------
w <- is.na(ethanol_na$NOx)
pred <-  predict(fit_gam, newdata = ethanol_na, 
                 type = "response")[w]
ymiss <- pred + rnorm(n = sum(w), 
                      mean = 0, 
                      sd = summary(fit_gam)$scale)
ymiss

## function impGAM is still experimental and not published in package VIM.
## Look out for updates of package VIM

## package deepImp is not yet published.
## Look out for updates/release of package deepImp (planned for 2023)


