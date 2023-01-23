#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 6: Deductive imputation and outlier replacement 
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3

library(VIM)
library(validate)
library(deductive)

## ----loadsilcagain, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------
data(eusilc, package = "laeken")



## ----dcmod, echo=TRUE, tidy=FALSE----------------------------------------------------------------------------------
library(dcmodify)
m <- modifier( 
  if (age < 0) age <- -1 * age,
  if (age >= 95) age <- 95,
  if (age < 16) pl030 <- NA,
  if (eqIncome < 0) eqIncome <- abs(eqIncome),
  if (eqIncome <= 0.8 * py010n) eqIncome <- 0.8 * py010n, 
  if (age < 16) py010n <- 0
)
modified <- modify(eusilc, m)


## ----ruleset00,eval=FALSE, echo=TRUE, eval =TRUE, tidy=FALSE-------------------------------------------------------
# ruleset
library("VIM")
data(sleep, package = "VIM")
sleep_replaced <- kNN(sleep, 
                      variable = c("NonD","Dream","Sleep"), 
                      imp_var = FALSE)

rules <- validator(
  NonD + Dream == Sleep,
  NonD >= 0,
  Dream >= 0
)


## ----violations0---------------------------------------------------------------------------------------------------
data(sleep, package = "VIM")
sleep <- kNN(sleep, variable = "Sleep", imp_var = FALSE)
# some artificial typos
sleep$Sleep[2] <- sleep$Sleep[2] * 100
sleep$NonD[5] <- sleep$NonD[5] * 100
cf <- confront(sleep, rules)
cf


## ----typoscorrect0,echo=TRUE, eval =TRUE, cache=TRUE, tidy=FALSE---------------------------------------------------
library("errorlocate")
sleep_replaced <- replace_errors(sleep, 
                                 rules, 
                                 value = "suggestion")
typos_corrected <- impute_lr(sleep_replaced, 
                             rules)


## ----compareviolationsout,eval=TRUE,echo=FALSE, tidy=FALSE---------------------------------------------------------
# Compare progress on rule violation using validate::compare 
compare(rules, 
        "original" = sleep, 
        "sleep_replaced" = sleep_replaced, 
        "typos_corrected" = typos_corrected)


## ------------------------------------------------------------------------------------------------------------------
head(typos_corrected)


## ----replaceviol0, cache=TRUE, tidy=FALSE--------------------------------------------------------------------------
rules <- validator(
  NonD + Dream == Sleep,
  NonD >= 0,
  Dream >= 0,
  Sleep < 24
)
sleep_replaced <- replace_errors(sleep, 
                                 rules, 
                                 value = "suggestion")
typos_corrected <- impute_lr(sleep_replaced, 
                             rules)
head(typos_corrected)


## ----rspa, echo=TRUE, eval =TRUE, message=FALSE, warning=FALSE, tidy=FALSE-----------------------------------------
library(rspa)
# Create imputed set:
data(sleep, package = "VIM")
sleep <- sleep[, -5] # you may comment this 
rules <- validator(
  NonD >= 2,
  Dream >= 0,
  NonD <= 18,
  Dream <= 7,
# # additional rules:
#   NonD + Dream == Sleep, # you may uncomment this 
#   Sleep >= 2, # you may uncomment this 
#   Sleep <= 20, # you may uncomment this 
  Span >= 2, 
  Span <= 100,
  Gest >= 10,
  Gest <= 700
)
# uncomment for other imputation methods
# imputed <- irmi(sleep, imp_var = FALSE)
# library(mice)
# imputed <- mice::complete(mice(sleep, m = 1))
# imputed <- missForest::missForest(sleep)$ximp
library(missMDA)
imputed <- data.frame(imputePCA(sleep)$completeObs)
imputed %>% confront(rules) %>% summary()


## ------------------------------------------------------------------------------------------------------------------
head(sleep[imputed$Dream < 0, ])
head(imputed[imputed$Dream < 0, ])


## ----match-restrictions,echo=TRUE, eval =TRUE----------------------------------------------------------------------
library(dplyr)
valid <- imputed %>% match_restrictions(rules)


## ----confront,echo=TRUE, eval =TRUE, tidy=FALSE--------------------------------------------------------------------
# Check: do we satisfy all rules?
valid %>% 
  confront(rules, lin.eq.eps=0.01) %>% 
  summary()


## ----validimputed,echo=TRUE, eval =TRUE----------------------------------------------------------------------------
head(valid[imputed$Dream < 0, ])




## ----laekensilc, echo=FALSE, warning = FALSE, message=FALSE--------------------------------------------------------
library(dplyr)
library(laeken)
data(eusilc)


## ----winsorization-univariate, tidy=FALSE--------------------------------------------------------------------------
library(laeken)
data(eusilc, package = "laeken")
# employee cash or near cash income (net)
pos <- eusilc$py010n == 0 | is.na(eusilc$py010n)
cash <- eusilc$py010n[!pos]


## ----winsorization-univariate2, tidy=FALSE-------------------------------------------------------------------------
# calculations for transformed data
wmed <- weightedMedian(log(cash),   
                       weights = eusilc$rb050[!pos],   
                       na.rm = TRUE)
# weighted median of employee cash income
exp(wmed)


## ----winsorization-univariate3, tidy=FALSE-------------------------------------------------------------------------
# threshold for potential outliers
threshold <- wmed + 3 * mad(log(cash))
# back-transformed to original scale
threshold <- exp(threshold)
threshold


## ----winsorization-univariate4, tidy=FALSE-------------------------------------------------------------------------
mean(cash > threshold)


## ----winsorization-univariate5, tidy=FALSE-------------------------------------------------------------------------
# summary of original cash income
summary(eusilc$py010n)
# imputation / winsorisation / replacement
eusilc$py010n[eusilc$py010n > 
                threshold & !is.na(eusilc$py010n)] <- threshold
# summary of imputed cash income
summary(eusilc$py010n)


## ----mahout0, message=FALSE, warning=FALSE-------------------------------------------------------------------------
library("robustbase")
library("rrcov")
data("wood")
## MM estimator for center and covariance
mm <- CovMMest(wood)
# squared Mahalanobis distances
mah <- mahalanobis(wood, center = mm$center, cov = mm$cov)
## outliers
indexOut <- mah > qchisq(0.975, df = ncol(wood) - 1)


## ---- echo=TRUE, eval=FALSE----------------------------------------------------------------------------------------
## indexOut


## ---- echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------
indexOut


## ------------------------------------------------------------------------------------------------------------------
library("sdcMicro")
wood_replaced <- mvTopCoding(wood)
# comparision
wood[indexOut, ]
wood_replaced[indexOut, ]

