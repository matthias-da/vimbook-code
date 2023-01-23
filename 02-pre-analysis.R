#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2013
###   Chapter 2: Distribution, pre-analysis of missing values and data quality.
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


## ----loadsilcagain0, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------
data(eusilc, package = "laeken")


## ----jaccardistshow, echo=TRUE-----------------------------------------------------------------------
jaccard_dist <- function(c1, c2){
  sum(c1 & c2) / 
    (sum(c1 & c2) + sum(c1 & !c2) + sum(!c1 & c2))
}
x1 <- c(1,0,0,0,1,0,1,0)
x2 <- c(1,1,1,1,0,0,1,0)
jaccard_dist(x1, x2)


## ---- echo = TRUE------------------------------------------------------------------------------------
data("brittleness", package = "VIM")
brittleness[4:5, ]
sqrt((368 - 697)^2 + (451 - 709)^2 + (426 - 733)^2)
# faster, and for all distances between all observations:
# dist(brittleness) # output suppressed


## ----------------------------------------------------------------------------------------------------
any(eusilc$age < 0)
index <- eusilc$age < 0
sum(index)


## ----------------------------------------------------------------------------------------------------
eusilc$age[index] <- 0


## ----validatesilc, tidy=FALSE------------------------------------------------------------------------
library(validate)
data(eusilc, package = "laeken")
ch <- check_that(
  eusilc[, c("age", "eqIncome", "py010n", "pl030")], 
  age >= 0,
  age < 95,
  pl030 %in% 1:7,
  if (age < 16) is.na(pl030), 
  eqIncome > 0.8 * py010n
)


## ----validatesilcprint, tidy=FALSE-------------------------------------------------------------------
ch


## ----summarych, validatesilcsum----------------------------------------------------------------------
summary(ch)


## ----validatorrecall,echo=TRUE, eval=TRUE, tidy=FALSE------------------------------------------------
# recall our rule set and
# define validation rules
rules <- validator(
  age >= 0,
  age < 95,
  pl030 %in% 1:7,
  if (age < 16) is.na(pl030), 
  eqIncome > 0.8 * py010n
)
# confront data with ruleset
cf <- confront(eusilc, rules)
# summary(cf) # basically the same as before

