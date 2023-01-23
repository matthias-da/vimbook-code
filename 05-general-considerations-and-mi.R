#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2013
###   Chapter 5: General considerations on univariate methods, single and multiple imputation
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3

## ---- echo=FALSE-------------------------------------------------------------------------------------
library(dplyr)


## ----datagen, cache = FALSE, echo=FALSE, warning=FALSE, message=FALSE, eval = T----------------------
# set random number generator
set.seed(1234)
  library(dplyr)
simdat <- function(){
  # bivariate normal data
  x <- MASS::mvrnorm(100, mu = c(10,10), Sigma = matrix(3*c(1,0.9,0.9,1), ncol = 2))
  x <- data.frame(x)
  colnames(x) <- c("xfull", "yfull") 
  x <- x %>% arrange(xfull)
  # create artificial missings MCAR
  w <- sample(1:nrow(x), 10)
  # create artificial missings MCAR
  lm1 <- lm(yfull ~ xfull, data = x)
  w3 <- resid(lm1) > quantile(resid(lm1), 0.9)
  w2 <- sample(50:nrow(x), 10, prob = (x$xfull[50:nrow(x)])^5)
  x$ymissMCAR <- x$ymissMAR <- x$ymissMNAR <- x$yfull
  x$ymissMCAR[w] <- NA
  x$ymissMAR[w2] <- NA
  x$ymissMNAR[w3] <- NA
  x$ymissMCAR_ind <- is.na(x$ymissMCAR)
  x$ymissMAR_ind <- is.na(x$ymissMAR)
  x$ymissMNAR_ind <- is.na(x$ymissMNAR)
  # additional information
  x$MCAR <- ifelse(x$ymissMCAR_ind, "true value of NA", "not modified")
  x$MAR <- ifelse(x$ymissMAR_ind, "true value of NA", "not modified")
  x$MNAR <- ifelse(x$ymissMNAR_ind, "true value of NA", "not modified")
  x$group <- factor(ifelse(x$yfull < 10, "low", "high"))
  # x$shapes <- factor(as.character(ifelse(x$ymiss_ind, "21", "b")))
  # head(x, 3) # first three observations
  return(x)
}
toydataMiss <- simdat()


## ----loadtoydataMiss, cache = FALSE------------------------------------------------------------------
data("toydataMiss", package = "VIM")


## ----headtoy, eval=FALSE, echo=TRUE------------------------------------------------------------------
## data(toydataMiss, package="VIM")
## head(toydataMiss, 3)


## ----headtoyout, eval=TRUE, echo=FALSE---------------------------------------------------------------
data(toydataMiss, package="VIM")
head(toydataMiss, 3)


## ----meanstoy----------------------------------------------------------------------------------------
mean(toydataMiss$ymissMAR, na.rm = TRUE)


## ----checkbias, cache=TRUE, tidy=FALSE---------------------------------------------------------------
sim <- function(){
  dat <- simdat()
  c("full" = mean(dat$yfull, na.rm = TRUE),
    "MCAR" = mean(dat$ymissMCAR, na.rm = TRUE),
    "MAR" = mean(dat$ymissMAR, na.rm = TRUE),
    "MNAR" = mean(dat$ymissMNAR, na.rm = TRUE))
}
set.seed(123)
r <- replicate(5000, sim())
dim(r)
rowMeans(r) - 10


## ----checkbiasvar, cache=TRUE------------------------------------------------------------------------
apply(r, 1, var) - 0.03


## ----checkbias2, cache=TRUE--------------------------------------------------------------------------
mean(replicate(1000, var(simdat()$yfull, na.rm = TRUE)))
mean(replicate(1000, var(simdat()$ymissMAR, na.rm = TRUE)))


## ----meanimp, cache = TRUE, tidy=FALSE, echo=FALSE---------------------------------------------------
# imputation with arthmetic mean
m <- mean(toydataMiss$ymissMCAR, na.rm = TRUE)   
toydataMiss$yimpMCAR_mean <- toydataMiss$ymissMCAR
toydataMiss$yimpMCAR_mean[toydataMiss$ymissMCAR_ind] <- m
m <- mean(toydataMiss$ymissMAR, na.rm = TRUE)  
toydataMiss$yimpMAR_mean <- toydataMiss$ymissMAR
toydataMiss$yimpMAR_mean[toydataMiss$ymissMAR_ind] <- m
m <- mean(toydataMiss$ymissMNAR, na.rm = TRUE)  
toydataMiss$yimpMNAR_mean <- toydataMiss$ymissMNAR
toydataMiss$yimpMNAR_mean[toydataMiss$ymissMNAR_ind] <- m


## ----checkbiasMean, cache=TRUE, tidy=FALSE-----------------------------------------------------------
# univariate mean imputation
impute_vec <- function(x){
  m <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- m
  return(x)
}

sim <- function(){
  dat <- simdat()
  c("full" = mean(dat$yfull),
    "MCAR" = mean(impute_vec(dat$ymissMCAR)),
    "MAR" = mean(impute_vec(dat$ymissMAR)),
    "MNAR" = mean(impute_vec(dat$ymissMNAR)))
}
set.seed(123)
r <- replicate(5000, sim())
dim(r)
# point estimates, bias
rowMeans(r) - 10
# variance of point estimates, bias
apply(r, 1, var) - 0.03


## ----grouptoy, tidy=FALSE----------------------------------------------------------------------------
toydataMiss <- toydataMiss %>% 
  group_by(group) %>% # high and low values of y
  mutate(m1 = ifelse(ymissMCAR_ind, 
                     mean(ymissMCAR, na.rm = TRUE), 
                     ymissMCAR))



## ----modeimpute, tidy=FALSE--------------------------------------------------------------------------
getmode <- function(x){
  names(which.max(table(x)))
}

set.seed(123)
# some example test data
cat <- cat_orig <- sample(c("AT","CH","D","EU"), 100, 
                          replace = TRUE, 
                          prob = c(0.8,0.05,0.1,0.05))
table(cat_orig)

# set some values to be missing
cat[sample(1:100, size = 20)] <- NA
cat_na <- cat
table(cat_na, useNA = "always")

# imputation with the mode
cat[is.na(cat_na)] <- getmode(cat_na)
table(cat)


## ----modeimputerand, tidy=FALSE----------------------------------------------------------------------
cat[is.na(cat_na)] <- sample(na.omit(unique(cat_na)), 
        size = 20, replace = TRUE, 
        prob = prop.table(table(cat_na)))
table(cat)


## ----miNote6d, eval = TRUE, echo=FALSE---------------------------------------------------------------
# regression coeficients and their standard errors, 
# result for pooled multiple imputation:
# library(mice)
# imp <- mice(x[, c(1,3)], m = 10, print = FALSE, method = "sample")
# imp1 <- with(imp, lm(GasNA ~ Temp))
# #summary(lm(Gas ~ Temp, data = x))
# summary(pool(imp1), conf.int = TRUE)
data(whiteside, package = "MASS")
x <- whiteside[, 2:3]
g1 <- ggplot(whiteside, aes(x = Temp, y = Gas)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# set one value to missing, just for demonstration
x <- x %>% mutate(GasNA = Gas, GasImp1 = Gas)
x$GasNA[25] <- NA
x <- x %>% mutate(GasNA_index = is.na(GasNA))
x <- x %>% 
  mutate(GasImp1 = GasNA,
         GasNA_index = ifelse(is.na(GasNA), "red", "black")) 
x$GasImp1[25] <- 4

imp <- function(x){
  mod <- lm(Gas ~ Temp, data = x)
  x[25, "GasImp1"] <- predict(mod)[25] + rnorm(1, mean = 0, sd = sd(resid(mod)))
  return(x)
}
for( i in 1:10 ){
  x <- cbind(x, imp(x)$GasImp1)
}
colnames(x) <- c("Temp","Gas","GasNA", "GasImp0", "GasNA_index", paste("imputation ", 1:10, sep=""))

se1 <- summary(lm(`imputation 1` ~ Temp, data = x))$coef[2,2]
se2 <- summary(lm(`imputation 2` ~ Temp, data = x))$coef[2,2]
se3 <- summary(lm(`imputation 3` ~ Temp, data = x))$coef[2,2]
se4 <- summary(lm(`imputation 4` ~ Temp, data = x))$coef[2,2]
se5 <- summary(lm(`imputation 5` ~ Temp, data = x))$coef[2,2]
se6 <- summary(lm(`imputation 6` ~ Temp, data = x))$coef[2,2]
se7 <- summary(lm(`imputation 7` ~ Temp, data = x))$coef[2,2]
se8 <- summary(lm(`imputation 8` ~ Temp, data = x))$coef[2,2]
se9 <- summary(lm(`imputation 9` ~ Temp, data = x))$coef[2,2]
se10 <- summary(lm(`imputation 10` ~ Temp, data = x))$coef[2,2]
se <- c(se1,se2,se3,se4,se5,se6,se7,se8,se9,se10)
se
co1 <- summary(lm(`imputation 1` ~ Temp, data = x))$coef[2,1]
co2 <- summary(lm(`imputation 2` ~ Temp, data = x))$coef[2,1]
co3 <- summary(lm(`imputation 3` ~ Temp, data = x))$coef[2,1]
co4 <- summary(lm(`imputation 4` ~ Temp, data = x))$coef[2,1]
co5 <- summary(lm(`imputation 5` ~ Temp, data = x))$coef[2,1]
co6 <- summary(lm(`imputation 6` ~ Temp, data = x))$coef[2,1]
co7 <- summary(lm(`imputation 7` ~ Temp, data = x))$coef[2,1]
co8 <- summary(lm(`imputation 8` ~ Temp, data = x))$coef[2,1]
co9 <- summary(lm(`imputation 9` ~ Temp, data = x))$coef[2,1]
co10 <- summary(lm(`imputation 10` ~ Temp, data = x))$coef[2,1]
co <- c(co1,co2,co3,co4,co5,co6,co7,co8,co9,co10)
co
vw <- mean(se^2)
vb <- 1 / 10 * sum(co - mean(co))
vtot <- vw + vb + vb/10
se_pooled <- sqrt(vtot)
se_pooled


## ----miNote6e, eval = TRUE, echo=FALSE---------------------------------------------------------------
data(whiteside, package = "MASS")
x <- whiteside[, 2:3]
g1 <- ggplot(whiteside, aes(x = Temp, y = Gas)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# set one value to missing, just for demonstration
x <- x %>% mutate(GasNA = Gas, GasImp1 = Gas, TempNA = Temp, TempImp1 = Temp, )
set.seed(123)
x <- x[sample(1:nrow(x)), ]
x$GasNA[1:20] <- NA
x$TempNA[21:40] <- NA
x <- x %>% mutate(GasNA_index = is.na(GasNA))
imp <- function(x){
  mod <- lm(Gas ~ Temp, data = x)
  x[1:20, "GasImp1"] <- predict(mod)[1:20] + rnorm(20, mean = 0, sd = sd(resid(mod)))
  x[21:40, "TempImp1"] <- predict(mod)[21:40] + rnorm(20, mean = 0, sd = sd(resid(mod)))
  return(list("se" = summary(lm(GasImp1 ~ TempImp1, data = x))$coef[2,2],
              "coef" = summary(lm(GasImp1 ~ TempImp1, data = x))$coef[1,2]))
}
se <- co <- numeric(10)
for( i in 1:10 ){
  se[i] <- imp(x)$se
  co[i] <- imp(x)$se
}
summary(se)


## ----miNote6ee, eval = TRUE, echo=FALSE--------------------------------------------------------------
# library(mice)
# imp <- mice(x[, c(5,3)], m = 10, print = FALSE, method = "sample")
# imp1 <- with(imp, lm(TempNA ~ GasNA))
# summary(pool(imp1), conf.int = TRUE)[2,3]
# selbst:
vw <- mean(se^2)
vb <- 1 / 10 * sum(co - mean(co))
vtot <- vw + vb + vb/10
se_pooled <- sqrt(vtot)
se_pooled


## ----students1, tidy=FALSE---------------------------------------------------------------------------
library(miceadds)
data(data.ma01)
# some variables needs to be coded as factor
data.ma01$migrant <- factor(data.ma01$migrant)
data.ma01$female <- factor(data.ma01$female)
data.ma01$migrant <- factor(data.ma01$migrant)
# select only the needed variables
x <- data.ma01[, c("math", "migrant", "female", 
                   "books", "hisei", "paredu", 
                   "studwgt")]
# first two observations
head(x, 2)


## ----students0, tidy=FALSE---------------------------------------------------------------------------
coefficients(summary(lm(scale(math) ~  migrant + female + 
                          books + hisei + paredu, 
                        weights = studwgt, data = x)))


## ----studentsmiss, fig.cap='Summary statistics on missing values using the aggr plot from VIM.'------
aggr(x)


## ----studentsirmi1, tidy=FALSE, cache=TRUE-----------------------------------------------------------
set.seed(123) 
imputed1 <- irmi(data.ma01)
coefficients(summary(lm(scale(math) ~  migrant + female + 
                          books + hisei + paredu, 
                        weights = studwgt, data = imputed1)))


## ----students2, cache=TRUE, warning=FALSE, message=FALSE---------------------------------------------
set.seed(123) 
imputed5 <- list()
for(i in 1:5){
  imputed5[[i]] <- irmi(data.ma01)
}


## ----students3, tidy=FALSE---------------------------------------------------------------------------
mod <- lapply(imputed5, function(x){
  coefficients(summary(lm(scale(math) ~  migrant + female + 
                            books + hisei + paredu, 
                          weights = studwgt, data = x)))[2,1:2]
})
coefMigration <- data.frame(t(matrix(unlist(mod), ncol = 5)))
colnames(coefMigration) <- c("coef_migration", 
                             "se_coef_migration")
coefMigration


## ----students4---------------------------------------------------------------------------------------
mtheta <- mean(coefMigration$coef_migration)
mtheta


## ----students5---------------------------------------------------------------------------------------
vw <- mean(coefMigration$se_coef_migration^2)
vw


## ----students6---------------------------------------------------------------------------------------
vb <- 1/4 * sum((coefMigration$coef_migration - mtheta)^2)
vb


## ----students7---------------------------------------------------------------------------------------
vtot <- vw + vb + vb / 5 
vtot


## ----students8---------------------------------------------------------------------------------------
sqrt(vtot)


## ----poolmice, tidy=FALSE----------------------------------------------------------------------------
mi_mice <- mice(x, printFlag = FALSE)
fit <- with(data = mi_mice, exp = lm(scale(math) ~  
                            migrant + female + 
                            books + hisei + paredu, 
                          weights = studwgt))
summary(pool(fit))

