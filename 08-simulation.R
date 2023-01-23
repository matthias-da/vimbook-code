## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, tidy = TRUE, warning = FALSE, message = FALSE)
options(width = 68)
# if(!knitr:::is_html_output())
# {
#   options("width"=56)
#   knitr::opts_chunk$set(tidy.opts=list(width.cutoff=56, indent = 2), tidy = TRUE)
#   knitr::opts_chunk$set(fig.pos = 'H')
# }
library(VIM)
library(dplyr)
library(simFrame)
library(laeken)
library(ggplot2)
theme_set(theme_bw())
theme_update(text = element_text(size=20))


## ----SBSpatt1, fig.cap='Missing pattern structure of the SBS5242 data set.', fig.pos='H'----
data("SBS5242", package = "VIM")
a <- aggr(SBS5242, plot = FALSE) 
plot(a, combined = TRUE)


## ----SBSpatt2, fig.cap='Missing values of the SBS5242 data set visualized by the matrixplot.', fig.pos='H'----
data("SBS5242", package = "VIM")
matrixplot(SBS5242, interactive = FALSE)


## ---- tidy=FALSE, warning=FALSE, message=FALSE, eval=FALSE--------------------
## sim_realdata <- function(x, rate){
##   complete <- miss <- na.omit(x)
##   # detect and flag outliers
##   obj <- !as.logical(rrcovHD::OutlierMahdist(x=complete)@flag)
##   indexOutliers <- (1:nrow(complete))[obj]
##   indexRegular <- (1:nrow(complete))[!obj]
##   # introduce missings only for non-outliers
##   for(j in 1:ncol(complete)){
##     miss[sample(indexRegular, size = round(rate * nrow(complete)),
##                 replace = FALSE), j] <- NA
##   }
##   # impute missings with several methods
##   imp_knn <- VIM::kNN(miss, imp_var = FALSE)
##   imp_mdapca <- missMDA::imputePCA(miss, ncp = 1,
##                                    method = "EM")$completeObs
##   source("R/impPCA.R")
##   imp_pca <- impPCA(miss, m = 1, verbose = FALSE)
##   imp_pmm <- complete(mice::mice(miss, printFlag = FALSE, m = 1))
##   imp_midastouch <- complete(mice::mice(miss,
##                     printFlag = FALSE, m = 1, method = "midastouch"))
##   imp_irmi <- VIM::irmi(miss, imp_var = FALSE, robust = TRUE, mi = 1)
##   imp_ranger <- missRanger::missRanger(data.frame(miss), verbose = 0)
##   imp_mixgb <- mixgb::mixgb(miss, m = 1)[[1]]
##   imp_deep <- deepImp::impNNet(data.frame(miss), verbose = FALSE)
##   # evaluation (e.g. with nrmse)
##   res <- c(
##     VIM::nrmse(complete, imp_knn, m = is.na(miss)),
##     VIM::nrmse(complete, imp_mdapca, m = is.na(miss)),
##     VIM::nrmse(complete, imp_pca, m = is.na(miss)),
##     VIM::nrmse(complete, imp_pmm, m = is.na(miss)),
##     VIM::nrmse(complete, imp_midastouch, m = is.na(miss)),
##     VIM::nrmse(complete, imp_irmi, m = is.na(miss)),
##     VIM::nrmse(complete, imp_ranger, m = is.na(miss)),
##     VIM::nrmse(complete, as.matrix(imp_mixgb), m = is.na(miss)),
##     VIM::nrmse(complete, as.matrix(imp_deep), m = is.na(miss))
##     )
##   names(res) <- c("VIM::kNN", "missMDA::imputePCA",
##                   "VIM::impPCA (mcd)","pmm (mice)",
##                   "midastouch (mice)", "VIM::irmi",
##                   "missRanger", "mixgb", "deepImp")
##   return(res)
## }
## 
## # simulation
## set.seed(123)
## dres <- replicate(25,
##                   sim_realdata(SBS5242, rate = 0.2))


## ---- echo=FALSE--------------------------------------------------------------
load("/Users/matthias/workspace/vimbook/sim/completeSim.rda")


## ----simRealresults, tidy=FALSE, warning=FALSE, message=FALSE, fig.cap='Simulation results (NRMSE) for the SBS5242 data set.', fig.pos='H'----
# produces next figure 
par(mar = c(4,10,0.2,0.1), mfrow = c(2,1), cex.axis = 0.75)
boxplot(t(dres), xlab = "NRMSE", horizontal = TRUE, las = 1)
abline( h = 0:9, col = "lightgray", lty = "dotted")
boxplot(t(dres), xlab = "NRMSE (in log-scale)", 
        horizontal = TRUE, las = 1, log = "x")
abline( h = 0:9, col = "lightgray", lty = "dotted")


## ---- fig.cap='One realization from the data generating function including some misclassifiations.', tidy=FALSE, eval=FALSE, echo=FALSE----
## set.seed(1234)
## # parameters for the functional relationship
## b0 <- 2
## b1 <- 1
## b2 <- -0.1
## # number of observations to be simulated
## n <- 200
## 
## # simulation of covariates and errors
## runtime <- rexp(n, 1/100)
## type <- as.logical(rbinom(n, 1, 0.4))
## eps <- rnorm(n, 0, 0.1)
## 
## # simulation of the response
## energy <- exp(b0 + b1 * type + b2 * log(runtime) + eps)
## x_good <- data.frame(runtime = runtime,
##                    type = type,
##                    energy = energy)
## 
## # simulating outliers / misclassifications
## n_outlier <- 10
## runtime_2 <- rexp(n_outlier, 1/350)
## type <- rbinom(n_outlier, 1, 0.4)
## which_runtime <- rbinom(1, 1, 0.5)
## runtime <- rexp(n_outlier, 1/10)
## runtime[type == which_runtime] <-
##   runtime_2[type == which_runtime]
## eps <- rnorm(n_outlier, 0, 0.1)
## energy <- exp(b0 + b1 * type + b2 * log(runtime) + eps)
## 
## # putting it together
## x_outlier <- data.frame(runtime = runtime,
##                            type = factor(!type),
##                            energy = energy)
## x <- rbind(x_good, x_outlier)
## x$type <- factor(x$type)
## 
## # plot of one single realization
## plot(energy ~ runtime, data = x,
##      col = type, pch = as.integer(type))


## ----mvrnormsim1, fig.cap = "Multivariate normal data with MCAR missing", cache=TRUE----
library(MASS)
# unit matrix of size 5 x 5
s <- diag(5) 
n <- 100
set.seed(123)
# simplified data generation by draws from a multivariate normal
x <- mvrnorm(n = n, mu = rep(10, 5), Sigma = diag(5)) 


## ----putMCARfun, cache=TRUE---------------------------------------------------
put_MCAR <- function(x, rate = rep(0.1, 5)){
  n <- nrow(x)
  for(j in 1:(ncol(x) - 1)){
    x[sample(1:n, round(rate[j] * 100)), j] <- NA    
  }
  return(x)
}  


## ----putMCARmy1, fig.cap='Simulated multivariate normal data with MCAR.', fig.pos='H', cache=TRUE----
# put missing values into simulated data
x <- put_MCAR(x)

# show them (in red)
library(VIM)
scattmatrixMiss(x)


## ----mvrnormsim2, tidy=FALSE, fig.cap='Simulated multivariate normal data with MCAR introducing a correlation structure.', fig.pos='H', cache=TRUE----
s[lower.tri(s)] <- s[upper.tri(s)] <- 
  sample(runif(5, min = 0.7, max = 0.97))
s
x <- mvrnorm(n = 100, mu = rep(10, 5), Sigma = s)
x <- put_MCAR(x) 
scattmatrixMiss(x)


## ----model1, tidy=FALSE-------------------------------------------------------
model1 <- function(n = 100, 
                   means = rep(10, 5), 
                   Sigma = NULL){
  if(is.null(Sigma)){
    s <- diag(length(means))
    s[lower.tri(s)] <-s[upper.tri(s)] <- 
      sample(runif(5, min = 0.7, max = 0.97) * c(-1,1,1,1,-1))    
  }
  s <- corpcor::make.positive.definite(s)
  x <- mvrnorm(n, mu = means, Sigma = s)
  x <- put_MCAR(x)
  return(x)
}


## ----amputeMCAR, eval=FALSE, echo=FALSE, fig.cap='Introduced missing values for each pattern.', fig.pos='H', tidy=FALSE----
## # simplified data set
## x <- mvrnorm(n = 200, mu = rep(10, 5), Sigma = diag(5))
## # specify the pattern (1 means observed, 0 missing)
## m_pattern <- matrix(c(1,1,1,1,1,
##                       0,1,1,1,1,
##                       0,0,1,1,1,
##                       0,1,1,0,0), ncol = 5, byrow = TRUE)
## # frequency of missing values for each pattern
## # (must sum up to 1)
## myfreq <- c(0, 0.1, 0.85, 0.05)
## # generate MCAR
## result <- mice::ampute(x,
##                        freq = myfreq,
##                        patterns = m_pattern,
##                        mech = "MCAR")
## # missing data introduced
## par(mar = c(3,4,7,10), mfrow = c(1,2))
## a <- aggr(result$amp, plot = FALSE)
## summary(a)
## plot(a, combined = TRUE, numbers = TRUE, prob = FALSE)
## par(mar = c(3,4,0.5,0.1))
## matrixplot(result$amp, sortby = "V1")


## ---- eval=FALSE, echo=FALSE--------------------------------------------------
## devtools::source_url('https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/generate/amputation.R')


## ----produce_NA, tidy=FALSE, eval=FALSE, echo=FALSE---------------------------
## miss <- produce_NA(x,
##                    mechanism="MCAR",
##                    perc.missing = 0.2,
##                   freq.patterns = myfreq,
##                    by.patterns = TRUE,
##                    patterns = m_pattern)
## 
## mean(is.na(X.miss)) # check: ok
## a <- aggr(X.miss, plot = FALSE)
## summary(a) # generated 21 missing patterns


## -----------------------------------------------------------------------------
data(expendituresEU, package = "robCompositions")
n <- nrow(expendituresEU)
ind <- sample(1:n, 
              size = round(0.1 * n), # 10%
              prob = expendituresEU$Housing) # MAR
expendituresEU[ind, c("Food", "Restaurants")] <- NA


## ----putMAR, tidy=FALSE, fig.cap='Incertion of missing values in the Animals data set with MAR.'----
data(Animals, package = "MASS")
Animals <- log10(Animals)
Animals_na <- Animals
set.seed(1234)
plot(Animals, xlab = "log10(body)", ylab = "log10(brain)")
w <- sample(1:nrow(Animals), 
       size = 10, 
       prob = (Animals$body + abs(min(Animals$body))) / 
         sum(Animals$body + abs(min(Animals$body))))
Animals_na[w, "brain"] <- NA
points(Animals$body[w], Animals$brain[w], 
       col = "red", pch = 20, cex = 1.5)


## ----sbsdata, tidy=FALSE------------------------------------------------------
data("SBS5242", package = "VIM")
dim(SBS5242)
amount_missrate <- apply(SBS5242, 2, function(x)sum(is.na(x))) /
  nrow(SBS5242)
amount_missrate
SBS5242[SBS5242 < 0] <- 0


## ----sbsknn-------------------------------------------------------------------
SBS5242 <- kNN(SBS5242, imp_var = FALSE)


## ----scattmatsbs, tidy=FALSE, fig.cap='Scatterplot matrix with highlighting of missing values of the simulated Structural Business Statistics data (retail sector).', out.width='100%'----
x <- log(SBS5242)
x[x == "-Inf"] <- 0
s <- cov(x)
means <- colMeans(x)
x <- mvrnorm(n = 100, mu = means, Sigma = s)
x_miss <- put_MCAR(x, rate = amount_missrate)
x_miss <- exp(x_miss)
scattmatrixMiss(log(x_miss), cex.labels = 0.5)


## ----betadata, tidy=FALSE-----------------------------------------------------
beta <- c(0, 0, 1, 2, 0, 0, 2, 1, 0.5)
names(beta) <- colnames(SBS5242)
beta
bigplayer <- x %*% beta
bigplayer <- (bigplayer - min(bigplayer)) 
bigplayer <- bigplayer / max(bigplayer)
x_miss <- data.frame(x_miss)
set.seed(123)
x_miss$y <- runif(100) < bigplayer
x_miss$y <- factor(ifelse(x_miss$y, 
                          "big_player", 
                          "small_player"))


## ----betadatahead, tidy=FALSE-------------------------------------------------
head(x_miss$y)


## ----genvarssimple------------------------------------------------------------
genVarsSimple <- function(n=200, p=50, k=3){
  T <- matrix(rnorm(n*k,0,1), ncol=k)
  B <- matrix(runif(p*k,-1,1), ncol=k)
  X <- T %*% t(B)
  E <-  matrix(rnorm(n*p, 0,0.1), ncol=p)
  XE <- X + E
  XE <- robCompositions::pivotCoordInv(XE)
  return(constSum(XE))  
}
library(robCompositions)
x <- genVarsSimple(100)
head(x, 2)


## ---- echo=FALSE, eval=TRUE---------------------------------------------------
rm(list = ls())


## ----latentmod, tidy=FALSE----------------------------------------------------
genVarsMed <- function(latent, n, p){
	part1 <- matrix(,ncol=p, nrow=n)
	for(i in 1:25){
		part1[,i] <- runif(n, -0.2, 0.2) + 
		  latent %*% rnorm(3, 1, 0.2) 
	}
	for(i in 26:37){
		part1[,i] <- cbind(1,latent) %*% 
		  c(rnorm(1), rnorm(3, 1, 1))*(-1)
	}
	for(i in 38:p){
		part1[,i] <- cbind(1,latent) %*% 
		  c(rnorm(1), rgamma(3, 1))	    
	}
	res <- exp(part1)
	colnames(res) <- paste("x", 1:p, sep="")
	res <- data.frame(res)
	return(res)
}


## ----latentmodgen-------------------------------------------------------------
latent <- mvrnorm(n = 50, mu=rep(1,3), Sigma=diag(3))

# simulate high-dimenaional data in the simplex
x <- genVarsMed(latent, n = 50, p = 60)


## ----roundedzeros-------------------------------------------------------------
setZeros2 <- function(x, alpha){
	qu <- as.numeric(apply(x, 2, quantile, alpha))
	for(i in seq(1, ncol(x), 2)){
		x[x[,i] < qu[i], i] <- 0
	}
	return(x)
}

# introduce rounded zeros
x <- setZeros2(x, 0.1)


## ----roundedzeroscounts-------------------------------------------------------
apply(x, 2, function(x) sum(x == 0))


## ----specifyInput, tidy=FALSE-------------------------------------------------
library(simPop)
data(eusilcS, package = "simPop")
inp <- specifyInput(data=eusilcS, 
                    hhid="db030",   # household information 
                    hhsize="hsize", # household size
                    strata="db040", # region as strata
                    weight="db090"  # sampling weights
                    )


## ----simStructure, tidy=FALSE-------------------------------------------------
eusilcP <- simStructure(data=inp, 
                        method="direct", 
                        basicHHvars=c("age", "rb090"))
class(eusilcP)
eusilcP


## -----------------------------------------------------------------------------
eusilcP <- simCategorical(eusilcP, 
                         additional = c("pl030", "pb220a"), 
                         regModel = "available", 
                         method="multinom")
eusilcP


## ---- warning=FALSE, message=FALSE, tidy=FALSE, echo=TRUE, eval=FALSE---------
## regModel = ~rb090+hsize+pl030+pb220a
## 
## # multinomial model with random draws
## eusilcP <- simContinuous(eusilcP,
##                          additional="netIncome",
##                          regModel = regModel,
##                          upper=200000,
##                          equidist=FALSE)
## eusilcP


## ----regmodelout, warning=FALSE, message=FALSE, tidy=FALSE, echo=FALSE, eval=TRUE----
regModel = ~rb090+hsize+pl030+pb220a

# multinomial model with random draws
eusilcP <- simContinuous(eusilcP,
                         additional="netIncome", 
                         regModel = regModel, 
                         upper=200000, 
                         equidist=FALSE)
eusilcP


## ---- tidy=FALSE--------------------------------------------------------------
population <- data.frame(pop(eusilcP))
population <- population %>% 
  dplyr::select(-netIncomeCat)


## -----------------------------------------------------------------------------
library(laeken)
# true gini
g <- gini(population$netIncome)


## ---- tidy=FALSE, eval=FALSE--------------------------------------------------
## sim_d <- function(population, ...){
##   # stratified group sampling
##   # households are sampled (grouping argument)
##   samp <- samp_complete <- simFrame::draw(population,
##       design = "db040", grouping = "db030",
##       size = rep(100, 9))
##   n <- nrow(samp)
##   # insert missings
##   s <- sample(1:n, size = round(0.1*n),
##          prob = (max(samp$age) - samp$age)/ max(samp$age))
##   samp[s, "netIncome"] <- NA
##   # impute with different methods
##   samp_mean <- samp
##   samp_mean$netIncome[is.na(samp_mean$netIncome)] <-
##     mean(samp_mean$netIncome, na.rm = TRUE)
##   samp_knn <- VIM::kNN(samp, variable = "netIncome",
##     dist_var = c("db040", "age", "rb090", "pl030", "pb220a"),
##     imp_var = FALSE)
##   samp_hotdeck <- VIM::hotdeck(samp, variable = "netIncome",
##                          ord_var = c("rb090", "pl030", "pb220a"),
##                          domain_var = "db040", imp_var = FALSE)
##   # evaluate bias of gini (true minus estimated)
##   bias_mean <- g$value - laeken::gini(samp_mean$netIncome,
##                                    weights = samp$.weight)$value
##   bias_knn <- g$value - laeken::gini(samp_knn$netIncome,
##                                    weights = samp$.weight)$value
##   bias_hotdeck <- g$value - laeken::gini(samp_hotdeck$netIncome,
##                                    weights = samp$.weight)$value
##   # collect results
##   res <- c("mean" = bias_mean,
##            "VIM::kNN" = bias_knn,
##            "VIM::hotdeck" = bias_hotdeck)
##   return(res)
## }
## set.seed(1234)
## results <- replicate(25, sim_d(population))


## ---- echo=FALSE--------------------------------------------------------------
load("/Users/matthias/workspace/vimbook/sim/sim_d.rda")


## ----simd, tidy=FALSE, fig.cap='Simulation results from the design-based simulation (based on 25 replicates)', fig.pos='H'----
boxplot(t(results), 
        ylab = "Bias of the Gini coefficient")
abline(h = 0, lty = "dashed", col = "gray")


## ----load SwissLabor, echo=TRUE, message=FALSE, warning=FALSE-----------------
if (!require("AER")) install.packages("AER")
data("SwissLabor", package = "AER")

x <- SwissLabor[, c(2:(ncol(SwissLabor)), 1)]
num_vars <- 1:5
fac_vars <- 6:7
x$foreign <- factor(x$foreign)
x$participation <- factor(x$participation)

library(robCompositions)
library(caret)
library(randomForest)
library(mice)


## ----putMCAR, echo = TRUE-----------------------------------------------------
put_MCAR <- function(x, rate) {
  n <- nrow(x)
  for (i in 1:(ncol(x) - 1)) {
    x[sample(1:n, round(rate[i] * n)), i] <- NA
  }
  return(x)
}


## ----outlier detection, echo=TRUE, tidy=FALSE---------------------------------
outindex <- NULL
# outliers for each group in the data:
for(i in levels(SwissLabor$participation)){
  outindex <- c(outindex, 
    robCompositions::outCoDa(
      SwissLabor[SwissLabor$participation == i, 2:4], 
      coda = FALSE, quantile = 0.975)$outlierIndex)
}
outindex <- sort(which(outindex))


## ----mean imputation function, echo=TRUE--------------------------------------
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

meanimp <- function(x_na){
  x_imp_mean <- x_na
  for(i in num_vars){
    m <- mean(na.omit(x_na)[, i], na.rm = TRUE)
    x_imp_mean[, i][is.na(x_imp_mean[, i])] <- m
  }
  for(i in fac_vars){
    m <- Mode(na.omit(x_na)[, i])
    x_imp_mean[, i][is.na(x_imp_mean[, i])] <- m
  }
  x_imp_mean
}


## ----getstats, echo=TRUE, tidy=FALSE------------------------------------------
get_acc <- function(i, x){
  n <- nrow(x)
  # split to train and test data
  train <- sample(1:n, 0.8 * n, replace = F)
  data_train <- x[train, ]
  
  # put missings into data
  miss <- put_MCAR(data_train[-outindex, ], rep(0.1, ncol(x)-1))
  miss <- rbind(miss, x[outindex[outindex %in% train], ])
  
  # imputation
  imp_mean <- meanimp(miss)
  imp_knn <- VIM::kNN(miss, imp_var = FALSE)
  imp_mice <- mice::complete(mice(miss, printFlag = FALSE, 
                                  m = 1))
  imp_irmi <- VIM::irmi(miss, imp_var = FALSE, 
                     robust = TRUE, maxit = 3, force = TRUE)
  imp_rf <- missRanger::missRanger(miss, verbose = 0)
  
  # receive stats
  getStats <- function(imp){
    res_imp <- randomForest::randomForest(participation ~ ., 
                                          imp)
    pred_imp <- predict(res_imp, x[-train, ])     
    performance_imp <- confusionMatrix(pred_imp, 
                          x[-train, 'participation']) 
    accuracy <- performance_imp$overall['Accuracy']
    F1 <- performance_imp$byClass['F1']
    return(c(accuracy, F1))
  }
  
  res_mean <- getStats(imp_mean)
  res_knn <- getStats(imp_knn)
  res_mice <- getStats(imp_mice)  
  res_irmi <- getStats(imp_irmi)
  res_rf <- getStats(imp_rf)
  
  res <- rbind(res_mean, res_knn, res_mice, res_irmi, res_rf)
  return(res)
}


## ----get1---------------------------------------------------------------------
set.seed(123)
get_acc(x = x)


## ----setR25-------------------------------------------------------------------
R <- 25


## ----reps25, eval=FALSE-------------------------------------------------------
## set.seed(123)
## r <- replicate(R, get_acc(x = x))


## ---- echo=FALSE, cache=FALSE-------------------------------------------------
load("~/workspace/vimbook/sim/r.rda")


## ----meanreps-----------------------------------------------------------------
results <- apply(r, 1:2, mean)
results


## ----plotsimf1, fig.align='center', tidy=FALSE--------------------------------
r2 <- data.frame(apply(r, 2, c))
lab <- c("mean", "knn", "mice (pmm)", "irmi", "missForest")
r2$method <- factor(rep(lab, times = R), 
                    levels = lab, labels = lab)
r2 <- reshape2::melt(r2, id.vars = "method")
ggplot(r2, aes(x = method, y = value)) + 
  geom_boxplot() + 
  facet_wrap(~ variable) + ylab("") + xlab("") +
  coord_flip()

