#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 11: Evaluation of the quality of imputation
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3

library(VIM)

## ----imputedaggr1, fig.cap='Aggregation graphic of the partially imputed data set sleep.', fig.align='center'------
data(sleep, package = "VIM")
sleep_impknn <- kNN(sleep, variable = c("NonD", "Dream"))
aggr(sleep_impknn, delimiter = "_imp")


## ----imputedaggr2, fig.cap='Customized aggregation plots of the partially imputed data set sleep - the combined aggregation graphic.'----
a <- aggr(sleep_impknn, delimiter = "_imp", plot = FALSE)
plot(a, combined = TRUE, sortVars = TRUE)


## ----denimp, fig.cap='Density of complete data in comparison with the densities of knn and arithmetic mean imputed data.', tidy=FALSE----
df <- data.frame(
  "den_complete" = density(sleep$Dream[!is.na(sleep$Dream)])$y,
  "den_knn" = density(sleep_impknn$Dream)$y,
  "den_mean" = density(e1071::impute(sleep)[, "Dream"])$y,
  "Dream" = density(e1071::impute(sleep)[, "Dream"])$x
)
df <- reshape2::melt(df, id.var = "Dream")
str(df, 2, vec.len = 1)
library(scales)
ggplot(df, aes(x = Dream, y = value, colour = variable)) + 
  geom_line(size = 1.2, alpha = 0.7)


## ----histplot, echo=FALSE------------------------------------------------------------------------------------------
sleep_impmean <- e1071::impute(sleep)
m <- is.na(sleep_impmean)
colnames(m) <- paste0(colnames(m), "_imp")
sleep_impmean <- cbind(sleep_impmean, m)


## ----tofactor, eval=FALSE, echo=FALSE------------------------------------------------------------------------------
## data(tao)
## tao$Year <- factor(tao$Year)
## tao$Latitude <- factor(tao$Latitude)
## tao$Longitude <- factor(tao$Longitude)


## ----imputedhist, fig.cap="Histogram of the variable Air.Temp of the data set tao, imputed values in Humidity are highlighted"----
#data(tao)
tao_imp <- kNN(tao)
par(mfrow = c(2,2))
spineMiss(tao_imp, delimiter = "_imp", pos = 6, selection="any")
histMiss(tao_imp, delimiter = "_imp", pos = 6, selection="any")
spineMiss(tao, pos = 6, selection = "any")
histMiss(tao, pos = 6, selection = "any")


## ----taomarginplot, fig.cap='Marginplot of the variables Air.Temp and Humidity of the tao data set', tidy=FALSE----
marginplot(tao_imp[, c("Humidity", "Air.Temp", "Humidity_imp",
                       "Air.Temp_imp")], delimiter = "_imp",
          col = c("darkgrey","orange","magenta"), pch = c(20), 
          alpha = 0.5, cex = 1.2)


## ----marginplotcomparison, fig.cap='Marginplots of the variables Air.Temp and Humidity of the tao data set, imputed with different imputation methods', tidy=FALSE----
tao_impmean <- tao
tao_impmean[, 4:ncol(tao)] <- e1071::impute(tao[, 4:ncol(tao)] )
m <- is.na(tao)
colnames(m) <- paste0(colnames(m), "_imp")
tao_impmean <- cbind(tao_impmean, m)

set.seed(122)
tao_impmice <- mice::mice(tao, m = 1)
tao_impmice <- mice::complete(tao_impmice)
tao_impmice <- cbind(tao_impmice, m)

tao_impmrf <- missForest::missForest(tao, 
                            verbose = FALSE)$ximp
tao_impmrf <- cbind(tao_impmrf, m)

tao_impirmi <- irmi(tao, robust = TRUE)

vars <- c("Humidity", "Air.Temp", 
          "Humidity_imp", "Air.Temp_imp")
cols <- c("darkgrey","orange","magenta")
par(mfrow = c(2,2), mar = c(4,4,2,0.2))
marginplot(tao_impmean[, vars], delimiter = "_imp",
          col = cols, pch = c(20), alpha = 0.5, 
          cex = 1.2, main = "mean imputation")
marginplot(tao_impmice[, vars], delimiter = "_imp",
          col = cols, pch = c(20), alpha = 0.5, 
          cex = 1.2, main = "mice")
marginplot(tao_impmrf[, vars], delimiter = "_imp",
          col = cols, pch = c(20), alpha = 0.5, 
          cex = 1.2, main = "missForest")
marginplot(tao_impirmi[, vars], delimiter = "_imp",
          col = cols, pch = c(20), alpha = 0.5, 
          cex = 1.2, main = "irmi")


## ----marginmatrixrf, fig.cap="Margin scatterplot matrix of all variables of the tao data set, imputed with missForest."----
marginmatrix(tao_impmrf, delimiter = "_imp")


## ----marginmatrixirmi, fig.cap="Margin scatterplot matrix of all variables of the tao data set, imputed with irmi."----
marginmatrix(tao_impirmi, delimiter = "_imp")


## ----parcoordimp, tidy=FALSE, fig.width=10, fig.height=6-----------------------------------------------------------
par(mfrow = c(1,2), mar = c(7,0.2,0.2,0.2))
parcoordMiss(tao_impmrf, delimiter = "_imp", 
             selection = "any", plotNA = FALSE, 
             alpha = 0.5)
parcoordMiss(tao_impirmi, delimiter = "_imp", 
             selection = "any", plotNA = FALSE, 
             alpha = 0.5)


## ----pcamean, fig.cap="Biplot of the pricipal component analysis. Results from imputation with the arithmetic mean. Observations with imputed values are highlighted in red. Different symbols according to the combination of latitude and year. ", tidy=FALSE----
# combination of year and latitude for shape symbols
strata <- apply(tao[, c("Year", "Latitude")], 1, 
                paste, collapse = "_")
shapes <- factor(strata)
levels(shapes) <- c(1, 16, 2, 24, 6)
shapes <- as.numeric(as.character(shapes))

# for highlighting imputed values
imps <- ifelse(apply(tao, 1, anyNA), 2, 1)

# PCA of imputed data
pr <- princomp(tao_impmean[, 4:8], cor = TRUE)

# Biplot
library(ggfortify)
autoplot(pr, col = imps, shape = shapes, 
         loadings = TRUE, loadings.label = TRUE, alpha = 0.9) + 
  theme_bw()  + 
  labs(title = "mean imputation") 


## ----pcarf, echo=FALSE---------------------------------------------------------------------------------------------
pr <- princomp(tao_impmrf[, 4:8], cor = TRUE)
p1 <- autoplot(pr, col = imps, shape = shapes, 
         loadings = TRUE, loadings.label = TRUE, alpha = 0.7) + 
  theme_bw() + 
  labs(title = "missForest") 


## ----princomptao, echo=FALSE, eval=TRUE----------------------------------------------------------------------------
pr <- princomp(tao_impirmi[, 4:8], cor = TRUE)
p2 <- autoplot(pr, col = imps, shape = shapes, 
         loadings = TRUE, loadings.label = TRUE, alpha = 0.7) + 
  theme_bw() + 
  labs(title = "IRMI") 


## ----princomptao2, echo=FALSE, eval=TRUE---------------------------------------------------------------------------
pr <- princomp(tao_impmice[, 4:8], cor = TRUE)
p3 <- autoplot(pr, col = imps, shape = shapes, 
         loadings = TRUE, loadings.label = TRUE, alpha = 0.7) + 
  theme_bw() + 
  labs(title = "PMM (mice)") 
# set.seed(122)
# tao_impmice2 <- mice::mice(tao, m = 1, method = "midastouch")
# tao_impmice2 <- mice::complete(tao_impmice)
# tao_impmice2 <- cbind(tao_impmice, m)
# pr <- princomp(tao_impmice2[, 4:8], cor = TRUE)
# autoplot(pr, col = imps, shape = shapes, 
#          loadings = TRUE, loadings.label = TRUE)
library(missMDA)
tao_imppca <- missMDA::imputePCA(tao)$completeObs
pr <- princomp(tao_imppca[, 4:8], cor = TRUE)
p4 <- autoplot(pr, col = imps, shape = shapes, 
         loadings = TRUE, loadings.label = TRUE, alpha = 0.7) + 
  theme_bw() +
  labs(title = "imputePCA") 


## ----biplotsremaining, message=FALSE, warning=FALSE, echo=FALSE, fig.cap='Biplots of the first two principal components. Observations with imputed values are highlighted in red.', echo=FALSE, out.width='100%', fig.width=9, fig.height=8----
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)


## ----pcacc, fig.cap="Biplot of the pricipal component analysis. Results from imputation with various methods compared with the full data set (top left plot). Observations that are set to missings and imputed are highlighted in red.", tidy=FALSE, echo=FALSE, cache=FALSE----
data("Prestige", package = "carData")
library(mice)
library(VIM)
library(missMDA)
library(robustbase)
library(ggfortify)
library(missForest)
library(gridExtra)
# set some missings (MAR) in alcohol
set.seed(123) 
prob <- Prestige$income / sum(Prestige$income)
Prestige_na <- Prestige
Prestige_na$prestige[sample(1:nrow(Prestige),
                            size = 25, prob = prob)] <- NA
imps <- ifelse(is.na(Prestige_na$prestige), 2, 1)

# impute
Prestige_imp_mice <- mice::complete(mice(Prestige_na, 
                              m = 1, printFlag = FALSE))
Prestige_imp_irmi <- irmi(Prestige_na, 
                              mi = 1, imp_var = FALSE)
Prestige_imp_rf <- missForest(Prestige_na, 
                                  verbose = FALSE)$ximp
Prestige_imp_pca <- imputePCA(Prestige_na[, 1:4])$completeObs
Prestige_imp_knn <- kNN(Prestige_na, imp_var = FALSE)


# PCA of original complete data
pr <- princomp(Prestige[, 1:4], cor = TRUE)

# PCA of imputed data
pr_imp_mice <- princomp(Prestige_imp_mice[, 1:4], cor = TRUE)
pr_imp_irmi <- princomp(Prestige_imp_irmi[, 1:4], cor = TRUE)
pr_imp_rf <- princomp(Prestige_imp_rf[, 1:4], cor = TRUE)
pr_imp_pca <- princomp(Prestige_imp_pca[, 1:4], cor = TRUE)
pr_imp_knn <- princomp(Prestige_imp_knn[, 1:4], cor = TRUE)

# Biplot
library(ggfortify)
p0 <- autoplot(pr, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "full data set") 
p1 <- autoplot(pr_imp_rf, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "missForest") 
p2 <- autoplot(pr_imp_irmi, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "irmi") 
p3 <- autoplot(pr_imp_mice, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "PMM (mice)") 
p4 <- autoplot(pr_imp_pca, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "imputePCA") 
p5 <- autoplot(pr_imp_knn, loadings = TRUE, 
               loadings.label = TRUE, alpha = 0.5, 
               col = imps) + 
  theme_bw()  + 
  labs(title = "kNN") 


## ----biplotscc, message=FALSE, warning=FALSE, echo=FALSE, fig.cap='Biplots of the first two principal components. Observations with imputed values are highlighted in red.', echo=FALSE, cache = FALSE, out.width='100%', fig.width=9, fig.height=10----
grid.arrange(p0, p1, p2, p3, p4, p5, ncol = 2)



## ----ameliaoverimpute, fig.cap="Overimputation diagnostics with package Amelia.", tidy=FALSE-----------------------
library(Amelia)
# sometimes crahses for the sleep data  
# and gives wrong results   
# res.amelia <- amelia(sleep, m = 15)   
# overimpute(res.amelia, var = "Sleep")  
# We use the tao data set  
res.amelia <- amelia(tao, m = 15, p2s = 0) 
overimpute(res.amelia, var = "Air.Temp")


## ---- echo=TRUE, eval=FALSE, tidy=FALSE----------------------------------------------------------------------------
## library(tourr)
## # In case you use RStudio, use this:
## # opens a graphics window outside of RStudio
## x11()
## 
## ?animate_dist
## ?animate_xy
## ?animate_pcp
## 
## NAinfo <- as.integer(is.na(Prestige_na$prestige)) + 1
## animate_xy(Prestige_imp_mice[, 1:4], col = NAinfo)



## ----mse1, cache=FALSE, eval=TRUE, tidy=FALSE----------------------------------------------------------------------
# utility function to have probabilities between 0 and 1
range01 <- function(x, ...){
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}
# data and MAR missing value creation
simData <- function(n = 100, truemean = 10){
  x <- mvrnorm(n, 
               mu = c(truemean,truemean), 
               Sigma = matrix(c(1, 0.9, 0.9, 1), 
                              ncol = 2))
  x[sample(1:100, 20, prob = range01(x[, 2])), 1] <- NA
  data.frame(x)
}


## ----mse2-save, cache = FALSE, eval=FALSE, tidy=FALSE, echo=FALSE--------------------------------------------------
library(MASS)
library(mice)
library(missRanger)
library(VIM)

set.seed(1234)

s <- matrix(c(1, 0.9, 0.9, 1), ncol = 2)
truemean <- 10

# function to impute and estimate
fun <- function(){
  x <- simData(truemean = 10)
  c("mean_cc" = mean(na.omit(x)[,1]),
    "mean_knn" = mean(VIM::kNN(x)[, 1]),
    "mean_rf" = mean(missRanger::missRanger(x, verbose=0)[,1]),
    "mean_mice_pmm" = mean(complete(mice::mice(x,
                        printFlag = FALSE))[,1]),
    "mean_irmi" = mean(VIM::irmi(x)[, 1]))
}

# simulate
x <- replicate(1000,
               fun())




## ----mse2, cache = FALSE, eval=TRUE, tidy=FALSE--------------------------------------------------------------------
bias <- rowMeans(x) - truemean
bias
# percent bias
100 * abs(bias / truemean)
# variance
v <- apply(x, 1, var)
v
# MSE
v + bias^2
# RMSE
sqrt(v + bias^2)


## ----mse3, cache = FALSE, eval=FALSE, echo=FALSE-------------------------------------------------------------------
simData <- function(n = 100, truemean = 10){
  x <- mvrnorm(n - 10,
               mu = c(truemean,truemean),
               Sigma = matrix(c(1, 0.9, 0.9, 1),
                              ncol = 2))
  x <- rbind(x, mvrnorm(5, mu = c(20,10), Sigma = s))
  x <- rbind(x, mvrnorm(5, mu = c(10,20), Sigma = s))
  x[sample(1:(n-10), 20, prob = range01(x[1:(n-10), 2])), 1] <- NA
  data.frame(x)
}
s <- matrix(c(1, 0.9, 0.9, 1), ncol = 2)
truemean <- 10
# simulate
fun_out <- function(){
                 x <- simData(truemean = 10)
                 c("mean_cc" = mean(na.omit(x[1:(n-10), ])[,1]),
                   "mean_knn" = mean(VIM::kNN(x)[1:(n-10), 1]),
                   "mean_rf" = mean(missRanger::missRanger(x, verbose = 0)[1:(n-10),1]),
                   "mean_mice_pmm" = mean(complete(mice::mice(x, printFlag = FALSE))[1:(n-10),1]),
                   "mean_irmi" = mean(VIM::irmi(x, robust = TRUE)[1:(n-10), 1]))
}

set.seed(123)
simOut <- replicate(100,
               fun())


## ----mse5a, cache = TRUE, eval=TRUE, echo=FALSE--------------------------------------------------------------------
truemean <- 10
bias <- rowMeans(simOut) - truemean
# percent bias
bias_perc <- 100 * abs(bias / truemean)
# variance
v <- apply(simOut, 1, var)
# MSE
mse <- v + bias^2
# RMSE
rmse <- sqrt(v + bias^2)
df <- round(cbind(bias, bias_perc, v, mse, rmse), 3)
df <- data.frame(df)
rownames(df) <- c("cc", "kNN", "rf", "mice-pmm","irmi-robust")
colnames(df) <- c("bias", "bias (percent)", "variance", "MSE", "RMSE")
kableExtra::kable(df, caption = "Bias, variance and (root) mean squared error of estimates after deletion (complete case (cc) analysis) and imputation with some well-known methods.", label = "methodsOut")



## ----genoutlier, tidy=FALSE----------------------------------------------------------------------------------------
gen_outlier <- function(n_outlier, mu, sigma){
  mvrnorm(n_outlier, 
    mu = colMeans(x, na.rm = T) + diag(cov(na.exclude(x))) * 
      sample(list(c(1, 1), c(-1, 1), 
                  c(1, -1), c(-1, -1)), 1)[[1]] * 0.8,
    Sigma = cov(na.exclude(x))/5)
  }

