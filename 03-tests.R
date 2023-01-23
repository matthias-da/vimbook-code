#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2013
###   Chapter 2: Distribution, pre-analysis of missing values and data quality.
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


## ----matrixplot2var, message=FALSE, warning=FALSE, fig.cap="Matrixplot of log10(Dream) and log10(BodyWgt) of the sleep data set. Missing are in red while any other value of the data matrix is in grey, whereby the grey scale depends on the magnitude of the value.", tidy=FALSE----
library(VIM)
data(sleep, package = "VIM")
matrixplot(log10(sleep[, c("Dream", "BodyWgt")]), 
           sortby = "BodyWgt")


## ----ttest-------------------------------------------------------------------------------------------
sleep$mDream <- factor(is.na(sleep$Dream))
table(sleep$mDream)


## ----ttest2, tidy = FALSE, echo = TRUE, eval=FALSE---------------------------------------------------
## t.test(BodyWgt ~ mDream, data = sleep)


## ----ttest2show, tidy = FALSE, echo=FALSE, eval=TRUE-------------------------------------------------
t.test(BodyWgt ~ mDream, data = sleep)


## ----wilcoxtest, echo=TRUE---------------------------------------------------------------------------
wilcox.test(BodyWgt ~ mDream, data = sleep)


## ----glmresprint, eval = TRUE, echo=FALSE, warning=FALSE, message=FALSE, tidy=FALSE------------------
x <- na.omit(sleep[, c("BodyWgt","BrainWgt","mDream")])
x <- sleep[, c("BodyWgt","BrainWgt","mDream")]
res <- glm(mDream ~ Span + Gest + log(BodyWgt) + log(BrainWgt) + 
         Danger + Exp + Pred, 
         data = sleep, 
         family = binomial(), 
         na.action = "na.omit")
summary(res)


## ----sleeppatternsoutput, echo = FALSE, tidy=FALSE---------------------------------------------------
data(sleep, package = "VIM")
summary(aggr(sleep[, 1:7], plot = FALSE))
# or alternatively with a lot of more information
# (we only display a part of the information)
s <- robCompositions::missPatterns(sleep[, 1:7])
s$groups


## ----pattern6----------------------------------------------------------------------------------------
s$groups$`0:0:1:1:0:0:0`


## ----obs6--------------------------------------------------------------------------------------------
sleep[s$groups$`0:0:1:1:0:0:0`, 1:7]


## ----mu6, tidy=FALSE---------------------------------------------------------------------------------
means_pattern6 <- colMeans(sleep[s$groups$`0:0:1:1:0:0:0`, 
                    c(1:2,5:7)])
means_pattern6


## ----cvest, message=FALSE, warning=FALSE-------------------------------------------------------------
library(monomvn)
cv_est <- monomvn(sleep[, 1:7])
#cv_est <- mlest(sleep[, 1:7])
means_full <- cv_est$mu
means_full
cv_full <- cv_est$S
cv_full


## ----littletest, message=FALSE, warning=FALSE, results='hide', eval = TRUE---------------------------
library(BaylorEdPsych)
p.val <- LittleMCAR(sleep[, 1:7])$p.val


## ----littletestres, message=FALSE, warning=FALSE-----------------------------------------------------
p.val


## ----testlittle, message=FALSE, warning=FALSE, cache = FALSE, tidy=FALSE-----------------------------
createMVNdata <- function(out = FALSE){
  s <- diag(5)
  s[upper.tri(s)] <- 0.9
  s[lower.tri(s)] <- 0.9
  x <- data.frame(MASS::mvrnorm(100, mu = rep(10, 5), 
                                Sigma = s))
  for(i in 1:5){ # 10% missing values with MCAR
    x[sample(1:100, 10), i] <- NA
  }
  if(out){ # optionally outliers
    x[1,] <- 1000 * x[1,]
    x[2,1] <- 1000 * x[2, ]
  }
  return(x)
}


## ----testlittleres, message=FALSE, warning=FALSE, cache = FALSE--------------------------------------
set.seed(123)
BaylorEdPsych::LittleMCAR(createMVNdata())$p.val


## ----testlittle2, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE-------------
sizeOfTest <- mean(replicate(500, 
  try(LittleMCAR(createMVNdata())$p.val)) < 0.05)


## ----sizeOfTest, testlittle3-------------------------------------------------------------------------
sizeOfTest


## ----logtest2, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE----------------
sizeOfTest_log <- mean(replicate(1000, 
  try(LittleMCAR(log(createMVNdata()))$p.val)) < 0.05)

## ----logtest3----------------------------------------------------------------------------------------
sizeOfTest_log


## ----logtest4, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE----------------
sizeOfTest_out <- mean(replicate(1000,
  try(LittleMCAR(log(createMVNdata(out=TRUE)))$p.val)) < 0.05)

## ----logtest5----------------------------------------------------------------------------------------
sizeOfTest_out


## ----missmechfirstoutput, cache = TRUE, echo=FALSE, eval=TRUE----------------------------------------
options(width=50)
library(MissMech)
TestMCARNormality(sleep[, c(1:4, 6:7)])


## ----missmech0, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE---------------
sizeOfTest_out <- mean(replicate(1000,   
  try(TestMCARNormality(createMVNdata())$pvalcomb)) < 0.05)

## ----missmech02--------------------------------------------------------------------------------------
sizeOfTest_out


## ----missmech00, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE--------------
sizeOfTest_out <- mean(replicate(1000,   
  try(TestMCARNormality(createMVNdata(),   
    method = "Nonparametric",   
    imputation.method = "Dist.Free")$pnormality)) < 0.05)

## ----missmech002-------------------------------------------------------------------------------------
sizeOfTest_out


## ----missmech1, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE---------------
sizeOfTest_out <- mean(replicate(500,   
  try(TestMCARNormality(log(createMVNdata()))$pvalcomb)) < 0.05)

## ----missmech2---------------------------------------------------------------------------------------
sizeOfTest_out


## ----missmech3, message=FALSE, warning=FALSE, cache = TRUE, results='hide', tidy=FALSE---------------
sizeOfTest_out <- mean(replicate(500, 
  try(TestMCARNormality(log(createMVNdata()), 
        method = "Nonparametric", 
        imputation.method = "Dist.Free")$pnormality)) < 0.05)

## ----missmech4---------------------------------------------------------------------------------------
sizeOfTest_out

