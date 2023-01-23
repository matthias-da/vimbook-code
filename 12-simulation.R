#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 12: Simulation of data for simulation studies
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


library(VIM)
library(dplyr)
library(simFrame)
library(laeken)


## ----SBSpatt1, fig.cap='Missing pattern structure of the SBS5242 data set.', fig.pos='H'----
data("SBS5242", package = "VIM")
a <- aggr(SBS5242, plot = FALSE) 
plot(a, combined = TRUE)


## ----SBSpatt2, fig.cap='Missing values of the SBS5242 data set visualized by the matrixplot.', fig.pos='H'----
data("SBS5242", package = "VIM")
matrixplot(SBS5242, interactive = FALSE)




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


## -----------------------------------------------------------------------------
data(expendituresEU, package = "robCompositions")
n <- nrow(expendituresEU)
ind <- sample(1:n, 
              size = round(0.1 * n), # 10%
              prob = expendituresEU$Housing) # MAR
expendituresEU[ind, c("Food", "Restaurants")] <- NA


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

