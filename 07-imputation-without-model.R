#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 7: Imputation without a model
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


## ----loadkableExtra, message=FALSE, warning=FALSE, echo=FALSE------------------------------------------------------
library(kableExtra)


## ----hdeck, echo=FALSE, message=FALSE------------------------------------------------------------------------------
library(VIM)
set.seed(236)
n <- 10
x <- data.frame(
  "x" = round(rlnorm(n), 2),
  "y" = round(rlnorm(n), 2),
  "c1" = factor(sample(LETTERS[1:3], n, replace = TRUE)),
  "d1" = factor(sample(c("R1","R2"), n, replace = TRUE)),   
  "o1" = round(rnorm(n, 30, 15)),
  "o2" = round(rnorm(n, 5), 2))
origX <- x
x[sample(1:n, 1), 1] <- NA
x[sample(1:n, 1), 2] <- NA
x[sample(1:n, 1), 3] <- NA


library(dplyr)


## ----applyhotdeck, eval=FALSE--------------------------------------------------------------------------------------
## hotdeck(x, imp_var = FALSE)


## ----applyhotdeckd1, eval=FALSE, tidy=FALSE------------------------------------------------------------------------
## hotdeck(x,
##         domain_var = "d1",
##         imp_var = FALSE)


## ----applyhotdeckd1_vis, echo=FALSE--------------------------------------------------------------------------------
domain_hotdeck <- hotdeck(x, domain_var = "d1") %>% arrange(d1)
domain_hotdeck_small <- domain_hotdeck[, names(x)]


## ----applyhotdeckcond_vis, echo=FALSE------------------------------------------------------------------------------
domain_cond_hotdeck <- hotdeck(x, 
         variable = "x",
         domain_var = "d1", 
         donorcond = list(">0.7")) %>% arrange(d1)
domain_cond_hotdeck_small <- domain_cond_hotdeck[, names(x)]

## ----applyhotdeckord, eval=FALSE, tidy=FALSE-----------------------------------------------------------------------
## hotdeck(x,
##         domain_var = "d1",
##         ord_var = "o1",
##         imp_var = FALSE)


## ----applyhotdeckord_vis, echo=F-----------------------------------------------------------------------------------
domain_sort_hotdeck <- hotdeck(x, domain_var = "d1", ord_var = c("o1")) %>% group_by(d1) %>% 
  arrange(desc(o1)) %>% arrange(d1) 
domain_sort_hotdeck_small <- domain_sort_hotdeck[, names(x)]

## ----hotdeck,eval=TRUE,cache=TRUE,warning=FALSE,message=FALSE,tidy=FALSE-------------------------------------------
require("VIM")
require("simFrame")
require("dplyr")
data("eusilcP")
pop <- eusilcP %>% 
  dplyr::select(region, gender, hsize, age, eqIncome, 
         ecoStat, citizenship, py010n, py050n, py090n) %>% 
  filter(age > 15 & age < 65) 
pop %>% dim
# generate missings: (10%)
for(v in c("ecoStat", "citizenship", "py010n", "py050n", 
           "py090n")){
  pop[sample(1:nrow(pop), round(nrow(pop)/10), 
             replace = TRUE), v] <- NA
}
system.time(popImp <- hotdeck(pop,
  ord_var = c("eqIncome", "age", "hsize"),
  domain_var=c("region", "gender")))


## ----pboxafterhotdeck, echo=FALSE, eval=FALSE----------------------------------------------------------------------
## pbox(popImp, del="_imp", pos=5)


## ----vis_gower,eval=TRUE,cache=TRUE,warning=FALSE,message=FALSE, echo=F--------------------------------------------
ranges <- x[1,]
ranges[1, ] <- c(max(x$x, na.rm = T) - min(x$x, na.rm = T),
            max(x$y, na.rm = T) - min(x$y, na.rm = T),
            NA, NA, 
            max(x$o1, na.rm = T) - min(x$o1, na.rm = T),
            max(x$o2, na.rm = T) - min(x$o2, na.rm = T))
row.names(ranges) <- 'range'
g_data <- x[c(1, 2), ]

g_dist  <- abs((g_data[1,] - g_data[2, ]) / as.numeric(ranges[1,]))
g_dist$c1 <- as.numeric(g_data$c1[1] == g_data$c1[2])
g_dist$d1 <- as.numeric(g_data$d1[1] == g_data$d1[2])
row.names(g_dist) <- 'delta'

Gower_dist <- rowMeans(g_dist)
names(Gower_dist) <- "Gower distance"


## ----vis_gover_1, message=F----------------------------------------------------------------------------------------
x


## ----vis_gover_2, message=F----------------------------------------------------------------------------------------
ranges


## ----vis_gover_3, message=F----------------------------------------------------------------------------------------
g_data


## ----vis_gover_4, message=F----------------------------------------------------------------------------------------
g_dist


## ----vis_gover_5, message=F----------------------------------------------------------------------------------------
Gower_dist


## ----gower-dist-data, message=FALSE, warning=FALSE, include=FALSE, tidy=FALSE--------------------------------------
library(gower)
library(MASS)
set.seed(1234)
demo.data <- mvrnorm(n = 10, mu = c(5,5), Sigma = matrix(c(1,0.8,0.8,2),ncol=2))
demo.data <- cbind(c("1":"10"),demo.data)
colnames(demo.data) <- c("","x1", "x2")


library(MASS)
demo.data.miss <- demo.data
set.seed(12)
demo.data.miss[sample(x = c(1:10), 1),3] <- NA



knn.data <- demo.data.miss
max <- max(knn.data[,2])
min <- min(knn.data[,2])
r <- max - min
gowers.dist <- abs(knn.data[1,2]- knn.data[-1,2]) /r
knn.data <- cbind(knn.data,"Gowers Dist." = c(NA,gowers.dist))

knitr::kable(
  head(knn.data, 10), caption = 'Dataset with the Gower Distances',
  booktabs = TRUE
)



## ----knn-demo, message=FALSE, warning=FALSE, include=FALSE, tidy=FALSE---------------------------------------------
library(gower)
knitr::kable(
  head(knn.data[c(5,6,7,8,9),], 5), caption = 'Dataset with the Gower Distances',
  booktabs = TRUE
)


data("eusilcP")
eusilcP <- eusilcP[eusilcP$age > 15 & eusilcP$age < 65, ]
samp <- eusilcP[sample(1:nrow(eusilcP), 14e3, replace = FALSE),
                c("region", "gender", "hsize", "age", 
                  "eqIncome", "ecoStat", "citizenship", 
                  "py010n", "py050n", "py090n")]
for(v in c("ecoStat", "citizenship", "py050n", "py090n", 
           "py010n")){
  samp[sample(1 : nrow(samp), round(nrow(samp)/10),
              replace = TRUE), v] <- NA
}
samp %>% dim
medianMixed <- function(x){
  nr <- sum(x == 0) / length(x)
  out <- sample(c(0, 1), size = 1,prob = c(nr, 1 - nr))
  if(out == 0 || nr == 1)
    return(0)
  else
    return(median(x[x != 0]))
}

s_knn <- system.time(sampImp <- kNN(pop,
                                    dist_var = c("eqIncome", "age", "hsize", "region", "gender"),
                                    k = 5,
                                    numFun = medianMixed))

data(eusilc)
eusilc <- eusilc %>% 
  filter(age > 17) %>%
  dplyr::select(hsize, db040, age, rb090, pl030, pb220a,
                py010n, eqIncome)
set.seed(123)
setMiss_silc <- function(eusilc, ni = 0.05){
  eusilc <- eusilc %>% filter(age > 17) 
  eusilcfull <- eusilc
  n <- nrow(eusilc)
  ni <- round(ni * nrow(eusilc))
  # set missings in db040  
  eusilc$db040[sample(1:n, size = ni, replace = FALSE, prob = eusilcfull$age * as.integer(eusilcfull$pl030))] <- NA
  # set missings in age
  modage <- lm(age ~ log(eqIncome + 1)*db040 + log(eqIncome + 1)*pl030 + pb220a*pl030, data = eusilcfull)
  eusilc$age[sample(1:n, size = ni, replace = FALSE, prob = resid(modage) - min(resid(modage)))] <- NA
  # set missings in rb090 (gender)
  modgender <- lm(as.integer(rb090) ~ db040 + hsize + log(eqIncome + 1)*pl030, data = eusilcfull)  
  eusilc$rb090[sample(1:n, size = ni, replace = FALSE, prob = resid(modgender) - min(resid(modgender)))] <- NA  
  # set missings in pl030
  modpl030 <- lm(as.integer(pl030) ~ age*pb220a + py010n, data = eusilcfull)
  eusilc$pl030[sample(1:n, size = ni, prob = resid(modpl030) - min(resid(modpl030)))] <- NA
  # set missings in pb220a
  modpb220a <- lm(as.integer(pb220a) ~ log(eqIncome + 1)*db040 + hsize + rb090 + age, data = eusilcfull)
  eusilc$pb220a[sample(1:n, size = ni, prob = resid(modpb220a) - min(resid(modpb220a)))] <- NA
  # set missings in eqIncome
  modincome <- lm(log(eqIncome + 1) ~ age + I(age^2) + pb220a + pl030 + db040*hsize, data = eusilcfull)
  eusilc$eqIncome[sample(1:n, size = ni, prob = resid(modincome) - min(resid(modincome)))] <- NA
  return(eusilc)
}
x <- setMiss_silc(eusilc)

# weights equal 1
x_knn <- kNN(x, dist_var = c("hsize", "db040", "age", 
                             "rb090", "pl030", "pb220a", 
                             "eqIncome"))
error_weights1 <- nrmse(eusilc$eqIncome, x_knn$eqIncome, 
                        x_knn$eqIncome_imp)

# weights equal auto
x_knn_wrf <- kNN(x, 
                 dist_var = c("hsize", "db040", "age", "rb090",
                              "pl030","pb220a","eqIncome"),
                 weights = "auto")
error_weightsrf <- nrmse(eusilc$eqIncome, x_knn_wrf$eqIncome, 
                         x_knn_wrf$eqIncome_imp)

error_weights1
error_weightsrf

# draw one of the donors randomly
draw1 <- function(x){
  sample(x, 1)
}
# impute 
sampImp <- kNN(samp, 
               dist_var = c("eqIncome", "age", "hsize", 
                            "region", "gender"), 
               k = 5, 
               numFun = draw1)


# draw one of the donors randomly
mean_rweighted <- function(x){
  w <- runif(1:length(x))
  w <- w / sum(w)
  weighted.mean(x, w)
}
# imputation
sampImp <- kNN(samp, 
               dist_var = c("eqIncome", "age", "hsize", 
                            "region", "gender"), 
               k = 5, 
               numFun = mean_rweighted)

## ------------------------------------------------------------------------------------------------------------------
data("Animals", package = "MASS")
df <- data.frame((log(Animals)))
cor(df)
robustbase::covMcd(df, cor = TRUE)$cor

## explanations of missMDA::imputePCA, missMDA::MIPCA, VIM::impPCA, VIM::impPCA(..., robust = "mcd")
## can be found in the book
