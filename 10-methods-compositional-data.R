#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 10: Methods for compositional data
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3

library(VIM)

## ----load package, echo=FALSE, results='hide'----------------------------------------------------------------------
library("robCompositions")
constSum <- function(x, const=1){
	x / rowSums(x) * const
}


## ----genDataCoDa2, echo=FALSE, results='hide'----------------------------------------------------------------------
genData <- function(n=1000, out=0.05, 
            Sigma=1*c(1,1)%*%t(c(1,1))+0.05*c(1,-1)%*%t(c(1,-1))){
    ## Gruppe ohne Ausreisser:
    z <- mvrnorm(n, mu=c(0,0), Sigma=Sigma)
    N <- dim(z)[1]
    n1 <- N - floor(n*out)
    n2 <- N - floor(2*n*out)
    if(out > 0){
      z[(n1+1):N, ] <- mvrnorm(floor(n*out), mu=c(0,6), Sigma=Sigma) ## erste Ausreissergruppe (Euclidean+Aitchison)
    }
    z <- isomLRinv(z) #ilr.inv(z)
    sum=runif(n1,0,1)  #rnorm(n1,10,1)
    z[1:n1, ] <- z[1:n1,] * sum
    if(out > 0){ 
      sum1=runif(floor(2*n*out),13,17) #rnorm(n2-n1,15,1)
      z[(n2+1):N, ] <- z[(n2+1):N, ] * sum1
      z[(n1+1):n2, ] <- z[(n1+1):n2, ] * 10
    }
    ## generate missings
    zmiss <- z
    s <- c(0.2, 0.1, 0.05, 0.05)
    for(i in 1:ncol(z)){
      zmiss[sample(1:n2, floor(s[i]*n2)), i] <- NA #1:index
    }
    list(zmiss=data.frame(zmiss), z2=data.frame(z), good=n2)
}


## ----seed, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------
set.seed(123)
library("MASS")


## ----new data, echo=FALSE------------------------------------------------------------------------------------------
x <- genData(100)


## ----plot.acomp, echo=FALSE----------------------------------------------------------------------------------------
plot.acomp <- 
function (x, ..., labels = colnames(X), cn = colnames(X), aspanel = FALSE,
    id = FALSE, idlabs = NULL, idcol = 2, center = FALSE, scale = FALSE,
    pca = FALSE, col.pca = par("col"), margin = "acomp", add = FALSE,
    triangle = !add, col = par("col"),
    cexT=1.5, 
    adj=-1    
    )
{
    col <- unclass(col)
    X <- oneOrDataset(x)
    oX <- X
    s60 <- sin(pi/3)
    c60 <- cos(pi/3)
    if (NCOL(X) > 3) {
        if (margin == "rcomp")
            infkt <- function(x, y, ...) {
                plot.acomp(rcompmargin(X, d = c(gsi.mapfrom01(x),
                  gsi.mapfrom01(y)), pos = 1)[, c(3, 2, 1)],
                  ..., aspanel = TRUE, center = center, scale = scale,
                  col = col)
            }
        else if (margin == "acomp") {
            infkt <- function(x, y, ...) {
                plot.acomp(acompmargin(X, d = c(gsi.mapfrom01(x),
                  gsi.mapfrom01(y)), pos = 1)[, c(3, 2, 1)],
                  ..., aspanel = TRUE, center = center, scale = scale,
                  col = col)
            }
        }
        else {
            if (!is.numeric(margin))
                margin <- match(margin, colnames(X))
            fest <- X[, margin, drop = FALSE]
            X <- X[, -margin]
            infkt <- function(x, y, ...) {
                plot.acomp(acomp(cbind(X[, c(gsi.mapfrom01(y),
                  gsi.mapfrom01(x))], fest)), ..., aspanel = TRUE,
                  center = center, scale = scale, col = col)
            }
        }
        nn <- NCOL(X)
        if (add)
            gsi.add2pairs(sapply(1:NCOL(X), gsi.mapin01), infkt,
                ...)
        else gsi.pairs(sapply(1:NCOL(X), gsi.mapin01), labels = labels,
            panel = infkt, ...)
    }
    else {
        if (is.null(cn)) {
            cn <- c(expression(x[1]), expression(x[2]), expression(x[3]))
        }
        if (aspanel) {
            usr <- par("usr")
            on.exit(par(usr))
            par(usr = c(0, 1, 0, 1), pty = "s")
            lines(x = c(0, c60, 1, 0), y = c(0, s60, 0, 0))
            text(0, 0.2, cn[1], pos = 4, offset = 0.01, xpd = TRUE, cex=2)
            text(1, 0.2, cn[2], pos = 2, offset = 0.01, xpd = TRUE, cex=2)
            text(0.5, s60, cn[3], pos = 3, offset = 0.01, xpd = TRUE, cex=2)
        }
        else {
            if (!add) {
                usr <- par("pty")
                on.exit(par(usr))
                par(pty = "s")
                plot(x = c(0, c60, 1, 0), y = c(0, s60, 0, 0),
                  xlim = c(0, 1), ylim = c(0, 1), type = "n",
                  xlab = "", ylab = "", axes = FALSE)
                gsi.plots[[dev.cur()]] <<- NULL
            }
            if (triangle) {
                segments(x0 = c(0, 1, c60), y0 = c(0, 0, s60),
                  x1 = c(1, c60, 0), y1 = c(0, s60, 0))
                mtext(cn[1], side = 1, adj = 0, padj=adj, line = 1.5, cex=cexT)
                mtext(cn[2], side = 1, adj = 1, padj=adj , line = 1.5, cex=cexT)
                text(0.5, s60 * 1.03, cn[3], pos = 3, offset = 0.01,
                  xpd = TRUE, cex=cexT)
            }
        }
        X <- acomp(X, c(1, 2, 3))
        Y <- scale.acomp(X, center = center, scale = scale)
        gsi.setCoorInfo(mean = if (center)
            -mean(acomp(X))
        else acomp(c(1, 1, 1)), scale = if (scale)
            1/msd(X)
        else 1)
        x <- Y[, 2] + Y[, 3] * c60
        y <- Y[, 3] * s60
        points(x, y, ..., col = col)
    }
    return(invisible(NULL))
}


## ----govexp, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------
#detach(package:MASS)


## ----miss, warning=FALSE, message=FALSE----------------------------------------------------------------------------
library(dplyr)
data("govexp")
# first three observations (long/tidy data format)
head(govexp, 3)
library("dplyr"); library("reshape2")
# from long format to wide format and year 2014
gov14 <- govexp %>% 
  filter(year == 2014) %>% 
  dplyr::select(-year)  %>%  
  reshape2::dcast(country ~ category, mean) 


## ---- echo=TRUE, eval=FALSE----------------------------------------------------------------------------------------
## head(gov14, 3)


## ---- echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------
head(gov14, 3)


## ----miss2---------------------------------------------------------------------------------------------------------
library("VIM")
a <- aggr(gov14, plot = FALSE)
a
# one (of the two) missing values is for Belgium in category 
# HOUCOMM (housing and community amenities)
w <- is.na(gov14$HOUCOMM)


## ----miss2b, echo=TRUE, eval=FALSE---------------------------------------------------------------------------------
## gov14[w, ]


## ----miss2bout, echo=FALSE, eval=TRUE------------------------------------------------------------------------------
gov14[w, ]


## ----miss3, cache = TRUE, tidy=FALSE-------------------------------------------------------------------------------
gov14imp_object <- impKNNa(gov14 %>% 
                    dplyr::select(-country))
# Extract imputed values
gov14imp <- gov14imp_object$xImp
# The imputed value for Belgium on HOUCOMM
options(width = 75)


## ---- echo=TRUE, eval=FALSE----------------------------------------------------------------------------------------
## gov14imp[w, ]


## ---- echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------
gov14imp[w, ]


## ----class---------------------------------------------------------------------------------------------------------
class(gov14imp_object)


## ----printSummary--------------------------------------------------------------------------------------------------
methods(class = "imp")
gov14imp_object


## ----printSummary2, tidy=FALSE-------------------------------------------------------------------------------------
summary(gov14imp_object)
# Also diagnostic plots are available
# plot(gov14imp_object, which = ...)


## ----imp, tidy=FALSE-----------------------------------------------------------------------------------------------
gov14imp2 <- impCoda(gov14 %>% dplyr::select(-country),
                     method = 'ltsReg')$xImp


## ---- eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------
## gov14imp2[w, ]


## ---- eval=TRUE, echo=FALSE----------------------------------------------------------------------------------------
gov14imp2[w, ]  


## ------------------------------------------------------------------------------------------------------------------
gov14[w,"EDU"] / gov14[w, "HEALTH"]  
gov14imp2[w,"EDU"] / gov14imp2[w, "HEALTH"]


## ----da, echo=FALSE------------------------------------------------------------------------------------------------
cda <- function(xOrig, xImp, w){
  da <- function(x,y){
	d <- 0
	p <- length(x)
	for(i in 1:(p-1)){
		for(j in (i+1):p){
			d <- d + (log(x[i]/x[j]) - log(y[i]/y[j]))^2
		}
	}
	d=d/p
	sqrt(d)
  }
  das <- 0
  for(i in 1:nrow(xOrig)){
	das <- das + da(x=xOrig[i,], y=xImp[i,])
  }
  das/w
}


## ----loadpackagescoda, echo=FALSE,hide=TRUE,message=FALSE,warning=FALSE--------------------------------------------
if("package:laeken" %in% search()) detach("package:laeken")
if("package:monomvn" %in% search()) detach("package:monomvn")
#if("package:MASS" %in% search()) detach("package:MASS")
if("package:dplyr" %in% search()) detach("package:dplyr")
if("package:VIM" %in% search()) detach("package:VIM") # otherwise chorizonDL from package VIM is loaded


## ----chorizzerosout, tidy=FALSE, echo=FALSE, eval=TRUE-------------------------------------------------------------
data("chorizonDL", package = "robCompositions")
# variables with zeros included 
# and number of zeros for each variable
which(apply(chorizonDL, 2, function(x) any(x == 0)))


## ------------------------------------------------------------------------------------------------------------------
chorizonDL[64, "As"]


## ------------------------------------------------------------------------------------------------------------------
attributes(chorizonDL)$DL["As"]


## ---- tidy=FALSE---------------------------------------------------------------------------------------------------
data("chorizonDL", package = "robCompositions")
# detection limits
dl <- attributes(chorizonDL)$DL
# exclude some uninteresting variables (for imputation)
chorizonDL <- chorizonDL %>% 
  dplyr::select(-`*ID`, -XCOO, -YCOO, -`*COUN`, -`*ASP`,
                -TOPC, -LITO)
# amount of zeros in each column
vars0 <- apply(chorizonDL, 2, function(x) any(x == 0))
colSums(chorizonDL[, vars0] == 0)
# index of zeros in As
w <- which(chorizonDL$As == 0)
w
# look at one zero in As (only few variables)
chorizonDL[w[1], 1:5]


## ----impBDL, cache=FALSE, warning=FALSE, message=FALSE, eval=FALSE-------------------------------------------------
impBDL <- imputeBDLs(chorizonDL, method = "lmrob")



## ----impBDL3, echo=TRUE, eval=TRUE---------------------------------------------------------------------------------
impBDL
# look at one imputed zero in As 
impBDL$x[w[1], 1:5]
# note that the detection limit was 
dl["As"]


## ------------------------------------------------------------------------------------------------------------------
head(election, 3)


## ----amalgan, tidy=FALSE-------------------------------------------------------------------------------------------
data("eusilc", package = "laeken")
attach(eusilc)
workinc <- py010n + py050n
capinc <- hy040n + hy090n
transh <- hy050n + hy070n + hy080n + hy110n - hy130n - hy145n
transp <- py090n + py100n + py110n + py120n + py130n + py140n
detach(eusilc)
silc <- data.frame("workinc" = workinc, 
                   "transp" = transp, 
                   "capinc" = capinc,
                   "transh" = transh)
head(silc, 2)
# NA's == structural zeros in this data set
silc[is.na(silc)] <- 0
# number of observations including zeros:
sum(apply(silc, 1, function(x) any(x == 0, na.rm = TRUE)))


## ----silcfig, fig.cap="Zero structure of the Austrian EU-SILC data. Left: the number of zeros for work income, capital income, household transfers and personal transfers. Right: combinations of zeros belonging to these four parts."----
par(mar = c(10,3,0,10))
silcNA <- silc
silcNA[silc == 0] <- NA
aggr(silcNA, numbers = TRUE, prop = FALSE)


## ----silcmore, eval=FALSE, echo=FALSE, cache = TRUE----------------------------------------------------------------
## silc <- silc[!silc$transh < 0, ]


## ----ch13out0, eval=FALSE, cache=FALSE, echo=TRUE------------------------------------------------------------------
## # exclude negative values in transh
silc <- silc[!silc$transh < 0, ]
# exclude observations with only one or zero parts observed
v <- apply(silc, 1, function(x) sum(x == 0) %in% c(1,2))
silc <- silc[v, ]
# compute mahalanobis distances
mahD <- compareMahal(silc, imp = "knn")

## ----ch13out, eval=TRUE, cache=FALSE, fig.cap="Comparing Mahalanobis distances obtained from the imputation approach and from the estimation in subcompositions applied to the EU-SILC data,All Rights Reserved."----
plot(mahD)

