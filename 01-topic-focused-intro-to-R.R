#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2023
###   Chapter 1: Topic-focused introduction to R and data sets used 
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3

## ----arith4-------------------------------------------------------------------
0.5 + 0.2 * log(0.15^2)


## ----countNAsleep-------------------------------------------------------------
library("VIM")
data(sleep, package = "VIM")
countNA(sleep)


## ----functions, eval=FALSE----------------------------------------------------
## res1 <- name_of_function(v1) # one input argument
## res2 <- name_of_function(v1, v2) # two input arguments
## res3 <- name_of_function(v1, v2, v3) # three input arguments
## # ...


## ----argsinit-----------------------------------------------------------------
args(countNA)
args(initialise)


## ----powsum-------------------------------------------------------------------
pow_sum <- function(x, p = 2){
  return(sum(x^p))
}


## ----unif, echo=TRUE----------------------------------------------------------
x <- runif(10, 0, 1)
x


## ----install, eval=FALSE------------------------------------------------------
## install.packages("VIM")


## ----devtools, eval=FALSE-----------------------------------------------------
## ## if not installed, install package remotes:
## if(!require(remotes)){
##     install.packages("remotes")
## }
## ## load the remotes package
## library("remotes")
## ## install package from github
## install_github("statistikat/VIM")


## ----helpstart, eval=FALSE----------------------------------------------------
## help.start()


## ----help2, eval=FALSE--------------------------------------------------------
## help(package = "VIM")


## ----help3, eval=FALSE--------------------------------------------------------
## ?kNN


## ----arithm1------------------------------------------------------------------
3 + 5


## ----arithm2------------------------------------------------------------------
'+'(3, 5)


## ----help4, eval=FALSE--------------------------------------------------------
## data("diabetes")


## ----help5, eval=FALSE--------------------------------------------------------
## help.search("Hotdeck")


## ----help6, eval=FALSE--------------------------------------------------------
## rm(list=ls())


## ----help7, eval=TRUE---------------------------------------------------------
apropos("knn")


## ----help7rs, eval=FALSE------------------------------------------------------
## RSiteSearch("irmi")


## ----lsshow-------------------------------------------------------------------
ls()


## ----getwd--------------------------------------------------------------------
getwd()


## ----help8, eval=FALSE--------------------------------------------------------
## g <- getwd()


## ----getwd2-------------------------------------------------------------------
# paste creates a string
p <- paste(getwd(), "/data", sep = "")
p


## ----help9, eval=FALSE--------------------------------------------------------
## # now change the working directory
## setwd(p)


## ----isnumeric----------------------------------------------------------------
v.num <- c(0.1, 0.3, 0.5, 0.9, 0.7)
v.num
is.numeric(v.num)


## ----logical------------------------------------------------------------------
v.num > 0.3


## ----help10, eval=FALSE, warning=FALSE, message=FALSE-------------------------
## v1 <- c(0.1, 0.2, 0.3)
## v2 <- c(0.4, 0.5)
## v1 + v2


## ----isnumeric2---------------------------------------------------------------
v2 <- c (100, TRUE, "A", FALSE)
v2
is.numeric (v2)


## ----gemasR, eval=TRUE--------------------------------------------------------
data("diabetes", package = "VIM")
# extract a subset of the variable sand from the gemas data
bmi_subset <- diabetes[1:10, "BMI"] 
bmi_subset
# positive indexing:
bmi_subset[c(3, 6, 7)]
# negative indexing:
bmi_subset[-c(1, 2, 4, 5, 8:10)]
# logical indexing:
bmi_subset < 30
# a logical expression can be written directly in []
bmi_subset[bmi_subset < 30]


## ----clrcoef------------------------------------------------------------------
## compute clr coefficients
mp <- robCompositions::missPatterns(diabetes)
## result is a list
class(mp)
# str gives the structure of the object
# (output suppressed because its lengthy in this case)
# str(mp)
names(mp)
## access elements from the named list with the dollar sign
summary(mp$rsum)


## ----diabetesclasslevels------------------------------------------------------
class(diabetes$Outcome)
levels(diabetes$Outcome)
summary(diabetes$Outcome)


## ----subsetgemas--------------------------------------------------------------
## select a subset of observations:
w <- diabetes$Outcome == "yes" & diabetes$BMI > 50
dim(diabetes[w, ]) 
## select a subset of variables 
cn <- colnames(diabetes) %in% c("Age", "BMI", "Outcome")
diabetes[w, cn]


## ----isnagemas----------------------------------------------------------------
sum(is.na(diabetes))


## ----isnagemas22--------------------------------------------------------------
countNA(diabetes)


## ----logical2-----------------------------------------------------------------
bmi_subset[1:2] <- NA
bmi_subset < 30


## ----varbmi-------------------------------------------------------------------
bmi_subset
var(bmi_subset)
var(bmi_subset, na.rm = TRUE)


## ----tablediabetes------------------------------------------------------------
data("diabetes", package = "VIM")
# counts of women on the number of pregnancies
table(diabetes$Pregnancies)
table(diabetes$Pregnancies, useNA = "ifany")


## ----misspatternsgemas--------------------------------------------------------
m <- robCompositions::missPatterns(diabetes)
names(m)
## patterns of missingness:
m$tabcombPlus
## observations with missings:
which(is.na(diabetes$Pregnancies), arr.ind = TRUE)
## e.g.
diabetes[5,]


## ----strddiabetesshow0, tidy=FALSE, echo=TRUE, eval=FALSE---------------------
## class(diabetes)
## str(diabetes)


## ----strddiabetesshow, tidy=FALSE, echo=FALSE, eval=TRUE----------------------
class(diabetes)
str(diabetes, vec.len = 1)


## ----summerysleep-------------------------------------------------------------
summary(sleep$Pred)


## ----summarypred--------------------------------------------------------------
class(sleep$Pred)
sleep$Pred <- factor(sleep$Pred)
class(sleep$Pred)
summary(sleep$Pred)


## ----countsummaries-----------------------------------------------------------
## how many "summary" methods exists:
length(methods(summary))
class(diabetes$Pregnancies)
summary(diabetes$Pregnancies)
## again, just to see the difference, convert to class factor:
summary(as.character(diabetes$Pregnancies))


## ----argsprint----------------------------------------------------------------
args(print)


## ----printfunction, echo=TRUE, tidy=FALSE-------------------------------------
print.missPatterns <- function(x, ...){
  cat("There are", sum(m$rindex), 
      "missing values in your data set\n")
}


## ----printfunction2, echo=TRUE------------------------------------------------
m # or equivalent: print(m)


## ----search, echo=TRUE, warning=FALSE, message=FALSE--------------------------
library(dplyr)
library(MASS)
search()


## ----dplyrselect, eval = FALSE------------------------------------------------
## select(diabetes, BMI)


## ----dplyrselect2, eval=FALSE-------------------------------------------------
## dplyr::select(diabetes, BMI) # output suppressed - too long


## ----calldplyr, eval = TRUE, echo = TRUE, message=FALSE, warning=FALSE--------
library("dplyr") 


## ----magrittrhead, eval = TRUE, echo = TRUE-----------------------------------
diabetes %>% 
  head


## ----pipeinR------------------------------------------------------------------
diabetes |> head()


## ----classdiabetes, eval = TRUE, echo = TRUE----------------------------------
class(diabetes)


## ----classdiabetes2, eval = TRUE, echo = TRUE---------------------------------
diabetes <- as_tibble(diabetes)
class(diabetes) 


## ----printdiabetes, eval = FALSE, echo = TRUE, results='hide'-----------------
## print(diabetes) # output suppressed


## ----slicediabetes, eval = TRUE, echo = TRUE, results='hide'------------------
diabetes %>% 
  slice(1) # first line, output suppressed


## ----slicediabetes2, eval = TRUE, echo = TRUE, tidy=FALSE---------------------
diabetes %>% 
  slice(c(1, 4, 10, 15, n())) 


## ----filterdiabetes, eval = TRUE, echo = TRUE, tidy=FALSE---------------------
diabetes %>% 
  filter(Age < 25 & Outcome == "yes" & BMI > 44)


## ----arrangediabetes, eval = TRUE, echo = TRUE, results='hide'----------------
diabetes %>% 
  arrange(BMI)
diabetes ## output suppressed


## ----arrangediabetes2, eval = TRUE, echo = TRUE, tidy=FALSE-------------------
diabetes %>% 
  arrange(desc(BloodPressure), BMI) %>% 
  head


## ----dplyrselect3, eval = TRUE, echo = TRUE-----------------------------------
diabetes %>% 
  dplyr::select(Age, BMI, Outcome) %>% 
  head 


## ----dplyrselect4, eval = TRUE, echo = TRUE-----------------------------------
diabetes %>% 
  dplyr::select(BMI:Age)


## ----dplyrselect5, eval = TRUE, echo = TRUE, results='hide'-------------------
diabetes %>% 
  dplyr::select(-Pregnancies) # output suppressed


## ----dplyrselect6, eval = TRUE, echo = TRUE-----------------------------------
diabetes %>% 
  dplyr::select(ends_with("e")) %>%
  head


## ----dplyrselect7, eval = TRUE, echo = TRUE-----------------------------------
diabetes %>% 
  distinct(BloodPressure, Age, .keep_all = TRUE) %>% 
  arrange(Age) %>%  
  dplyr::select(Age, BMI, Pregnancies, BloodPressure) %>% 
  head


## ----dplyrtransmute, eval = TRUE, echo = TRUE, tidy=FALSE---------------------
diabetes %>% 
  transmute("younger" = Age < 40, 
            "younger01" = ifelse(younger, 0, 1), 
            Age) %>% 
  head(2)


## ----dplyrsummarize, eval = TRUE, echo = TRUE, tidy=FALSE---------------------
diabetes %>% 
  group_by(Outcome, Pregnancies) %>% 
  summarise("mean" = mean(DiabetesPedigreeFunction),
            "median" = median(DiabetesPedigreeFunction),
            "sd" = sd(DiabetesPedigreeFunction),
            "mad" = mad(DiabetesPedigreeFunction)
            ) %>% 
  arrange(desc(median)) %>% 
  head


## ----dplyrmutate, eval = TRUE, echo = TRUE, tidy=FALSE------------------------
diabetes %>% 
  group_by(Outcome) %>% 
  arrange(BMI) %>% 
  dplyr::select(DiabetesPedigreeFunction, BMI) %>% 
  mutate(cmean_diabetes = cummean(DiabetesPedigreeFunction), 
         csum_BMI = cumsum(BMI))


## ----dt1, eval=TRUE, echo=TRUE, results='hide'--------------------------------
require(data.table)
diabetes <- data.table(diabetes)
diabetes   # print output suppressed


## ----dt3,eval=TRUE, echo=TRUE-------------------------------------------------
tables()


## ----dt4,eval=TRUE, echo=TRUE-------------------------------------------------
diabetes$Agegroup <- diabetes[, j = Age < 40] 


## ----dt6,eval=TRUE, echo=TRUE-------------------------------------------------
diabetes[, tmp1 := rnorm(nrow(diabetes))] 


## ----dt8,eval=TRUE, echo=TRUE-------------------------------------------------
diabetes[, tmp1 := NULL]
# diabetes$tmp1 <- NULL  # equivalent


## ----dt9,eval=FALSE, echo=FALSE-----------------------------------------------
## Cars93[, tmp1 := NULL]
## Cars93$tmp2 <- NULL


## ----dt10,eval=TRUE, echo=TRUE, results='hide'--------------------------------
diabetes[i = 2] # second row, all columns 
diabetes[i = c(1,5)] # first and fifth row, all columns
diabetes[i = -c(1:5)] # exclude the first five rows


## ----dt12,eval=FALSE, echo=TRUE, tidy=FALSE-----------------------------------
## diabetes[j = 3] # a data.table back from the third column
## diabetes[j = "BloodPressure"] # same here
## diabetes[j = BloodPressure] # this gives a vector back


## ----dt13,eval=TRUE, echo=TRUE, tidy=FALSE------------------------------------
diabetes[i=1:3, j = "BloodPressure"] 


## ----dt16,eval=TRUE, echo=TRUE, tidy=FALSE------------------------------------
diabetes[1:3, .(Glucose, BloodPressure,   
                Med.BloodPressure = median(BloodPressure),   
                Mean.BloodPressure = mean(BloodPressure))]  


## ----dt18,eval=TRUE, echo=TRUE------------------------------------------------
setkey(diabetes, Outcome, Age) 


## ----dt19,eval=TRUE, echo=TRUE------------------------------------------------
key(diabetes)


## ----dt20,eval=TRUE, echo=TRUE------------------------------------------------
diabetes[Outcome == "yes"] 


## ----dt22,eval=TRUE, echo=TRUE, cache=TRUE, tidy=FALSE------------------------
require(microbenchmark)
N <- 1000000
dat <- data.table(
  x = sample(LETTERS[1:20], N, replace=TRUE),  
  y = sample(letters[1:5], N, replace=TRUE))
df <- as.data.frame(dat)
head(dat, 3) # first three observations

setkey(dat, x, y)

microbenchmark(
  data.table = dat[list(c("B", "D"), c("b", "d"))],  
  dplyr = df %>% filter(x %in% c("B", "D") & y %in% c("b", "d")),  
  baseR = df[df$x %in% c("B", "D") & df$y %in% c("b", "d"), ]  
)


## ----dt23,eval=TRUE, echo=TRUE, cache=TRUE, tidy=FALSE------------------------
diabetes[, .(
      mean = mean(BloodPressure, na.rm = TRUE), 
      IQR = IQR(BloodPressure, na.rm = TRUE), 
      median = as.double(median(BloodPressure, na.rm = TRUE))
             ), 
         by = Outcome]


## ----aggrcensus, fig.cap="Frequencies of missing values and combinations of missing values for the census data from UCI."----
# remotes::install_github("delta-rho/datadr")
data(adult, package = "datadr")
adult[adult == "?"] <- NA
print(aggr(adult, cex.axis = 0.7))


## ----airqual, fig.cap="Frequencies of missing values and combinations of missing values for the airquality data."----
data("airquality")
aggr(airquality)


## ----bcancersummary-----------------------------------------------------------
data(bcancer, package = "VIM")
summary(aggr(bcancer, plot = FALSE))


## ----brittlenessdata, cache = TRUE--------------------------------------------
summary(aggr(brittleness, plot = FALSE))


## ----chorizonDLdata-----------------------------------------------------------
data("chorizonDL", package = "VIM")
dim(chorizonDL)
# number of values below detection limit
sum(chorizonDL == 0, na.rm = TRUE)



## ----matrixplotcolic, fig.cap="Matrixplot of the horse colic data set, sorted by the heart rate (pulse).", cache=TRUE, tidy=FALSE----
data(colic)
matrixplot(colic,   
           sortby = "pulse",  
           cex.axis = 0.7,   
           labels = substr(colnames(colic), 1, 11))



## ----collisions, fig.cap="Frequencies of missing values for the collisions data set.", cache=TRUE, tidy=FALSE----
data(collisions, package = "VIM")
aggr(collisions,   
     labels = substr(colnames(collisions), 1, 14),   
     cex.axis = 0.5)


## ----diabetesplot, echo=TRUE, fig.align='center', fig.cap='Boxplot of Age (diabetes data) and boxplots of age grouped by the missing index of other variables.'----
data(diabetes, package = "VIM")
par(mar = c(8,4,0.1,0.1))
pbox(diabetes, pos = 8)


## ----eusilc13pufaggr, message=FALSE, warning=FALSE, fig.cap="Proportion of missing values of the eusilc13puf data set and their combinations in variables."----
# uncomment in case these packages are not installed:
# install.packages(c("VIM", "simPop")) 
data("eusilc13puf", package = "simPop")
library("VIM")
# par(cex.labels = 0.5)
aggr(eusilc13puf, cex.axis = 0.45)



## ----aggrfood-----------------------------------------------------------------
data(food, package = "VIM")
aggr(food, plot = FALSE)



## ----pulpmatrixplot, fig.align='center', fig.cap='Matrixplot of the pulplignin data sorted by ChipRate.'----
data(pulplignin)
par(mar = c(7,4,0.1,0.1))
matrixplot(pulplignin, sortby = "ChipRate")


## ----sbsplot, fig.cap="Frequencies of missing values in the structural business statistics data set."----
data("SBS5242", package = "VIM")
aggr(SBS5242)


## ----sleep, echo=TRUE, cache=TRUE, fig.align='center', fig.cap='Histogramm of log(BodyWgt) of the sleep data set. Colors related to the index of missing values in variable NonD (non-dreaming phase).'----
data(sleep)
print(aggr(sleep,plot = FALSE))
spineMiss(log(sleep[, c("BodyWgt", "NonD")]))


## ----mtao, warning=FALSE, message=FALSE, fig.cap='Matrixplot for the tao data set sorted by Year.', fig.align='center'----
data("tao")
par(mar = c(8,2, 0.1, 0.1))
matrixplot(tao)



## ----wines, echo=TRUE, fig.cap='Matrixplot of the wine data set sorted by country.'----
data(wine, package = "VIM")
par(mar = c(9,4,0.1,0.1))
matrixplot(wine, sortby = "country")

