#########################################################################################
###   Book: 
###   Visualization and Imputation of Missing Values  
###   Springer Publishing, 2013
###   Chapter 4: Visualization of missing values
###   Copyright: Matthias Templ
###              University of Applied Sciences and Arts Northwestern Switzerland (FHNW) 
###   Licence: GPL3


## ----visremove, fig.cap="Example of deleting observations containing missing values without a warning.", fig.pos="H"----
data("sleep", package = "VIM")
# observations with NA removed without providing a warning
plot(Dream ~ NonD, data = sleep) 


## ----visremoveggplot, fig.cap="Example of deleting observations containing missing values with a warning, but still only the complete information is plotted.", warning=TRUE, message=TRUE, fig.pos="H"----
library(ggplot2)
# observations with NA removed with a warning
ggplot(sleep, aes(x = NonD, y = Dream)) + 
        geom_point() 


## ----loadsleepdata, message=FALSE, warning=FALSE-----------------------------------------------------
library("VIM")
data("sleep")


## ----cs, echo=TRUE, eval=FALSE, tidy=FALSE-----------------------------------------------------------
## cs <- complete.cases(sleep)
## sleep[!cs, ]


## ----csoutput, echo=FALSE, eval=TRUE, tidy=FALSE-----------------------------------------------------
cs <- complete.cases(sleep)
sleep[!cs, 1:10]


## ----countNA-----------------------------------------------------------------------------------------
countNA(sleep)


## ----countNA2----------------------------------------------------------------------------------------
apply(sleep, 2, countNA)


## ----aggr, cache=TRUE--------------------------------------------------------------------------------
a <- aggr(sleep, plot = FALSE)
class(a)
a


## ----summary-----------------------------------------------------------------------------------------
summary(a)


## ----misspatternssleep, message=FALSE, warning=FALSE-------------------------------------------------
library("robCompositions")
m <- missPatterns(sleep)
names(m)


## ----groupsshow, eval=FALSE, echo=TRUE---------------------------------------------------------------
## m$groups


## ----groupsoutput, echo=FALSE, eval=TRUE-------------------------------------------------------------
m$groups


## ----------------------------------------------------------------------------------------------------
m$tabcombPlus


## ----rsumprint, echo=TRUE, eval=FALSE----------------------------------------------------------------
## m$rsum


## ----rsumoutput, echo=FALSE, eval=TRUE---------------------------------------------------------------
m$rsum


## ----aggr1, fig.align='center', fig.cap="Aggregation plot of the income components in the public use sample of the Austrian EU-SILC data from 2004. Left: barplot of the proportions of  missing values in each of the income components. Right: all existing combinations of missing (dark grey) and non-missing (light grey) values in the observations. The frequencies of the combinations are visualized by small horizontal bars.", echo=TRUE, collapse=FALSE, tidy=FALSE, fig.pos="H"----
plot(a, numbers = TRUE, fig.pos="H")


## ----aggr2, fig.align='center', fig.cap="Aggregation plot showing all existing combinations of missing (dark grey) and non-missing   (light grey) values in the observations and columns. The frequencies of the combinations are visualized by small bars.", cache=TRUE, echo=TRUE, collapse=FALSE, tidy=FALSE, fig.pos="H"----
plot(a, 
     numbers = TRUE, 
     prop = FALSE, 
     combined = TRUE) 


## ----ggmissvar, warning=FALSE, message=FALSE, fig.cap="Number of missings for each variable using package naniar.", fig.pos="H"----
library(naniar)
data(sleep, package = "VIM")
gg_miss_var(sleep, facet = Pred)


## ----vishist, warning=FALSE, message=FALSE, fig.cap="Histogram of variable span highlighting missing values in the remaining variables.", fig.pos="H"----
histMiss(sleep, pos = 6, selection = "any")


## ----visspine, fig.cap="Spinogram of Span with highlighting based on variable Dream.", collapse=FALSE, tidy=FALSE, fig.pos="H"----
spineMiss(sleep[, c("Span", "Dream")])


## ----visbar, fig.cap="Barplot of variable Danger with highlighting based on variable Sleep.", collapse=FALSE, tidy=FALSE, fig.pos="H"----
barMiss(sleep[, c("Danger", "Sleep")])


## ----visbarspine, fig.cap="Spineplot of variable Danger with highlighting based on variable Sleep.", fig.pos="H"----
spineMiss(sleep[, c("Danger", "Sleep")])


## ----visboxplot, fig.cap="Parallel boxplots to observe MAR situations for a selected continuous variable.", warning=FALSE, message=FALSE, fig.pos="H",out.extra='', out.width='70%'----
sleep$logBodyWgt <- log10(sleep$BodyWgt)
par(mar = c(5,4,0.1,0.1))
pbox(sleep, pos = 10) 


## ----visscatter, tidy=FALSE, fig.cap='Scatterplot with information on missing values in the margins.'----
marginplot(sleep[, c("Sleep", "Dream")], 
           alpha = 0.65, # transparency of points
           pch = 20, cex = 1.3)


## ----scattMiss, fig.cap="Scatterplots of the variables `Sleep` (total sleeping time per hour a day) and `Dream` (dreaming per day in hours) in the mammals `sleep` data displayed as rug representation and lines limited by tolerance ellipses (right).", tidy=FALSE, fig.pos="H", out.extra=''----
scattMiss(sleep[, c("Sleep", "Dream")], 
    inEllipse = TRUE, # default equals FALSE
    side = 1  # default, for missings in Dream, put side = 2
)


## ----visscatmat, fig.cap="Scatterplot matrix of log-transformed BodyWgt (body weight), log-transformed BrainWgt (brain weight), Dream (amount of sleep with rapid eye movement and Sleep (total amount of sleep in hours per day) in the mammal sleep data. Observations with missing values in Dream are highlighted in all bivariate plots that do not include that variable.", fig.pos="H", out.extra=''----
sleep$logBrainWgt <- log(sleep$BrainWgt)
vars <- c("logBodyWgt", "logBrainWgt", "Dream", "Sleep")
scattmatrixMiss(sleep[, vars], highlight = "Dream")


## ----facetmiss, fig.cap="Groupwise scatterplots with missing information in the margins.", tidy=FALSE, fig.pos="H", out.extra=''----
library(naniar)
ggplot(sleep, 
       aes(x = Sleep, y = Dream)) + 
  geom_miss_point() + 
  facet_wrap(~ Pred)


## ----paarcoordMiss1, fig.cap='Parallel coordinate plot of selected variables of the sleep data. Red lines indicate missing values in one of the variables Sleep, NonD or Dream. Brown lines indicate missings in one of the variables displayed on the x-axis.', collapse=FALSE, tidy=FALSE, fig.pos="H"----
par(mar = c(6,1,1,1))
parcoordMiss(sleep, 
             highlight = c("Sleep","Dream","NonD"), 
             selection = "any", 
             plotvars = 6:ncol(sleep), 
             plotNA = TRUE)


## ----paarcoordMiss2, fig.cap="Parallel coordinate plot of selected variables of the sleep data. Red lines indicate missing values in variable `Sleep`, `Dream` or `NonD`. Observations with missing values in the displayed variables on the x-axis are suppressed (option `plotNA = FALSE`).", collapse=FALSE, tidy=FALSE, fig.pos="H"----
par(mar = c(6,1,1,1))
parcoordMiss(sleep, 
             highlight = "Sleep",  
             plotvars = c(3,4,6:ncol(sleep)), 
             plotNA = FALSE)


## ----paarcoordMiss3, fig.cap="Parallel coordinate plot of selected variables of the subset of the sleep data. . The difference to the previous plot is that missings in the plotted variables are shown in brown and also shown outside the usual parallel coordinate plot.", collapse=FALSE, tidy=FALSE, fig.pos="H"----
par(mar = c(6,1,1,1))
parcoordMiss(sleep, 
             highlight = "Sleep",  
             plotvars = c(3,4,6:ncol(sleep)), 
             plotNA = TRUE)


## ----vismisnaniar, fig.cap="Matrix plot as implemented in the naniar R package showing missings and observed values in a data matrix.", fig.pos="H", out.extra=''----
library(naniar)
vis_miss(sleep)


## ----matrix, fig.cap="Matrix plot of selected variables of the subset of the `sleep` data sorted by variable `BodyWgt` (body weight of mammals)", fig.pos="H", out.extra=''----
matrixplot(sleep, sortby = "BodyWgt")


## ----matrix2, fig.cap="Matrix plot of selected variables of the subset of the `sleep` data sorted by variable `Span` (life span of mammals)", fig.pos="H", out.extra=''----
matrixplot(sleep, sortby = "Span")


## ----mosaic, tidy=FALSE, fig.cap="Mosaic plot of `Pred` (predator level of a mammal) and `Danger` (of mammals). Missing values in `Sleep` (total sleeping time) are highlighted in red.", fig.pos="H", out.extra=''----
mosaicMiss(sleep, plotvars = c("Danger","Pred"), 
           highlight = "Sleep", miss.labels = FALSE)


## ----map, fig.cap="Growing dot map of As (concentration of Arsenic) and Bi (concentration of ...) in the Kola C-horizon data.", fig.pos="H", fig.height=8, fig.width=8, collapse=FALSE, tidy=FALSE, fig.pos="H", out.extra=''----
data(chorizonDL, package = "VIM")
data(kola.background, package = "VIM")
coo <- chorizonDL[, c("XCOO", "YCOO")]
## for missing values
x <- chorizonDL[, c("Ca", "As", "Bi")]
growdotMiss(x, coo, kola.background, border = "white", 
            interactive = FALSE)


## ----dropoutvis, fig.cap="Dropouts visualized in a matrixplot.", fig.pos="H"-------------------------
data(dropout, package = "dropR")
dropout <- dropout %>% 
  arrange(q10,q9,q8,q7,q6,q5,q4,q3,q2,q1)
m <- is.na(sleep)
m |> head()
matrixplot(dropout)

