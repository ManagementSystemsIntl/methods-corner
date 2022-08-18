basePackages <- c("abind","acs", "arm","blme","boot","car","CBPS","cem","choroplethr","corpcor","data.table","DBI","descr","DescTools","doBy","dplyr","dummies","ElemStatLearn","FactoMineR", "foreign","formatR","fPortfolio","gclus","gcookbook","ggplot2","GPArotation","graphics","grDevices","grid","gridExtra","gss","hett","Hmisc","httr","jpeg","KernSmooth","knitr","labeling","lars","latentnet","lattice","lavaan", "lessR","list","lme4","locfit","lsr","lubridate","manipulate","markdown","MASS","Matrix","memisc","methods","mice","mnormt","modeltools","multilevel","mvnormtest","mvtnorm","network","nFactors","nlme","NLSdata","openintro", "optmatch","parallel","plotrix","plyr","polycor","pscl","psych","pwr","quantmod","QuantPsyc","quantreg","randomForest", "Rcmdr","reshape","reshape2", "RItools","rJava","rms","robust","RODBC","rpart","rstudio","sandwich","sem","sm","sn","SparseM","sqldf","stats","stringi","stringr","strucchange","survey","survival","swirl","tables","timeDate","UsingR","utils","vcd","XLConnect","xlsx","xlsxjars","XML","xtable","xts")

lapply(basePackages,library,character.only=T)

base <- c("abind","boot","caTools","car","CBPS","cem","choroplethr","chron","class","codetools","colorspace","corpcor","covTest","datasets","data.table","DBI","descr","DescTools","devtools","dichromat","doBy","plyr","dplyr","dummies","easyanova","effects","FactoMineR","foreign","gcookbook","gdata","ggplot2","GPArotation","graph","graphics","grDevices","gridExtra","gsubfn","gtools","hett","HH","highr","HistData","Hmisc","installr","jpeg","kernlab","KernSmooth","knitr","labeling","lars","latentnet","latticeExtra","lavaan","lazyeval","leaps","lessR","list","lme4","lmtest","lubridate","magrittr","mapplots","markdown","MASS","MatchIt","Matrix","memisc","methods","mice","miscTools","mlogit","modeltools","multcomp","multilevel","munsell","nFactors","nlme","openxlsx","plotrix","polycor","psych","quantreg","relimp","reshape2","Rgraphviz","RItools","rlme","robust","robustbase","rstudio","sandwich","sem","sm","SparseM","statmod","stats","stats4","survey","survival","swirl","systemfit","tables","tidyr","utils","vcd","xlsx")

lapply(base,library,character.only=T)

smallBase <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", "janitor", "reshape2","tidyr","broom", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", "plotrix", "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "PowerUpR", "openxlsx", "ggrepel", "readr", "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", "texreg", "janitor","psych","dplyr", "tidyverse", "bbplot", "viridis", "here", "ggridges", "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight")

lapply(smallBase, library, character.only=T)

font_add_google("Source Sans Pro", "sans-serif")

analysis <- c("plm", "rethinking" , "brms", "rstan", "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth", "panelView")

map(analysis, library, character.only=T)

lapply(analysis, library, character.only=T)

datacln <- c("dataMaid", "assertr", "pointblank", "validate")

map(datacln, library, character.only=T)

lapply(datacln, library, character.only=T)

# no more BAS package
# no more brmstools
# no more ezsummary

# ceterisParibus for counterfactual plots

SNApackages <- c("igraph", "NetCluster","NetData","sna","statnet","tergm", "DiagrammeR")
lapply(SNApackages, library, character.only=T)

maps <- c("geoR", "ggmap", "mapplots","mapproj","maps", "maptools", "raster", "rgdal","rgeos","rworldmap", "shapefiles", "sp",
          "spatstat", "plotGoogleMaps", "zoom")

lapply(maps, library,character.only=T)

financePackages <- c("PerformanceAnalytics","fAssets","fBasics","timeSeries","TSA","tseries")


NLPtools <- c("tm", "slam","igraph","openNLP","snowfall")

lapply(NLPtools,library,character.only=T)

markov <- c("expm","markovchain","diagram","pracma")

lapply(markov, library, character.only=T)

valuation <- c("support.CEs", "DCchoice","mlogit", "apollo")
map(valuation, library, character.only=T)

library(DCchoice)


options(digits=3, scipen=6)
options(digits=4, scipen=6)

x11()

# no axis lines
theme_set(theme_bw() + theme(panel.grid.major.x=element_blank(),
                             panel.grid.minor.x=element_blank(),
                             panel.grid.major.y=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5)) +
               theme(axis.title=element_text(size=16)) +
               theme(axis.text=element_text(size=14)))

# no horizontal lines
theme_set(theme_bw() + theme(panel.grid.major.y=element_blank(),
                             panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5),
                             axis.title=element_text(size=16),
                             axis.text=element_text(size=14)))
# no vertical lines
theme_set(theme_bw() + theme(panel.grid.major.x=element_blank(),
                             panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5),
                             axis.title=element_text(size=16),
                             axis.text=element_text(size=14)))

# vertical and horizontal lines
theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=16, hjust=.5),
                             axis.title=element_text(size=14),
                             axis.text=element_text(size=12),
                             strip.text=element_text(size=12)))


theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5),
                             axis.title=element_text(size=16),
                             axis.text=element_text(size=14),
                             legend.text=element_text(size=14),
                             strip.text=element_text(size=14)))

theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5, family = "sans"),
                             plot.subtitle = element_text(size=16, family="sans"),
                             plot.caption=element_text(size=12, family="sans"),
                             axis.title=element_text(size=16, family="sans"),
                             axis.text=element_text(size=14, family="sans"),
                             legend.text=element_text(size=14, family="sans"),
                             strip.text=element_text(size=14, family="sans")))

theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5, family = "Source Sans Pro"),
                             plot.subtitle = element_text(size=16, family="Source Sans Pro"),
                             plot.caption=element_text(size=12, family="Source Sans Pro"),
                             axis.title=element_text(size=16, family="Source Sans Pro"),
                             axis.text=element_text(size=14, family="Source Sans Pro"),
                             legend.text=element_text(size=14, family="Source Sans Pro"),
                             strip.text=element_text(size=14, family="Source Sans Pro")))


nobord <- theme(panel.border=element_blank(),
                axis.ticks = element_blank())

# for CSO summary
theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=20, hjust=.5, family = "Source Sans Pro"),
                             plot.subtitle = element_text(size=18, family="Source Sans Pro"),
                             plot.caption=element_text(size=14, family="Source Sans Pro"),
                             axis.title=element_text(size=18, family="Source Sans Pro"),
                             axis.text=element_text(size=16, family="Source Sans Pro"),
                             legend.text=element_text(size=16, family="Source Sans Pro"),
                             strip.text=element_text(size=16, family="Source Sans Pro")))




theme_set(theme_economist() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=18, hjust=.5),
                             axis.title=element_text(size=16),
                             axis.text=element_text(size=14),
                             strip.text=element_text(size=14)))

# coefficient plot

ggplot(zFullS, aes(x=median, y=fct_reorder(term_labs, median))) +
  geom_vline(xintercept=0, color="darkgrey", size=1.2) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, color=color), height=0, size=.9) +
  #geom_point(aes(color=color), size=3) +
  geom_label(aes(color=color, label=paste(100*round(median,2), "%", sep=""), size=3)) +
  scale_color_manual(values=c("darkblue", "grey60", "maroon")) +
  scale_color_manual(values=c("grey60", "maroon")) +
  nobord +
  guides(color=F,
         size=F) +
  #scale_x_continuous(breaks=seq(-8,6,2)) +
  labs(x="Change in probability of receiving\nfinancing after GIST competition",
       y="",
       title="",
       caption="") +
  scale_x_continuous(labels=percent)



# slopegraph

spec <- theme_set(theme_bw() +
                    theme(legend.position="none",
                          panel.border = element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          panel.grid.major.y=element_blank(),
                          panel.grid.minor.y=element_blank(),
                          axis.title.x=element_blank(),
                          panel.grid.major.x=element_blank(),
                          axis.text.x=element_text(size=18, face="bold"),
                          axis.ticks=element_blank(),
                          plot.title=element_text(face="bold", size=22, hjust=.5),
                          plot.subtitle=element_text(hjust=.5)))


  # for poster
theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=22, hjust=.5),
                             plot.caption=element_text(size=14),
                             axis.title=element_text(size=20),
                             axis.text=element_text(size=18),
                             legend.text=element_text(size=16),
                             strip.text=element_text(size=18)))

  # slopegraph poster

spec <- theme_set(theme_bw() +
            theme(legend.position="none",
                  panel.border = element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  panel.grid.major.y=element_blank(),
                  panel.grid.minor.y=element_blank(),
                  axis.title.x=element_blank(),
                  panel.grid.major.x=element_blank(),
                  axis.text.x=element_text(size=18, face="bold"),
                  axis.ticks=element_blank(),
                  plot.title=element_text(face="bold", size=22, hjust=.5),
                  plot.subtitle=element_text(hjust=.5)))



theme_economist(base_size=18)

?ggthemes
library(ggthemes)
?theme_economist

# include minor x axis to go with labeled scale points
theme_set(theme_bw() + theme(panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=16, hjust=.5),
                             axis.title=element_text(size=16),
                             axis.text=element_text(size=14)))


?theme_bw
p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
                                     colour=factor(gear))) + facet_wrap(~am)

p
p + theme_gray()
p + theme_bw()
p + theme_linedraw()
p + theme_light()
p + theme_dark()
p + theme_minimal()
p + theme_classic()
p + theme_void()


# codebook ----------------------------------------------------------------

codebook <- function(data) {
  name <- data.frame(names(data))
  name$class <- lapply(data, class)
  name$label <- get_label(data)
  return(name)
}



# r markdown template -----------------------------------------------------

---
  title: "MENTOR explore"
output:
  html_document:
  toc: yes
toc_depth: '5'
toc_float: yes
---

  ```{r}
library(tidyverse)
library(strengejacke)
library(psych)
library(brms)
library(skimr)
library(knitr)
library(xlsx)
library(here)
library(haven)
```

```{r setup, include=FALSE, echo=FALSE}
require("knitr")
opts_knit$set(root.dir = here())
```

```{r global_options}
knitr::opts_chunk$set(fig.width=10, fig.height=8, warning=FALSE, message=FALSE, cache=TRUE, error=T)
```

```{r}

options(digits=3, scipen=6)

# vertical and horizontal lines
theme_set(theme_bw() + theme(panel.grid.minor.x=element_blank(),
                             panel.grid.minor.y=element_blank(),
                             plot.title=element_text(face="bold",size=16, hjust=.5),
                             axis.title=element_text(size=14),
                             axis.text=element_text(size=12),
                             strip.text=element_text(size=12)))
```


# mode --------------------------------------------------------------------


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# multiplot ---------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# slopegraph --------------------------------------------------------------

library(devtools)
source_url("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/slopegraph.r")


# quote each element of a vector ------------------------------------------

q <- function(...) {
  sapply(match.call()[-1], deparse)
}


# scale by 2 SDs (Gelman) -------------------------------------------------

# Function for standardizing regression predictors by dividing by 2 sd's
#
# 17 Aug 2006:  rewritten to allow more options for binary inputs.  The new
#   default is to center binary inputs at zero (to allow for interactions to
#   be easier to interpret) but _not_ to rescale by dividing by 2 sd's.

standardize <- function (object, unchanged=NULL,
                         standardize.y=FALSE, binary.inputs="center"){
  #
  # Function for automatically standardizing by dividing by 2 sd's
  # Default is to standardize the input variables as follows:
  #   - Numeric variables that take on more than two values are each rescaled
  #     to have a mean of 0 and a sd of 0.5 (so that the scaled regression coef
  #     corresponds to a change from mean - 1*sd to mean + 1*sd)
  #   - Binary variables are rescaled to have a mean of 0 and a difference of 1
  #     between their two categories
  #   - Non-numeric variables that take on more than two values are unchanged
  #   - Variables that take on only one value are unchanged
  # In the new model, centered variables (by default, binary inputs) are
  #   prefixed with "c." and rescaled variables (by default, all other inputs)
  #   are prefixed with "z."
  #
  # Options:
  #   unchanged:  vector of names of parameters to leave unstandardized
  #   standardize.y:  if TRUE, the outcome variable is standardized also
  #   binary.inputs:  options for standardizing binary input variables:
  #     "0/1" (rescale so that the lower value is 0 and the upper is 1)
  #     "-0.5/0.5" (rescale so that the lower value is -0.5 and upper is 0.5)
  #     "center" (rescale so that the mean of the data is 0 and the difference
  #               between the two categories is 1)
  #     "full" (rescale by subtracting the mean and dividing by 2 sd's)
  #     "leave.alone" (do nothing)
  #
  call <- object$call
  if (is.null(call)) call <- object@call
  form <- call$formula
  varnames <- all.vars (form)
  n.vars <- length (varnames)
  #
  # Decide which variables will be unchanged
  #
  transform <- rep ("leave.alone", n.vars)
  if (standardize.y) {
    transform[1] <- "full"
  }
  for (i in 2:n.vars){
    v <- varnames[i]
    if (is.null(call$data)){   # if regression is using the regular workspace
      thedata <- get(v)
    }
    else {                     # if the regression is using a data frame
      thedata <- get(as.character(call$data))[[v]]
    }
    if (is.na(match(v,unchanged))){
      num.categories <- length (unique(thedata[!is.na(thedata)]))
      if (num.categories==2){
        transform[i] <- binary.inputs
      }
      else if (num.categories>2 & is.numeric(thedata)){
        transform[i] <- "full"
      }
    }
  }
  #
  # New variable names:
  #   prefix with "c." if centered or "z." if centered and scaled
  #
  varnames.new <- ifelse (transform=="leave.alone", varnames,
                          ifelse (transform=="full", paste ("z", varnames, sep="."),
                                  paste ("c", varnames, sep=".")))
  transformed.variables <- (1:n.vars)[transform!="leave.alone"]

  if (is.null(call$data)){
    #
    # If the regression is using the regular workspace, define the new variables
    #
    for (i in transformed.variables){
      assign (varnames.new[i], rescale(get(varnames[i]), binary.inputs))
    }
  }
  else {
    #
    # If the regression uses a data frame, define the new variables there
    #
    newvars <- NULL
    for (i in transformed.variables){
      assign (varnames.new[i],
              rescale (get (as.character(call$data)) [[varnames[i]]], binary.inputs))
      newvars <- cbind (newvars, get(varnames.new[i]))
    }
    assign (as.character(call$data),
            cbind (get(as.character(call$data)), newvars))
  }
  #
  # Now call the regression with the new variables
  #
  call.new <- call
  L <- sapply (as.list (varnames.new), as.name)
  names(L) <- varnames
  call.new$formula <- do.call (substitute, list (form, L))
  return (eval (call.new))
}

rescale <- function (x, binary.inputs){
  # function to rescale by subtracting the mean and dividing by 2 sd's
  x.obs <- x[!is.na(x)]
  if (!is.numeric(x)) x <- as.numeric(factor(x))
  if (length(unique(x.obs))==2){
    x <- (x-min(x.obs))/(max(x.obs)-min(x.obs))
    if (binary.inputs=="0/1") return (x)
    else if (binary.inputs=="-0.5,0.5") return (x-0.5)
    else if (binary.inputs=="center") return (x-mean(x.obs))
    else if (binary.inputs=="full") return ((x-mean(x.obs))/(2*sd(x.obs)))
  }
  else {
    return ((x-mean(x.obs))/(2*sd(x.obs)))
  }
}


ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))

    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }

    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(aes(size=1.2), colour="blue")
    return(p)
  }

  lapply(re, f)
}


# lm.cluster with hard coded weights --------------------------------------

sourceURL("https://stackoverflow.com/questions/43942417/r-clustered-robust-standard-errors-using-miceadds-lm-cluster-error-with-subse")

lm.cluster <- function (data, formula, cluster, wgts=NULL, ...)
{
  TAM::require_namespace_msg("multiwayvcov")
  if(is.null(wgts)) {
    mod <- stats::lm(data = data, formula = formula)
  } else {
    data$.weights <- wgts
    mod <- stats::lm(data = data, formula = formula, weights=data$.weights)
  }
  if (length(cluster) > 1) {
    v1 <- cluster
  }
  else {
    v1 <- data[, cluster]
  }
  dfr <- data.frame(cluster = v1)
  vcov2 <- multiwayvcov::cluster.vcov(model = mod, cluster = dfr)
  res <- list(lm_res = mod, vcov = vcov2)
  class(res) <- "lm.cluster"
  return(res)
}


# clean corpus ------------------------------------------------------------


clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Remove stop words
  #corpus <- tm_map(corpus, removeWords,
  #                 c(stopwords("en"), "amp", "chardonnay", "wine", "glass"))
  return(corpus)
}



# marginal effects --------------------------------------------------------

pred <- function(x, y, z) {
  out <- data.frame(measure=deparse(substitute(x)),
                    lowersd=x[y,2],
                    uppersd=x[z,2],
                    diff=x[z,2] - x[y,2],

                    lowerlower=x[y,4],
                    upperlower=x[z,4],
                    lowerdiff=x[z,4] - x[y,4],

                    lowerupper=x[y,3],
                    upperupper=x[z,3],
                    upperdiff=x[z,3] - x[y,3])
  out <- out %>%
    select(1:4, 7,10, everything())

  out
}




# did table ---------------------------------------------------------------

did_table <- function(model, mod_name) {

  "Assumes a model of the form y_i = b_0 + endline*d_0 + treatment*b_1 + endline*treatment*d_1"

  out <- data.frame(measure=c("control","treatment","grp_diff"),
                    baseline=c(model$coefficients[1],
                               sum(model$coefficients[c(1,3)]),
                               model$coefficients[3]),
                    endline=c(sum(model$coefficients[c(1,2)]),
                              sum(model$coefficients[1:4]),
                              sum(model$coefficients[c(3,4)])),
                    time_diff=c(model$coefficients[2],
                                sum(model$coefficients[c(2,4)]),
                                model$coefficients[4]),
                    #se=model$coeficients[4,2],
                    row.names=1:3)

  #print("Assumes a model of the form y_i = b_0 + endline*d_0 + treatment*b_1 + endline*treatment*d_1")
  #print(out)
}

a <- did_table(r1)
a

did_table2 <- function(model, mod_name) {

  "Assumes a model of the form y_i = b_0 + endline*d_0 + treatment*b_1 + endline*treatment*d_1"

  out <- data.frame(model=mod_name,
                    measure=c("control","treatment","difference"),
                    baseline=c(model$coefficients[1],
                               sum(model$coefficients[c(1,3)]),
                               model$coefficients[3]),
                    endline=c(sum(model$coefficients[c(1,2)]),
                              sum(model$coefficients[1:4]),
                              sum(model$coefficients[c(3,4)])),
                    difference=c(model$coefficients[2],
                                 sum(model$coefficients[c(2,4)]),
                                 model$coefficients[4]),
                    #se=model$coeficients[4,2],
                    row.names=1:3)

  print("Assumes a model of the form y_i = b_0 + endline*d_0 + treatment*b_1 + endline*treatment*d_1")
  print(out)
}

did_table(l1, "linear")

did_table(r1, "robust")

summary(l1)
l1$coefficients[,2]

tidy(l1)
augment(l1)
glance(l1)


one_entry <- function(x) {
  for (i in length(x)) attr(x[[i]], "names") <- NULL
  return(x)
}



# Myers index -------------------------------------------------------------

source("https://data.princeton.edu/eco572/R/myers.txt")

myers <- function(target, freq, bin_start, bin_size, na_omit = FALSE) {
  d <- data.frame(target = target, freq = freq)
  d_filter <- seq(bin_start, bin_start + bin_size + 10 - 2)
  if (!all(d_filter %in% target)) stop("Bin setup exceeds available data.")
  d <- d %>%
    filter(target %in% d_filter) %>%
    arrange(target) %>%
    mutate(lastdigit = target %% 10, wgt = c(1:9, rep(10, bin_size - (10 - 1)), 9:1))

  if (na_omit) d <- na.omit(d)

  group_by(d, lastdigit) %>%
    summarise(freq = sum(freq * wgt)) %>%
    mutate(
      freq = freq / sum(freq),
      score = (freq * 100) - 10,
      score = abs(score)
    ) %>%
    summarise(myers = sum(score) / 2)
}


Myers <- function(Value, Age, ageMin = 10, ageMax = 90){

  # hard code period to 10 for digits
  period <- 10

  # must be of same length for indexing
  stopifnot(length(Value) == length(Age))
  stopifnot(ageMin %% period == 0 & ageMax %% period == 0)


  # select out ages, place into matrix for summing over digits
  ind     <- Age >= ageMin & Age < ageMax
  # a row corresponds to a digit
  VA      <- matrix(Value[ind], nrow = period, dimnames = list(0:(period-1), NULL))

  # sum staggered, once without the youngest group but with the oldest one (tab2)
  # and once with the youngest and without the oldest
  tab1    <- rowSums(VA) # differs from other implementations, but matches PASEX

  tab2    <- rowSums(VA[, - 1])

  # weighted tabulation
  TAB     <- tab1 * 1:period + tab2 * c(period:1 - 1)
  # interpret as % that would need to be redistributed...
  my      <- sum(abs(TAB / sum(TAB) - 1 / period)) * 50
  return(my)
}



MyersI <-
  function(x,ages){
    stopifnot(length(x)>40)
    if (missing(ages)) {
      ages <- 0:(length(x)-1)
    }
    # ul = upper limit = largest age evenly divisible by 10 up to 100
    # i.e if highest age is 85, then it spits back 80
    ul <- min(c(max(ages[ages %% 10 == 0]), 100))
    # indices to pick out 2 tabulations
    ind1 <- ages >= 10 & ages < (ul-10)
    ind2 <- ages >= 20 & ages < ul
    # sum by digits, in this case picked out by modulars (%%)
    tab1 <- tapply(x[ind1],ages[ind1] %% 10,sum)
    tab2 <- tapply(x[ind2],ages[ind2] %% 10,sum)
    # weighted tabulation
    TAB <- tab1 * 1:10 + tab2 * 9:0
    # interpret as % that would need to be redistributed...
    sum(abs(TAB/sum(TAB) - .1)) * 50
  }

MyersI(age, age)













