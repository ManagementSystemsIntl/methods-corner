#Basic Analysis in R

# One of the nice things about R is that you can customize everything
# This also can be a pain, so here are a few basics to get started

options(stringsAsFactors = FALSE) # good to turn this option off; for more see, http://simplystatistics.org/2015/07/24/stringsasfactors-an-unauthorized-biography/

#find current directory
getwd()
#define your directory
setwd("C:/Users/jstein/Desktop/R stuff")


## Reading data in: step by step

# hh <- read.csv("hh_91_baseline.csv")
# 
# From right to left...
# 
# read.csv("hh_91_practice.csv") : reads in `hh_91_practice.csv`.
# 
# <- : assigns the data read in by `read.csv` to an object.
# 
# hh_91_practice : the object that gets assigned
# 
# At the end of this, our csv file data will be in the R dataset `hh`.

#-------------------------------------------------------------------------------
hh <- read.csv("hh_91_baseline.csv")

## Our data

# Let's click on the dataset in the Environment pane in RStudio.

#-------------------------------------------------------------------------------

## Reading data in: Excel and Stata

# * Use `read_excel` to read Excel files.
# * Use `read_dta` for Stata files.

# 
# library(readxl)
# my_excel_data <- read_excel("my_excel_file.xlsx", sheet = 1)
# 
# library(haven)
# my_stata_data <- read_dta("my_stata_file.dta")
# 
# These functions come from the `readxl` and `haven` packages, which we load using
# the `library` function.

#-------------------------------------------------------------------------------

## Looking at data

# Use `class` to see what type of object `hh` is.

class(hh)

# `hh` is a data frame, which is the R name for a dataset.

#-------------------------------------------------------------------------------

## Looking at data: data types

# R stores data in objects of different kinds:
# 
# Vectors -- one-dimensional collection of numbers or character strings
# 
# Data frames -- two-dimensional collection (rows and columns) of mixed data
# 
# Lists -- arbitrary collection of other objects

#-------------------------------------------------------------------------------

## Looking at data: previewing the data

# Use `head` to see the first few rows.

head(hh)

# Or you can just open the dataset in RStudio

#-------------------------------------------------------------------------------

## Looking at data: data frame properties

# How many rows and columns does `hh` have? What are its variables?

nrow(hh)
ncol(hh)
names(hh)

#-------------------------------------------------------------------------------

## Looking at data: modifying variable names

# R is case sensitive, and it's generally not fun to deal with mixed case 
# variable names. 
# 
# Let's make sure we have all lower case variable names.

names(hh) <- tolower(names(hh))

#-------------------------------------------------------------------------------


## Looking at data: variables

# We work with individual variables by using the `$` extraction operator.

hh$agehead

# This is all the data in the `agehead` variable.

#-------------------------------------------------------------------------------

## Looking at data: rows and columns

# We can also use the more flexible `[]` extraction operator.

hh[1:5, c("educhead", "agehead", "hhland")]

# Rows and columns are separated by a comma.
# 
# If you leave out rows or columns (e.g. `hh[1:5, ]`), you get all rows (or 
# columns).

#-------------------------------------------------------------------------------

## Looking at data: variable types

# Variables in a data frame are vectors -- numeric vectors, character vectors, 
# etc.

class(hh$agehead)

#We may want to change the variable type some day
#a few good functions to know:
# varname <- as.character(n$a) #numeric to character
# varname <-as.numeric(n$a) #character to numeric
# varname <-as.Date(my_dates, format = "%m-%d-%Y") #date format
# varname <- as.factor(n$a)


#-------------------------------------------------------------------------------

## Looking at data: simple summaries

# We can now get summary statistics from the data in our dataset.

table(hh$agehead)
mean(hh$agehead)
summary(hh$agehead)
#summary() provides different output for each variable, depending on its class. 
#summary() displays the minimum, 1st quartile, median,
#mean, 3rd quartile, and maximum. 
str(hh) #this provides info on the structure of the dataset
table(hh$agehead, hh$sexhead)


#-------------------------------------------------------------------------------
#check for missing values
sum(is.na(hh$agehead))
#wow! we are so lucky: no missing values in household head age
# What would it look like if we did have missing values?

x <- c(1, NA, 2, NA, 3)
is.na(x)
sum(is.na(x))

#checking if ANY variables have missing values
sapply(hh, function(x) sum(is.na(x)))

#Dreams do come true! 

# **dplyr** -- tools for filtering, manipulating, aggregating data
# **tidyr** -- tools for reshaping data
# these are called packages. There are packages that do pretty much everything.
# to install a package, type install.packages("package name")
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
#let's find hhs with young hh heads
hh <- hh %>% 
  mutate(young_hh = ifelse(agehead < 30, "young", "old"))

ncol(hh) #and look at that, a new column is born!

#dplyr is a very neat package that uses pipes: these %>%
#I like to think about code as a story.
# pipes are similar to saying, "and then." 
hh %>% #use the hh data frame...and then
  group_by(young_hh) %>%  #group by young_hh
  summarize(n_obs = n(), 
            mean_fam = mean(famsize), 
            mean_age = mean(agehead), 
            mean_ed = mean(educhead)) #and then summarize these variables and call them these new names

#let's also look at female compared to male headed households
hh %>% #use the hh data frame
  group_by(sexhead) %>%  #and then group by sex
  summarize(n_obs = n(), 
            mean_fam = mean(famsize), 
            mean_age = mean(agehead), 
            mean_ed = mean(educhead)) #and then summarize these

#we can use pipes to filter rows, too
# Let's just look at HH with heads older than 50
hh2 <- hh %>% 
  filter(agehead>50) 
#and then sort
hh2 <- hh2[-order(hh2$agehead),]  #descending, note the - before order
hh2 <- hh2[order(hh2$agehead),]   #ascending

## Filtering columns
# We want to look only at the villiage id and the age of the head of HH from the recently filtered dataset.
hh3 <- hh2 %>% 
  select(vill, agehead)

#------------------------------------------------------------------------------------------------------------
#Ok, let's get an idea of the distribution
#I like to check the distribution of key varaibles via a graph
install.packages("ggplot2")
library(ggplot2)
ggplot(hh, aes(x=agehead)) + #/define the dataset and the x axis/#
  geom_histogram(binwidth=1, col="white", fill="darkblue", alpha=0.5) + #/set the graph type to histogram/#
  geom_density(aes(y = ..count..)) #/add a density line/#


# you can also use numbers
as.data.frame(table(hh$agehead))

#let's compare the distribution between old and young households
ggplot(hh, aes(agehead, fill = as.factor(young_hh), 
                color = as.factor(young_hh))) + 
  geom_density(alpha = 0.5)

#you can play around with this some more.
#I picked household head age, but there are other variables that might be more interesting

#---------------------------------------------------------------------------------------------------
#OK, random assignment time
set.seed(11082016) #we set the seed for replicability
#there are many ways to do random assignment in R, including used pacakges. I will show you two approaches
hh$T2 <- runif(nrow(hh),0,1) #assign a random number to each row

#sort the row
hh_sorted <- hh %>% 
  arrange(T2)

#create a cutoff to determine treatment or control. 0.5 is pretty standard.
hh_sorted2 <- hh_sorted %>% 
  mutate(treatment = ifelse(T2 < .5, "1", "0"))

#let's see how that worked out
hh_sorted2 %>% 
  group_by(treatment) %>% 
  summarize(n_obs = n()) #it looks like we have a fairly even distribution between T and C

#Another approach: we could just randomly apply 1s and 0s
hh$T<-sample(0:1, nrow(hh), replace=T) #random selection with replacement
hh %>% 
  group_by(T) %>% 
  summarize(n_obs = n()) #looks balanced; of course, we will test the balance later

#let's compare the distribution between treatment and control
ggplot(hh, aes(agehead, fill = as.factor(T), 
               color = as.factor(T))) + 
  geom_density(alpha = 0.5)
#beautiful

#let's see if the visual mataches the numbers
t.test(hh$agehead~ hh$T) #this is to run a simple difference of means test
#the code below compares each of the variables between treatment and control
lapply(hh[,c("agehead"
             , "sexhead"
             , "educhead"
             , "famsize"
             , "hhasset"
             , "expfd"
             , "expnfd"
             , "exptot"
             , "rice"
             , "wheat"
             , "milk"
             , "potato"
             , "egg"
             , "oil")]
       , function(x) t.test(x ~ hh$T, var.equal = TRUE))



#-------------------------------------------------------------------------
#Analysis
#Our selected outcome variable is the
#natural logarithm of the total expenditures.
hh$lnexptot<-log(hh$exptot) 
t.test(hh$exptot~ hh$rand)
#randomization seems to have worked, as there is no difference 

#let's fast forward to endline
end_hh <- read.csv("hh_91_endline.csv")
end_hh$lnexptot<-log(end_hh$exptot) 
t.test(end_hh$exptot~ end_hh$T)
#WOW! There is quite a difference between treatment and control!

#It is possible that the program impact depend on the characteristics of the microcredit
#beneficiaries. The evidence of a heterogeneous effect could help to
#unravel the channels through which the impact is generated.
hetero_impact_model <- glm(lnexptot ~ T + sexhead + educhead + famsize +agehead,  data = end_hh)
summary(hetero_impact_model)
#On average, each additional year of education of the household head is associated
# with a 6% increase of the program impact

# Other useful functions 
coefficients(hetero_impact_model) # model coefficients
confint(hetero_impact_model, level=0.95) # CIs for model parameters 
fitted(hetero_impact_model) # predicted values
residuals(hetero_impact_model) # residuals
anova(hetero_impact_model) # anova table 
vcov(hetero_impact_model) # covariance matrix for model parameters 
plot(hetero_impact_model) #view regression plots

