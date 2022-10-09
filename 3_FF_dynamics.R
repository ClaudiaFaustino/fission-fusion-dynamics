## publication: https://doi.org/10.1007/s00265-022-03235-0
## Focal follows: Modelling Fission-fusion dynamics (Objective 3)
## author: Claudia Faustino & Fabio L. Matos
## date: September 2021

## load packages
library(dplyr)     ## for data manipulation
library(multgee)  ## for multinomial GEE models
library(performance) ## for assessing multicollinearity

## Load data and pre-processing ----
work.model <- read.csv("~/dataset.csv")

### Defining reference levels for the explanatory variables
work.model$MC <- as.factor(work.model$MC)
work.model$MC <- relevel(work.model$MC, ref = "1.Yes")

## defining the baseline for the response variable for the multinomial GEE
## NOTE: For the multinomial GEE using the function nomLORgee, the J-th response category is treated as baseline.

work.model$Change.Class <- as.factor(work.model$Change.Class)
work.model$Change.Class <- relevel(work.model$Change.Class, ref = "ST")

## Fit the multinomial GEE models ----

### null model
fit.Struct.null <- nomLORgee(Change.Class ~  1,
                          id = Focal.Number, data = work.model, LORstr = "time.exch")

### using MC pairs as covariate
fit.Struct.MC <- nomLORgee(Change.Class ~  MC,
                         id = Focal.Number, data = work.model, LORstr = "time.exch")


### using MC pairs and hour (continuous) as covariates
fit.Struct.MC.H <- nomLORgee(Change.Class ~  MC + Hour,
                          id = Focal.Number, data = work.model, LORstr = "time.exch")
 

## Diagnostics ----
### significance
waldts(fit.Struct.null, fit.Struct.MC)
waldts(fit.Struct.MC, fit.Struct.MC.H)
waldts(fit.Struct.null, fit.Struct.MC.H)

## Check for Multicollinearity
multicollinearity(fit.Struct.MC.H)


