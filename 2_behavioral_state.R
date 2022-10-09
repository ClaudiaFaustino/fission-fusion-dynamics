## publication: https://doi.org/10.1007/s00265-022-03235-0
## Focal follows: Modelling behavioral state (Objective 2)
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

work.model$Month <- as.factor(work.model$Month)
work.model$Month <- relevel(work.model$Month, ref = "6")


## defining the baseline for the response variable for the multinomial GEE
## NOTE: For the multinomial GEE using the function nomLORgee, the J-th response category is treated as baseline.

work.model$Behavior.Class <- as.factor(work.model$Behavior.Class)
work.model$Behavior.Class <- relevel(work.model$Behavior.Class, ref = "T")

## Fit the multinomial GEE models ----

### null model
fit.Beh.null <- nomLORgee(Behavior.Class ~ 1,
                          id = Focal.Number, data = work.model, LORstr = "time.exch")

### using MC pairs as covariate
fit.Beh.MC <- nomLORgee(Behavior.Class ~ MC,
                        id = Focal.Number, data = work.model, LORstr = "time.exch")

### using MC pairs and Month as covariates
fit.Beh.MC.Month<- nomLORgee(Behavior.Class ~ MC + Month,
                             id = Focal.Number, data = work.model, LORstr = "time.exch")


## Diagnostics ----
### significance
waldts(fit.Beh.null, fit.Beh.MC)
waldts(fit.Beh.MC, fit.Beh.MC.Month)
waldts(fit.Beh.null, fit.Beh.MC.Month)

## Check for Multicollinearity
multicollinearity(fit.Beh.MC.Month)


