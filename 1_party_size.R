## publication: https://doi.org/10.1007/s00265-022-03235-0
## Focal follows: Modelling party size (Objective 1)
## author: Claudia Faustino & Fabio L. Matos
## date: Jan 2021

## load packages
library(geepack) ## GEE models
library(car) ## for the anova
library(ggstatsplot) ## for data visualization
library(tidymodels) ## required for plotting

## Load data and pre-processing ----
work.model <- read.csv("~/dataset.csv")

### Defining reference levels for the explanatory variables
work.model$MC <- as.factor(work.model$MC)
work.model$MC <- relevel(work.model$MC, ref = "1.Yes")

work.model$Year.month <- as.factor(work.model$Year.month)
work.model$Year.month <- relevel(work.model$Year.month, ref = "201606")

work.model$Time.Day <- as.factor(work.model$Time.Day)
work.model$Time.Day <- relevel(work.model$Time.Day, ref = "1.morning")

work.model$Behavior.Class <- as.factor(work.model$Behavior.Class)
work.model$Behavior.Class <- relevel(work.model$Behavior.Class, ref = "T")


## Fit the GLM-GEE model ----
mod_party_size <- geeglm(Group.Size ~ Time.Day + MC + Behavior.Class + Year.month,
                      id=Focal.Number, 
                      data=work.model, 
                      corstr="independence", 
                      family= poisson(link = "log"))

### Model summary
summary(mod_party_size)

## Diagnostics ----
### significance
anova(mod_party_size)

### collinearity
car::vif(mod_party_size)

### r squared values from GEE models based on Zheng (2000) 

gee_model <- mod_party_size

Y_bar = mean(gee_model$y, na.rm = T)
rsquare_gee <- 1-(sum(gee_model$weights * (gee_model$y - gee_model$fitted.values)^2, na.rm = T)/sum(gee_model$weights*(gee_model$y - Y_bar)^2, na.rm = T))
rsquare_gee

## Data visualization ----
### Dot-and-whisker plot 

ggcoefstats(
  x = mod_party_size,
  only.significant = TRUE,
  point.args = list(size = 3, color = " grey"),
  stats.label.args = list(
    
    force        = 0.5,
    nudge_x      = 4,
    nudge_y      = 1.5,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2,
    size = 3.5,
    box.padding = unit(0.5, "lines")
    
  ),
  title = "Coefficients with statistical significance for Party size",
)  
