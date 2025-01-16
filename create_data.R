# https://cran.r-project.org/web/packages/mlpwr/vignettes/IRT_Vignette.html
#===== Libraries
library(tidyverse)
library(simTool)
library(psych)
library(eRm)
library(mirt)
library(mlpwr)
#===== Create data
# Define item parameters
a <- c(0.1, 1.0, 10, 100) # Discrimination parameters
d <- c(1, 1, 1, 1)      # Difficulty parameters

# Simulate data
set.seed(1)
x1 <- simdata(a = a, d = d, N = 1000, itemtype="dich")
set.seed(2)
x2 <- simdata(a = a, d = d, N = 1000, itemtype="dich")

x3 <- x1 + x2

summary(x1)
summary(x2)
summary(x3)
apply(x1, 2,var, na.rm=TRUE)
