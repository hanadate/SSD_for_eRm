#===== install packages
# if (!require("devtools")) {
#   install.packages("devtools", dependencies = TRUE)}
# devtools::install_github("DejanDraschkow/mixedpower") 
# install.packages("lme4")

#===== Libraries
library(tidyverse)
library(lme4)
library(mixedpower)
library(doMPI)

#===== Load example models
source("replicate_eRm_by_glmer.R")

#===== LLTM
glmer.rasch
t<-proc.time()
cl <- startMPIcluster(count=56-1)
registerDoMPI(cl)
power.rasch <- mixedpower(model=glmer.rasch, data=raschdat1_long,
                          fixed_effects=c("item"),
                          simvar="person", steps=c(10,20,30,40,50,60),
                          critical_value=2, n_sim=10,
                          SESOI=FALSE, databased=TRUE,
                          maxCores=detectCores())
closeCluster(cl)
proc.time()-t
#=== laptop(AMD Ryzen 7 7730U with Radeon Graphics 2.00 GH 16 threads): 
#= steps 6 x n_sim 10: 5 mins
saveRDS(power.rasch, "power_rasch.rds")
