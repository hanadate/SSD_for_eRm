#===== install packages
# if (!require("devtools")) {
#   install.packages("devtools", dependencies = TRUE)}
# devtools::install_github("DejanDraschkow/mixedpower") 
# install.packages("lme4")

#===== Libraries
library(tidyverse)
library(lme4)
library(eRm)
library(mixedpower)
library(doMPI)
#===== load functions
source("hosts_num.R")
source("power_simulation_mpi.R")
source("mixedpower_mpi.R")

#===== LLTM
raschdat1_long <- raschdat1 %>% 
  mutate(person=rownames(.)) %>% 
  pivot_longer(cols=starts_with("I"), names_to="item", values_to="resp") %>% 
  mutate(person=as.integer(person))
items <- unique(raschdat1_long$item)
number_of_items <- length(items)
# Fit the model using glmer
t<-proc.time()
glmer.rasch <- glmer(resp ~ item + (1 | person), 
                     data = raschdat1_long, 
                     family = binomial,
                     control=glmerControl(optCtrl=list(maxfun=100000))
)
proc.time()-t #41sec
summary(glmer.rasch)

# power analysis
glmer.rasch
print(paste0("hosts_num: ",hosts_num(hosts="hosts")))
print(paste0("cores_num: ",cores_num(hosts="hosts")))
n_sim<-10 #1000
print(paste0("n_sim: ",n_sim))
chunkSize <- floor(n_sim/(2*hosts_num(hosts="hosts")))
print(paste0("chunkSize: ",chunkSize))

t<-proc.time()
power.rasch <- mixedpower_mpi(model=glmer.rasch, data=raschdat1_long,
                          fixed_effects=c("item"),
                          simvar="person", steps=c(10,30,50),
                          critical_value=2, n_sim=n_sim,
                          SESOI=FALSE, databased=TRUE,
                          maxCores=hosts_num(hosts="hosts"),
                          chunkSize=chunkSize)

proc.time()-t
#=== laptop(AMD Ryzen 7 7730U with Radeon Graphics 2.00 GH 16 threads): 
#= n_sim=10: 3 mins, n_sim=100: 15 mins, n_sim=1000: 150 mins
#=== 56 threads (8 workers)
#= n_sim=100 & chunkSize=*1: 16 min 
#= n_sim=100 & chunkSize=*2: 16 min 
#= n_sim=1000 & chunkSize=*2: 155 min
print("Finished fitting")
saveRDS(power.rasch, "power_rasch.rds")
print("Saved")