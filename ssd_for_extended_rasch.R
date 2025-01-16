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
library(doParallel)
library(mirt)
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
t<-proc.time()
power.rasch <- mixedpower(model=glmer.rasch, data=raschdat1_long,
                          fixed_effects=c("item"),
                          simvar="person", steps=c(10,30,50),
                          critical_value=2, n_sim=10,
                          SESOI=FALSE, databased=TRUE)

proc.time()-t
# nsim 10: 197sec(thinkpad), 62sec(a311), sec(ishioka)

#=== laptop(AMD Ryzen 7 7730U with Radeon Graphics 2.00 GH 16 threads): 
#= n_sim=10: 3 mins, n_sim=100: 15 mins, n_sim=1000: 150 mins
#=== 56 threads (8 workers)
#= n_sim=100 & chunkSize=*1: 16 min 
#= n_sim=100 & chunkSize=*2: 16 min 
#= n_sim=1000 & chunkSize=*2: 155 min

model<-c("RM","RSM")
a<-c(.5,1.0,1.5)
d<-c(-1.0,0,1.0)
N<-c(10,30,50)

simdata(a=a,d=d,N=N[1],itemtype="dich")

