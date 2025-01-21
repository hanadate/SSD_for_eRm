#===== install packages
# if (!require("devtools")) {
#   install.packages("devtools", dependencies = TRUE)}
# devtools::install_github("DejanDraschkow/mixedpower") 
# install.packages("lme4")
Sys.setenv(LANGUAGE="en")
#===== Libraries
library(tidyverse)
library(lme4)
library(eRm)
library(mixedpower)
library(doParallel)
library(mirt)
library(stargazer)

#===== RM, RSM
#====
model<-c("RM","RSM")
a<-seq(0,1,.5) # Discrimination parameters
d<-seq(-1,1,.5) # Difficulty parameters
ad<-expand.grid(a,d) %>% 
  `colnames<-`(c("a","d")) %>% 
  mutate(a_d=paste0(a,"_",d))
N<-seq(50,200,50)

# create data
set.seed(1)
x1 <- simdata(a=ad$a,d=ad$d,N=max(N),itemtype="dich") %>% 
  as.data.frame() %>% 
  `colnames<-`(ad$a_d) %>% 
  mutate(`!!`=0) %>% 
  select(`!!`, everything())
set.seed(2)
x2 <- simdata(a=ad$a,d=ad$d,N=max(N),itemtype="dich") %>% 
  as.data.frame() %>%
  `colnames<-`(ad$a_d) %>% 
  mutate(`!!`=0)%>% 
  select(`!!`, everything())
x3 <- x1+x2 #%>% 
  # dendrify
  

raschdat_long <- x1 %>% 
  mutate(person=rownames(.)) %>%
  pivot_longer(cols=-person, names_to="item", values_to="resp") %>% 
  mutate(person=as.integer(person),
         item=factor(item))
raschdat_long$item <- factor(raschdat_long$item, levels=colnames(x1))
t <- proc.time()
(glmer.rasch <- glmer(resp ~ 0 + item + (1 | person), 
                     data = raschdat_long, 
                     family = binomial))
proc.time()-t #15sec
saveRDS(glmer.rasch, "glmer_rasch.rds")
glmer.rasch <- readRDS("glmer_rasch.rds")

t<-proc.time()
power.rasch <- mixedpower(model=glmer.rasch, data=raschdat_long,
                          fixed_effects=c("item"),
                          simvar="person", steps=N,
                          critical_value=2, n_sim=1000,
                          SESOI=FALSE, databased=TRUE)
proc.time()-t 
# Core(TM) i9-12900   2.40 GHz: 3.2hour9
saveRDS(power.rasch, "power_rasch.rds")
power.rasch <- readRDS("power_rasch.rds")
power.rasch
multiplotPower(power.rasch)

(power.rasch.ad <- power.rasch %>% 
  mutate(effect=str_remove(effect, "item")) %>% 
  separate_wider_delim(effect,delim="_",names=c("a","d")))

# sample size 50
(pwr.N50.rasch <- power.rasch.ad %>% 
  select(`50`,a,d) %>% 
  filter(a>=0) %>% 
  pivot_wider(names_from=a, values_from=`50`) %>% 
  as.matrix()
)
stargazer(pwr.N50.rasch)
(pwr.N200.rasch <- power.rasch.ad %>% 
    select(`200`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`200`) %>% 
    as.matrix()
)
stargazer(pwr.N200.rasch)
#=====
#===== LLTM, LRSM
