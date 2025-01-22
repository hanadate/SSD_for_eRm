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
a<-seq(0,1,.5) # Discrimination parameters
d<-seq(-1,1,.5) # Difficulty parameters
ad<-expand.grid(a,d) %>% 
  `colnames<-`(c("a","d")) %>% 
  mutate(a_d=paste0(a,"_",d))
N<-seq(50,200,50)

#==== RM
# create data
set.seed(1)
x1 <- simdata(a=ad$a,d=ad$d,N=max(N),itemtype="dich") %>% 
  as.data.frame() %>% 
  `colnames<-`(ad$a_d) %>% 
  mutate(`!!`=0) %>% 
  select(`!!`, everything())

# Fit
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

# Power
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
(power.N50.rasch <- power.rasch.ad %>% 
  select(`50`,a,d) %>% 
  filter(a>=0) %>% 
  pivot_wider(names_from=a, values_from=`50`) %>% 
  as.matrix()
)
stargazer(power.N50.rasch)
(power.N200.rasch <- power.rasch.ad %>% 
    select(`200`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`200`) %>% 
    as.matrix()
)
stargazer(power.N200.rasch)

#==== RSM
# create data
# Define item parameters
a<-seq(0,1,.5) # Discrimination parameters
d<-seq(-1,1,.5) # Difficulty parameters
ad<-expand.grid(a,d) %>% 
  `colnames<-`(c("a","d")) %>% 
  mutate(a_d=paste0(a,"_",d),
         d2=d-1)
N<-seq(50,200,50)
# Specify item type as 'graded' for polytomous data
itemtype <- rep('graded', length(a))
# Generate data
set.seed(1)
x2 <- simdata(a=ad$a, d=matrix(c(ad$d, ad$d2), nrow(ad), 2), N=max(N), 
              itemtype=rep("graded", nrow(ad))) %>% 
  as.data.frame() %>% 
  `colnames<-`(ad$a_d) %>% 
  mutate(`!!`=0) %>% 
  select(`!!`, everything())

# mapping matrix for linear tree
linear_tree_map <- data.frame(node1=c(0,1,1), node2=c(NA,0,1)) %>% 
  `rownames<-`(c(0,1,2)) %>% 
  as.matrix()
# add a column has original responses for linear tree map 
linear_tree_map_resp <- 
  rownames_to_column(as.data.frame(linear_tree_map), var="resp") %>% 
  mutate(resp=as.double(resp))
rsmdat_long <- x2 %>% 
  as.data.frame() %>% 
  # long form
  mutate(person=rownames(.)) %>%
  pivot_longer(cols=-person, names_to="item", values_to="resp") %>% 
  mutate(person=as.integer(person),
         item=factor(item)) %>%  
  left_join(., linear_tree_map_resp, by=c("resp")) %>% 
  pivot_longer(cols=starts_with("node"),
               names_to="node",
               values_to="resp_node") %>% 
  mutate(person=as.integer(person),
         item=factor(item))
rsmdat_long$item <- factor(rsmdat_long$item, levels=colnames(x2))

# lme4 for RSM
t <- proc.time()
(glmer.rsm <- glmer(resp_node ~ -1 + item + node + (1 | person), 
                   family=binomial, data=rsmdat_long))
proc.time()-t # 14sec
saveRDS(glmer.rsm, "glmer_rsm.rds")
glmer.rsm <- readRDS("glmer_rsm.rds")

# Power
t<-proc.time()
power.rsm <- mixedpower(model=glmer.rsm, data=rsmdat_long,
                          fixed_effects=c("item"),
                          simvar="person", steps=N,
                          critical_value=2, n_sim=1000,
                          SESOI=FALSE, databased=TRUE)
proc.time()-t 
saveRDS(power.rsm, "power_rsm.rds")
power.rsm <- readRDS("power_rsm.rds")
# Core(TM) i9-12900   2.40 GHz: 48min

(power.rsm.ad <- power.rsm %>% 
  mutate(effect=str_remove(effect, "item")) %>% 
  filter(effect!="nodenode2") %>% 
  separate_wider_delim(effect,delim="_",names=c("a","d")))

# sample size 50
(power.N50.rsm <- power.rsm.ad %>% 
    select(`50`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`50`) %>% 
    as.matrix()
)
stargazer(power.N50.rsm)
(power.N200.rsm <- power.rsm.ad %>% 
    select(`200`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`200`) %>% 
    as.matrix()
)
stargazer(power.N200.rsm)

#===== LLTM, LRSM




