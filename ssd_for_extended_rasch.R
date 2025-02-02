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
library(jtools)
library(foreach)

#===== RM, RSM
a<-seq(0,1,.5) # Discrimination parameters
d<-seq(-1,1,.5) # Difficulty parameters
ad<-expand.grid(a,d) %>% 
  `colnames<-`(c("a","d")) %>% 
  mutate(a_d=paste0(a,"_",d))
N<-c(50,200,1000)

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
(glmer.rasch <- glmer(resp ~ -1 + item + (1 | person), 
                      data = raschdat_long, 
                      family = binomial))
proc.time()-t #15sec
saveRDS(glmer.rasch, "glmer_rasch.rds")
glmer.rasch <- readRDS("glmer_rasch.rds")

# Power
t<-proc.time()
set.seed(1)
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
(power.N1000.rasch <- power.rasch.ad %>% 
    select(`1000`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`1000`) %>% 
    as.matrix()
)
stargazer(power.N1000.rasch)
#==== RSM
# create data
# Define item parameters
a<-seq(0,1,.5) # Discrimination parameters
d<-seq(-1,1,.5) # Difficulty parameters
ad<-expand.grid(a,d) %>% 
  `colnames<-`(c("a","d")) %>% 
  mutate(a_d=paste0(a,"_",d),
         d2=d-1)
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
proc.time()-t # 
saveRDS(glmer.rsm, "glmer_rsm.rds")
glmer.rsm <- readRDS("glmer_rsm.rds")

# Power
t<-proc.time()
set.seed(1)
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
(power.N1000.rsm <- power.rsm.ad %>% 
    select(`1000`,a,d) %>% 
    filter(a>=0) %>% 
    pivot_wider(names_from=a, values_from=`1000`) %>% 
    as.matrix()
)
stargazer(power.N1000.rsm)

#==== gap btw RSM - RM
power.N50.rsm
power.N50.rasch
gap.rsm.rm.N50<- matrix(as.numeric(power.N50.rsm[,2:4]) - as.numeric(power.N50.rasch[,2:4]), 5,3)
stargazer(gap.rsm.rm.N50)
gap.rsm.rm.N200<- matrix(as.numeric(power.N200.rsm[,2:4]) - as.numeric(power.N200.rasch[,2:4]), 5,3)
stargazer(gap.rsm.rm.N200)

ggplot(data.frame(RSM=as.numeric(power.N50.rsm[,2:4]), RM=as.numeric(power.N50.rasch[,2:4])), aes(RSM,RM))+
  geom_point()+
  geom_abline(slope=1, intercept=0, linetype="dashed")+
  jtools::theme_apa()+
  scale_x_continuous(expand=expansion(mult=c(.1,.1)))+
  scale_y_continuous(expand=expansion(mult=c(.1,.1)))
ggsave("gap_rsm_rm.png",width=3,height=3)
cor(as.numeric(power.N50.rsm[,2:4]),as.numeric(power.N50.rasch[,2:4]))

#===== LLTM, LRSM
# create design matrix
eta1 <- c(2:3); eta2 <- c(2:3); eta3 <- c(2:3)
eta1eta2 <- expand.grid(eta1=eta1, eta2=eta2, eta3=eta3) %>% 
  filter(eta1>=eta2 & eta2>=eta3)
W <- foreach(i=1:nrow(eta1eta2)) %do% {
  W.tmp <- expand.grid(factor(seq(eta1eta2$eta1[i])), 
                       factor(seq(eta1eta2$eta2[i])),
                       factor(seq(eta1eta2$eta3[i]))) %>% 
    as.data.frame %>% 
    mutate(item=factor(row_number())) %>% 
    select(item, Var1, Var2, Var3)
  return(W.tmp)
}

#=== LLTM
# create dataset
# Fix a=.5, d=.5, N=c(50,200,1000)
set.seed(1)
x3 <- simdata(a=rep(.5,nrow(W[[lW]])),d=rep(.5,nrow(W[[lW]])),N=max(N),itemtype="dich") %>% 
  as.data.frame() %>% 
  mutate(`!!`=0) %>% 
  select(`!!`,everything())
lW <- length(W)

lltmdat_long <- foreach(i=1:length(W)) %do% { 
  lltmdat_long.tmp <- x3[,1:(nrow(W[[i]])+1)] %>% 
    mutate(person=rownames(.)) %>%
    pivot_longer(cols=-person, names_to="item", values_to="resp") %>% 
    mutate(person=as.integer(person),
           item=factor(str_remove(item,"Item_"))) %>% 
    inner_join(., W[[i]], by="item")
}

glmer.lltm <- foreach(i=1:length(lltmdat_long)) %do% {
  glmer(resp ~ -1 + Var1 + Var2 + Var3 + (1 | person), 
        data = lltmdat_long[[i]], 
        family = binomial)
}
glmer.lltm
saveRDS(glmer.lltm, "glmer_lltm.rds")
glmer.lltm <- readRDS("glmer_lltm.rds")
t <- proc.time()
power.lltm <- foreach(i=1:length(lltmdat_long)) %do% {
  set.seed(1)
  mixedpower(model=glmer.lltm[[i]], data=lltmdat_long[[i]],
             fixed_effects=c("Var1","Var2","Var3"),
             simvar="person", steps=N,
             critical_value=2, n_sim=1000,
             SESOI=FALSE, databased=TRUE)
}
proc.time()-t #44603sec
saveRDS(power.lltm, "power_lltm.rds")
power.lltm <- readRDS("power_lltm.rds")
power.lltm

#==== LRSM
# create dataset
# Fix a=.5, d=.5, N=c(50,200,1000)
set.seed(1)

x4 <- simdata(a=rep(.5,nrow(W[[lW]])),
              d=matrix(c(rep(.5,nrow(W[[lW]])), rep(.5-1,nrow(W[[lW]]))),nrow(W[[lW]]),2),
              N=max(N),itemtype="graded") %>% 
  as.data.frame() %>% 
  mutate(`!!`=0) %>% 
  select(`!!`,everything())
lW <- length(W)

# mapping matrix for linear tree
linear_tree_map <- data.frame(node1=c(0,1,1), node2=c(NA,0,1)) %>% 
  `rownames<-`(c(0,1,2)) %>% 
  as.matrix()
# add a column has original responses for linear tree map 
linear_tree_map_resp <- 
  rownames_to_column(as.data.frame(linear_tree_map), var="resp") %>% 
  mutate(resp=as.double(resp))

lrsmdat_long <- foreach(i=1:length(W)) %do% { 
  lrsmdat_long.tmp <- x4 %>% 
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
           item=factor(str_remove(item,"Item_"))) %>% 
    inner_join(., W[[i]], by="item")
  return(lrsmdat_long.tmp)
}

# lme4 for LRSM
t <- proc.time()
glmer.lrsm <- foreach(i=1:length(lrsmdat_long)) %do% {
  glmer(resp_node ~ -1 + Var1 + Var2 + Var3 + node + (1 | person), 
        data = lrsmdat_long[[i]], 
        family = binomial)
}
proc.time()-t # 48sec
saveRDS(glmer.lrsm, "glmer_lrsm.rds")
glmer.lrsm <- readRDS("glmer_lrsm.rds")

# Power
t <- proc.time()
power.lrsm <- foreach(i=1:length(lrsmdat_long)) %do% {
  set.seed(1)
  mixedpower(model=glmer.lrsm[[i]], data=lrsmdat_long[[i]],
             fixed_effects=c("Var1","Var2","Var3","node"),
             simvar="person", steps=N,
             critical_value=2, n_sim=1000,
             SESOI=FALSE, databased=TRUE)
}
proc.time()-t #23564sec
saveRDS(power.lrsm, "power_lrsm.rds")
power.lrsm <- readRDS("power_lrsm.rds")

