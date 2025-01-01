# replicate example for eRm package by glmer.
#===== libraries
library(eRm)
library(lme4)
library(tidyverse)
library(stargazer)

#===== Rasch model
#==== eRm
erm.rasch <- RM(raschdat1, sum0=FALSE)
# pres.rasch <- person.parameter(res.rasch)

#==== glmer
raschdat1_long <- raschdat1 %>% 
  mutate(person=rownames(.)) %>% 
  pivot_longer(cols=starts_with("I"), names_to="item", values_to="resp")
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

#==== compare glmer with eRm 
glmer.rasch.est.se <- summary(glmer.rasch)$coefficients %>% 
  as.data.frame %>% 
  mutate(item=rownames(.)) %>% 
  mutate(item=str_replace(item, "item", "")) %>% 
  rename(glmer.est=Estimate,
         glmer.se="Std. Error") %>% 
  select(item, glmer.est, glmer.se) %>% 
  # invert easiness to difficulty.
  mutate(glmer.est=-1*glmer.est)

erm.rasch.est <- erm.rasch$etapar %>% 
  as.data.frame() %>% 
  mutate(item=names(erm.rasch$etapar)) %>% 
  rename(eRm.est=".")
erm.rasch.se <- erm.rasch$se.eta %>% 
  as.data.frame %>% 
  mutate(item=names(erm.rasch$etapar)) %>% 
  rename(eRm.se=".")

erm.rasch.est.se <- full_join(erm.rasch.est,erm.rasch.se, by="item")
compare.rasch <- full_join(glmer.rasch.est.se, erm.rasch.est.se, by="item") %>% 
  filter(item!="(Intercept)") %>% 
  mutate(item=as.integer(str_remove(item,"I"))) %>%
  arrange(item)

stargazer(compare.rasch, summary=FALSE, rownames=FALSE,
          omit.table.layout="-!")

#===== LLTM
#==== eRm
# design matrix
W <- matrix(c(1,2,1,3,2,2,2,1,1,1),ncol=2) %>% 
  as.data.frame() %>% 
  `colnames<-`(c("eta 1","eta 2")) %>% 
  mutate(item=paste0("I",row_number()))
erm.lltm <- LLTM(lltmdat2, W=W[1:2], sum0=FALSE)

#==== glmer
lltmdat2_long <- lltmdat2 %>% 
  mutate(person=rownames(.)) %>% 
  pivot_longer(cols=starts_with("I"), names_to="item", values_to="resp") %>%
  inner_join(., W, by=c("item"))
items <- unique(lltmdat2_long$item)
number_of_items <- length(items)
# Join items
# Fit the model using glmer
t<-proc.time()
glmer.lltm <- glmer(resp ~ `eta 1` + `eta 2` + (1|person), 
                     data=lltmdat2_long, family=binomial,
                     control=glmerControl(optCtrl=list(maxfun=100000))
)
proc.time()-t #  sec
summary(glmer.lltm)

#==== compare glmer with eRm 
glmer.lltm.est.se <- summary(glmer.lltm)$coefficients %>% 
  as.data.frame %>% 
  mutate(item=rownames(.)) %>% 
  mutate(item=str_replace(item, "item", "")) %>% 
  rename(glmer.est=Estimate,
         glmer.se="Std. Error") %>% 
  select(item, glmer.est, glmer.se) %>% 
  # not need to invert easiness to difficulty.
  mutate(glmer.est=glmer.est)

erm.lltm.est <- erm.lltm$etapar %>% 
  as.data.frame() %>% 
  mutate(item=names(erm.lltm$etapar)) %>% 
  rename(eRm.est=".")
erm.lltm.se <- erm.lltm$se.eta %>% 
  as.data.frame %>% 
  mutate(item=names(erm.lltm$etapar)) %>% 
  rename(eRm.se=".")

erm.lltm.est.se <- full_join(erm.lltm.est,erm.lltm.se, by="item")
compare.lltm <- full_join(glmer.lltm.est.se, erm.lltm.est.se, by="item") %>% 
  filter(item!="(Intercept)") %>% 
  mutate(item=as.integer(str_remove(item,"F"))) %>%
  arrange(item) %>% 
  rename(eta=item)

stargazer(compare.lltm, summary=FALSE, rownames=FALSE, 
          omit.table.layout="-!")

#===== LRSM
#==== eRm 
erm.lrsm <- LRSM(lrsmdat, mpoints = 2, groupvec = 1, sum0 = FALSE)
# design matrix is automatically generated
W <- erm.lrsm$W %>% 
  as.data.frame() %>% 
  mutate(item.cate.mp=rownames(.)) %>% 
  separate_wider_delim(item.cate.mp, delim=" ", names=c("item.cate","mp")) %>% 
  separate_wider_delim(item.cate, delim=".", names=c("item","cate"))

#==== glmer
# mapping matrix for linear tree
linear_tree_map <- data.frame(node1=c(0,1,1,1), node2=c(NA,0,1,1), node3=c(NA,NA,0,1)) %>% 
  `rownames<-`(c(0,1,2,3)) %>% 
  as.matrix()
# add a column has original responses for linear tree map 
linear_tree_map_resp <- 
  rownames_to_column(as.data.frame(linear_tree_map), var="resp") %>% 
  mutate(resp=as.double(resp))
# dendrify
lrsmdat_long <- lrsmdat %>%
  rownames_to_column(., var="person") %>%
  pivot_longer(cols=starts_with("I"),
               names_to="item.mp",
               values_to="resp") %>%
  separate_wider_delim(item.mp, delim=".",names=c("item","mp")) %>% 
  left_join(., linear_tree_map_resp, by=c("resp")) %>% 
  pivot_longer(cols=starts_with("node"),
               names_to="node",
               values_to="resp_node") %>% 
  arrange(person,mp,item,node) %>% 
  mutate(item=as.factor(item),
         node=as.factor(node),
         mp=as.factor(mp),
         node=str_replace(node, "node", "c")) %>% 
  rename(cate=node) %>% 
  # Join design matrix
  inner_join(., W, by=c("mp","item","cate")) %>% 
  select(person,mp,item,cate,starts_with("eta"),resp_node,resp)

# Fit the model using glmer
t<-proc.time()
glmer.lrsm <- glmer(resp_node ~ 
                      `eta 1`+`eta 2`+`eta 3`+`eta 4`+`eta 5` +
                      (1|person), 
                    data=lrsmdat_long, family=binomial,
                    control=glmerControl(optCtrl=list(maxfun=100000))
)
proc.time()-t #  3sec
#==== compare glmer with eRm 
summary(glmer.lrsm)$coefficients
glmer.lrsm@theta
erm.lrsm$etapar
erm.lrsm$betapar

glmer.lrsm.est.se <- summary(glmer.lrsm)$coefficients %>% 
  as.data.frame %>% 
  mutate(item=rownames(.)) %>% 
  mutate(item=str_replace(item, "item", "")) %>% 
  mutate(item=str_replace(item, "nodenode", "c")) %>% 
  mutate(item=str_replace(item, "mp","")) %>% 
  rename(glmer.est=Estimate,
         glmer.se="Std. Error") %>% 
  select(item, glmer.est, glmer.se) %>% 
  # not need to invert easiness to difficulty.
  mutate(glmer.est=glmer.est,
         item=str_replace_all(item,"`","")) %>% 
  filter(item!="(Intercept)")

erm.lrsm.est <- erm.lrsm$etapar %>% 
  as.data.frame() %>% 
  mutate(item=names(erm.lrsm$etapar)) %>% 
  rename(eRm.est=".")
erm.lrsm.se <- erm.lrsm$se.eta %>% 
  as.data.frame %>% 
  mutate(item=names(erm.lrsm$etapar)) %>% 
  rename(eRm.se=".")

erm.lrsm.est.se <- full_join(erm.lrsm.est,erm.lrsm.se, by="item")

compare.lrsm <- full_join(glmer.lrsm.est.se, erm.lrsm.est.se, by=c("item")) %>% 
  arrange(item) %>% 
  rename(eta=item) %>% 
  mutate(eta=str_remove(eta, "eta "))

stargazer(compare.lrsm, summary=FALSE, rownames=FALSE, 
          omit.table.layout="-!")


#===== LPCM
#==== eRm 
#group vector
G <- c(rep(1,10),rep(2,10)) 
erm.lpcm <- LPCM(lpcmdat, mpoints = 2, groupvec = G)
# design matrix is automatically generated
W <- erm.lpcm$W %>% 
  as.data.frame() %>% 
  mutate(item.cate.mp=rownames(.)) %>% 
  separate_wider_delim(item.cate.mp, delim=" ", names=c("item.cate","mp","gr")) %>% 
  separate_wider_delim(item.cate, delim=".", names=c("item","cate"))

#==== glmer
# mapping matrix for linear tree
linear_tree_map <- data.frame(node1=c(0,1,1,1), node2=c(NA,0,1,1), node3=c(NA,NA,0,1)) %>% 
  `rownames<-`(c(0,1,2,3)) %>% 
  as.matrix()
# add a column has original responses for linear tree map 
linear_tree_map_resp <- 
  rownames_to_column(as.data.frame(linear_tree_map), var="resp") %>% 
  mutate(resp=as.double(resp))

# TODO: edit for LPCM
# dendrify
lpcmdat_long <- lpcmdat %>%
  rownames_to_column(., var="person") %>%
  pivot_longer(cols=starts_with("I"),
               names_to="item.mp",
               values_to="resp") %>%
  separate_wider_delim(item.mp, delim=".",names=c("item","mp")) %>% 
  left_join(., linear_tree_map_resp, by=c("resp")) %>% 
  pivot_longer(cols=starts_with("node"),
               names_to="node",
               values_to="resp_node") %>% 
  arrange(person,mp,item,node) %>% 
  mutate(item=as.factor(item),
         node=as.factor(node),
         mp=as.factor(mp),
         node=str_replace(node, "node", "c")) %>% 
  rename(cate=node) %>% 
  # Join design matrix
  inner_join(., W, by=c("mp","item","cate")) %>% 
  select(person,mp,item,cate,starts_with("eta"),resp_node,resp)

# Fit the model using glmer
t<-proc.time()
glmer.lpcm <- glmer(resp_node ~ 
                      `eta 1`+`eta 2`+`eta 3`+`eta 4`+`eta 5` +
                      (1|person), 
                    data=lpcmdat_long, family=binomial,
                    control=glmerControl(optCtrl=list(maxfun=100000))
)
proc.time()-t #  3sec
#==== compare glmer with eRm 
summary(glmer.lpcm)$coefficients
glmer.lpcm@theta
erm.lpcm$etapar
erm.lpcm$betapar

glmer.lpcm.est.se <- summary(glmer.lpcm)$coefficients %>% 
  as.data.frame %>% 
  mutate(item=rownames(.)) %>% 
  mutate(item=str_replace(item, "item", "")) %>% 
  mutate(item=str_replace(item, "nodenode", "c")) %>% 
  mutate(item=str_replace(item, "mp","")) %>% 
  rename(glmer.est=Estimate,
         glmer.se="Std. Error") %>% 
  select(item, glmer.est, glmer.se) %>% 
  # not need to invert easiness to difficulty.
  mutate(glmer.est=glmer.est,
         item=str_replace_all(item,"`","")) %>% 
  filter(item!="(Intercept)")

erm.lpcm.est <- erm.lpcm$etapar %>% 
  as.data.frame() %>% 
  mutate(item=names(erm.lpcm$etapar)) %>% 
  rename(eRm.est=".")
erm.lpcm.se <- erm.lpcm$se.eta %>% 
  as.data.frame %>% 
  mutate(item=names(erm.lpcm$etapar)) %>% 
  rename(eRm.se=".")

erm.lpcm.est.se <- full_join(erm.lpcm.est,erm.lpcm.se, by="item")

compare.lpcm <- full_join(glmer.lpcm.est.se, erm.lpcm.est.se, by=c("item")) %>% 
  arrange(item) %>% 
  rename(eta=item) %>% 
  mutate(eta=str_remove(eta, "eta "))

stargazer(compare.lpcm, summary=FALSE, rownames=FALSE, 
          omit.table.layout="-!")





#====================================================
#====== below is depricated 
# Sample Size Determination for extended Rasch model.
# refer to https://lkumle.github.io/power_notebooks/Scenario3_notebook.html
#===== Libraries
library(tidyverse)
library(lme4)
library(eRm)
library(simr)
library(mixedpower)
library(irtrees)
library(apaTables)
library(parallel)
library(foreach)

#===== lme4
# View it in long form format
VerbAgg %>% glimpse()
# mapping matrix for linear tree
linear_tree_map <- data.frame(node1=c(0,1,1), node2=c(NA,0,1)) %>% 
  `rownames<-`(c("no","perhaps","yes")) %>% 
  as.matrix()
# S1,S2 are situations where someone else is to be blamed.
# S3,S4 are situations where one is self to be blamed.
# add a column has original responses for linear tree map 
linear_tree_map_resp <- 
  rownames_to_column(as.data.frame(linear_tree_map), var="resp")
# separate tables to dendrify
design_matrix <- VerbAgg %>% 
  distinct(item, btype, situ, mode) %>% 
  `rownames<-`(.$item) 
persons <- VerbAgg %>% 
  dplyr::distinct(id, Gender, Anger)
responses <- VerbAgg %>% 
  dplyr::distinct(id, item, resp) %>% 
  dplyr::left_join(., linear_tree_map_resp, by=c("resp")) %>% 
  tidyr::pivot_longer(cols=starts_with("node"),
                      names_to="node",
                      values_to="resp_node") %>% 
  dplyr::arrange(id,item) %>% 
  tidyr::drop_na()
# unite tables 
verbagg_dendrified <- design_matrix %>% 
  left_join(., responses, by=c("item")) %>% 
  arrange(item)
# separate by item
for(i in c("S1","S2","S3","S4")){
  assign(paste0(i,"_dendrified"),verbagg_dendrified %>% filter(str_detect(item,i)))
}


# separate by item
for(i in c("S1","S2","S3","S4")){
  assign(i,VerbAgg %>% filter(str_detect(item,i)))
}
for(i in c("S1","S2","S3","S4")){
  assign(
    paste0(i,"_wide2"),VerbAgg %>% filter(str_detect(item,i)) %>% 
      dplyr::mutate(r2=ifelse(r2=="Y",1,0)) %>% 
      tidyr::pivot_wider(id_cols=id, names_from=item, values_from=r2)
  )
}
for(i in c("S1","S2","S3","S4")){
  assign(
    paste0(i,"_wide3"),VerbAgg %>% filter(str_detect(item,i)) %>% 
      dplyr::mutate(resp=case_when(
        resp=="yes" ~ 2,
        resp=="perhaps" ~ 1,
        resp=="no" ~ 0
      )) %>% 
      tidyr::pivot_wider(id_cols=id, names_from=item, values_from=resp)
  )
}
# VerbAgg_wide3 <- dplyr::inner_join(S1_wide3, S2_wide3, by=c("id")) %>% 
#   dplyr::inner_join(., S3_wide3, by=c("id")) %>% 
#   dplyr::inner_join(., S4_wide3, by=c("id"))
for(i in c("S1","S2","S3","S4")){
  assign(paste0(i,"_design_num"),
         design_matrix %>% filter(str_detect(item,i)) %>% 
           fastDummies::dummy_cols(select_columns=c("btype","situ","mode"),
                                   remove_first_dummy=TRUE,
                                   remove_selected_columns=TRUE) %>% 
           dplyr::select(-item)
  )
}
design_matrix_num <- bind_rows(S1_design_num, S2_design_num, S3_design_num, S4_design_num)


#===== LLTM
# `-1` or `0` in formula avoids that the first item is used as the reference item and the basis for the intercept. 
# Thus, from c categories, formula with -1 encodes into c columns, while formula without -1 encodes into c-1 columns. 
# formulation for glmer is explained in 9.6 and 19.2, Crawley 2013
lltm <- glmer(formula= r2 ~ btype +situ + mode + (1|id),
              data=VerbAgg, family=binomial)
# result
(summary_lltm <- summary(lltm))
# Actual number of persons 
length(unique(VerbAgg$id))

# eRm for LLTM
erm_lltm <- LLTM(VerbAgg2[,-c(1:2)]-1, design_matrix_num)

# compare glmer with eRm for LLTM
cbind(summary_lltm$coefficients[-1,1:2], 
      t(rbind(erm_lltm$etapar,erm_lltm$se.eta)) %>% 
        `colnames<-`(c("Estimate", "Std. Error")))

#===== RSM
# VerbAgg3T <- irtrees::dendrify(irtrees::VerbAgg3[,-(1:2)], linear_tree_map)
# VerbAgg3T %>% is.list()
# VerbAgg3T %>% head
# VerbAgg3T %>% distinct(item,node)
# irtrees::dendrify is 

t <- proc.time()
rsm <- glmer(resp_node ~ item + node + (1|id),
                   data=verbagg_dendrified, family=binomial)
proc.time()-t # 154sec
(summary_rsm <- summary(rsm))
# eRm for RSM
erm_rsm <- RSM(VerbAgg3[,-c(1:2)]-1, sum0=TRUE)
erm_rsm

# compare glmer with eRm for RSM
summary_rsm$coefficients

cbind(
  summary_rsm$coefficients[order(rownames(summary_rsm$coefficients)),][-c(1,nrow(summary_rsm$coefficients)),1:2] 
  ,
  t(rbind(erm_rsm$etapar,erm_rsm$se.eta)) %>% 
    `colnames<-`(c("Estimate", "Std. Error")) %>% 
    .[order(rownames(.)),] %>% 
    .[-1,] 
)

#===== LRSM
t <- proc.time()
lrsm <- glmer(resp_node ~ btype + situ + mode + node + (1|id),
                    data=verbagg_dendrified, family=binomial)
proc.time()-t # 7sec
(summary_lrsm <- summary(lrsm))
# eRm for LRSM
design_matrix_lrsm <- slice(design_matrix_num, rep(1:n(), each=2)) %>% 
  mutate(across(everything(), ~ifelse(row_number(.)%%2==1,.,.*2))) %>% 
  mutate(omega=ifelse(row_number()%%2==1,0,1))
erm_lrsm <- LRSM(X=VerbAgg3[,-c(1:2)]-1, 
                      W=design_matrix_lrsm
)
# compare glmer with eRm for LLTM
cbind(summary_lrsm$coefficients[-1,1:2]
      ,      
      t(rbind(erm_lrsm$etapar,erm_lrsm$se.eta)) %>% 
        `colnames<-`(c("Estimate", "Std. Error"))
)

# #===== PCM
# t <- proc.time()
# pcm_ <- glmer(resp_node ~ 0 + (0+item):(0+node) + (1|id),
#                    data=verbagg_dendrified,
#                    family=binomial)
# proc.time()-t #700sec
# saveRDS(pcm, "pcm.rds")
# pcm <- readRDS("pcm.rds")
# (summary_pcm <- summary(pcm))
# # eRm for PCM
# erm_pcm <- PCM(X=VerbAgg3[,-c(1:2)], sum0=TRUE)
# # compare glmer with eRm for LLTM
# summary_pcm$coefficients[order(rownames(summary_pcm$coefficients)),][-c(1,nrow(summary_pcm$coefficients)),1:2] %>% 
#   rownames ->a; a
# t(rbind(erm_pcm$etapar,erm_pcm$se.eta)) %>% 
#   `colnames<-`(c("Estimate", "Std. Error")) %>% 
#   .[order(rownames(.)),] %>% 
#   rownames ->b; b
# data.frame(c(0,a),b)
# # glmer: s1docurse, s4wantshout in pcm do not exist.
# # eRm: s1wantcurse in erm_pcm does not exist.

#===== LPCM
# adjacent-category logit for each item independent on number of categories.
t <- proc.time()
lpcm <- glmer(resp_node ~ btype + situ + mode + item:node + (1|id),
                    data=verbagg_dendrified, family=binomial)
lpcm
proc.time()-t # 13sec
(summary_lpcm <- summary(lpcm))

# eRm for LPCM
num_item <- length(unique(design_matrix$item))
design_matrix_lpcm <- foreach(i=seq(num_item), .combine=rbind) %do% {
  dm_temp <- t(data.frame(c1=rep(0,num_item),c2=rep(0,num_item)))
  dm_temp[2,i] <- 1
  rownames(dm_temp) <- paste0(unique(design_matrix$item)[i],"_",rownames(dm_temp))
  return(dm_temp)
} %>% 
  `colnames<-`(as.character(unique(design_matrix$item))) %>% 
  bind_cols(
    slice(design_matrix_num, rep(1:n(), each=2)) %>% 
      mutate(across(everything(), ~ifelse(row_number(.)%%2==1,.,.*2))),
    .)
    
erm_lrsm <- LRSM(X=VerbAgg3[,-c(1:2)]-1, 
                      W=design_matrix_lpcm)
erm_lrsm
    
    
    
    
    
    
    #== Scenario: You don't have any data like VerbAgg.
    # Delete response and person id to make only item dataset
    item_property <- VerbAgg %>% 
      dplyr::select(item, btype, situ, mode) %>% 
      unique
    glimpse(item_property) #24 * 4
    length(unique(item_property$item)) #24
    
    # create artificial data for random effect
    person_id <- (1:30)
    # combine person id and item id
    artificial_data <- expand.grid(id=person_id, item=item_property$item) %>% 
      dplyr::left_join(., item_property, by="item")
    
    # set fixed effect (intercept, btype, mode, situ) without pre information
    fixed_effects <- lltm@beta
    # set random intercept variance
    random_variance <- list(lltm@theta^2)
    # create Glmer for regular LLTM
    artificial_glmer <- makeGlmer(formula= r2 ~ -1 + btype + mode + situ + (1|id), 
                                  family="binomial", fixef=fixed_effects, 
                                  VarCorr=random_variance, data=artificial_data)
    t <- proc.time()
    power <- mixedpower(model=artificial_glmer,
                        data=artificial_data,
                        fixed_effects=c("btype","mode","situ"),
                        simvar="id", steps=seq(from=50,to=300,by=50),
                        critical_value=2, 
                        # SESOI=fixed_effects*0.85,
                        n_sim=1000)
    # R provides two file formats of its own for storing data, .RDS and .RData. 
    # RDS files can store a single R object, and RData files can store multiple R objects.
    saveRDS(power, "lltm_params_from_actual.rds")
    proc.time()-t # 3708 sec 
    lltm_params_from_actual <- readRDS("lltm_params_from_actual.rds") %>% 
      dplyr::mutate(mode="actual")
    # summary(lltm_params_from_actual)
    # multiplotPower(lltm_params_from_actual)
    
    
    # create Glmer for regular LLTM with fixed_effect/2
    artificial_glmer <- makeGlmer(formula= r2 ~ -1 + btype + mode + situ + (1|id), 
                                  family="binomial", fixef=fixed_effects/2, 
                                  VarCorr=random_variance, data=artificial_data)
    t <- proc.time()
    power <- mixedpower(model=artificial_glmer,
                        data=artificial_data,
                        fixed_effects=c("btype","mode","situ"),
                        simvar="id", steps=seq(from=50,to=300,by=50),
                        critical_value=2, 
                        # SESOI=fixed_effects*0.85,
                        n_sim=1000)
    # R provides two file formats of its own for storing data, .RDS and .RData. 
    # RDS files can store a single R object, and RData files can store multiple R objects.
    saveRDS(power, "lltm_params_from_fixed_half.rds")
    proc.time()-t 
    lltm_params_from_fixed_half <- readRDS("lltm_params_from_fixed_half.rds") %>% 
      dplyr::mutate(mode="half_fixef")
    
    # create Glmer for regular LLTM with variance/2
    random_variance_half <- list(lltm@theta^2/2)
    artificial_glmer <- makeGlmer(formula= r2 ~ -1 + btype + mode + situ + (1|id), 
                                  family="binomial", fixef=fixed_effects, 
                                  VarCorr=random_variance_half, data=artificial_data)
    t <- proc.time()
    power <- mixedpower(model=artificial_glmer,
                        data=artificial_data,
                        fixed_effects=c("btype","mode","situ"),
                        simvar="id", steps=seq(from=50,to=300,by=50),
                        critical_value=2, 
                        # SESOI=fixed_effects*0.85,
                        n_sim=1000)
    # R provides two file formats of its own for storing data, .RDS and .RData. 
    # RDS files can store a single R object, and RData files can store multiple R objects.
    saveRDS(power, "lltm_params_from_var_half.rds")
    proc.time()-t 
    lltm_params_from_var_half <- readRDS("lltm_params_from_var_half.rds") %>% 
      dplyr::mutate(mode="half_var")
    
    lltm_params <- dplyr::bind_rows(lltm_params_from_actual, lltm_params_from_fixed_half, lltm_params_from_var_half)
    multiplotPower(lltm_params)
    