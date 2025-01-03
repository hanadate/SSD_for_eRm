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

# create the table
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
  arrange(item) %>% 
  mutate(gap=abs(eRm.est-glmer.est))
stargazer(compare.rasch, summary=FALSE, rownames=FALSE, 
          align=TRUE,
          label="reprasch", 
          title="Replication \\texttt{eRm::RM} by \\texttt{lme4::glmer}")
# Create the plot
compare.rasch.long <- bind_rows(mutate(erm.rasch.est.se,group="eRm") %>% 
            select(item,eRm.est,eRm.se,group) %>% 
            rename(est=eRm.est, se=eRm.se)
          , mutate(glmer.rasch.est.se,group="glmer") %>% 
            select(item,glmer.est,glmer.se,group) %>% 
            rename(est=glmer.est, se=glmer.se)) %>% 
  filter(item!="(Intercept)") %>% 
  mutate(order=as.integer(str_remove(item,"I")),
         item=as.character(str_remove(item,"I"))) 
ggplot(compare.rasch.long, aes(x = reorder(item,order), y = est, fill = group)) + 
  geom_col(position = position_dodge(0.7), width = 0.5) +
  theme_classic() +
  scale_fill_grey(start=.5,end=.8) +
  geom_errorbar(aes(ymin=est-se, ymax=est+se),
                  data=compare.rasch.long, 
                  position=position_dodge(0.7), width = 0.1)+
  xlab("item")
ggsave("reprasch.png",width=7,height=2)


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
  mutate(glmer.est=glmer.est,
         item=str_remove_all(item, "`"))

#create the table
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
  mutate(item=as.integer(str_remove(item,"eta "))) %>%
  arrange(item) %>% 
  rename(eta=item) %>% 
  mutate(gap=abs(eRm.est-glmer.est))

stargazer(compare.lltm, summary=FALSE, rownames=FALSE, 
          align=TRUE,
          label="replltm", 
          title="Replication \\texttt{eRm::LLTM} by \\texttt{lme4::glmer}")
# Create the plot
compare.lltm.long <- bind_rows(mutate(erm.lltm.est.se,group="eRm") %>% 
                                  select(item,eRm.est,eRm.se,group) %>% 
                                  rename(est=eRm.est, se=eRm.se)
                                , mutate(glmer.lltm.est.se,group="glmer") %>% 
                                  select(item,glmer.est,glmer.se,group) %>% 
                                  rename(est=glmer.est, se=glmer.se)) %>% 
  filter(item!="(Intercept)") %>% 
  mutate(order=as.integer(str_remove(item,"eta ")),
         item=as.character(str_remove(item,"eta "))) %>% 
  rename(eta=item)
ggplot(compare.lltm.long, aes(x = reorder(eta,order), y = est, fill = group)) + 
  geom_col(position = position_dodge(0.7), width = 0.5) +
  theme_classic() +
  scale_fill_grey(start=.5,end=.8) +
  geom_errorbar(aes(ymin=est-se, ymax=est+se),
                data=compare.lltm.long, 
                position=position_dodge(0.7), width = 0.1)+
  xlab("eta")
ggsave("replltm.png",width=7,height=2)


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
  mutate(eta=str_remove(eta, "eta ")) %>% 
  mutate(gap=abs(eRm.est-glmer.est))

stargazer(compare.lrsm, summary=FALSE, rownames=FALSE, 
          align=TRUE,
          label="replrsm", 
          title="Replication \\texttt{eRm::LRSM} by \\texttt{lme4::glmer}")
# Create the plot
compare.lrsm.long <- bind_rows(mutate(erm.lrsm.est.se,group="eRm") %>% 
                                 select(item,eRm.est,eRm.se,group) %>% 
                                 rename(est=eRm.est, se=eRm.se)
                               , mutate(glmer.lrsm.est.se,group="glmer") %>% 
                                 select(item,glmer.est,glmer.se,group) %>% 
                                 rename(est=glmer.est, se=glmer.se)) %>% 
  filter(item!="(Intercept)") %>% 
  mutate(order=as.integer(str_remove(item,"eta ")),
         item=as.character(str_remove(item,"eta "))) %>% 
  rename(eta=item)
ggplot(compare.lrsm.long, aes(x = reorder(eta,order), y = est, fill = group)) + 
  geom_col(position = position_dodge(0.7), width = 0.5) +
  theme_classic() +
  scale_fill_grey(start=.5,end=.8) +
  geom_errorbar(aes(ymin=est-se, ymax=est+se),
                data=compare.lrsm.long, 
                position=position_dodge(0.7), width = 0.1)+
  xlab("eta")
ggsave("replrsm.png",width=7,height=2)

#===== LPCM
#==== eRm 
#group vector
G <- c(rep(1,10),rep(2,10)) 
erm.lpcm <- LPCM(lpcmdat, mpoints = 2, groupvec = G, sum0=FALSE)
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

# dendrify
lpcmdat_long <- lpcmdat %>%
  mutate(gr=str_c("g",G)) %>%
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
  inner_join(., W, by=c("gr","mp","item","cate")) %>% 
  select(person,gr,mp,item,cate,starts_with("eta"),resp_node,resp)

# Fit the model using glmer
glmer.lpcm <- glmer(resp_node ~ 
                      `eta 1`+`eta 2`+`eta 3`+`eta 4`+`eta 5` +
                      `eta 6`+`eta 7`+`eta 8`+`eta 9`+`eta 10` +
                      (1|person), 
                    data=lpcmdat_long, family=binomial,
                    control=glmerControl(optCtrl=list(maxfun=100000))
)
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
  rename(eta=item) %>% 
  mutate(eta=as.integer(str_remove(eta, "eta "))) %>% 
  arrange(eta) %>% 
  mutate(gap=abs(eRm.est-glmer.est))

stargazer(compare.lpcm, summary=FALSE, rownames=FALSE, 
          align=TRUE,
          label="replpcm", 
          title="Replication \\texttt{eRm::LPCM} by \\texttt{lme4::glmer}")
# Create the plot
compare.lpcm.long <- bind_rows(mutate(erm.lpcm.est.se,group="eRm") %>% 
                                 select(item,eRm.est,eRm.se,group) %>% 
                                 rename(est=eRm.est, se=eRm.se)
                               , mutate(glmer.lpcm.est.se,group="glmer") %>% 
                                 select(item,glmer.est,glmer.se,group) %>% 
                                 rename(est=glmer.est, se=glmer.se)) %>% 
  filter(item!="(Intercept)") %>% 
  mutate(order=as.integer(str_remove(item,"eta ")),
         item=as.character(str_remove(item,"eta "))) %>% 
  rename(eta=item)
ggplot(compare.lpcm.long, aes(x = reorder(eta,order), y = est, fill = group)) + 
  geom_col(position = position_dodge(0.7), width = 0.5) +
  theme_classic() +
  scale_fill_grey(start=.5,end=.8) +
  geom_errorbar(aes(ymin=est-se, ymax=est+se),
                data=compare.lpcm.long, 
                position=position_dodge(0.7), width = 0.1) +
  xlab("eta")
ggsave("replpcm.png",width=7,height=2)
