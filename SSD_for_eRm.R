# Sample Size Determination for extended Rasch model.
# refer to https://lkumle.github.io/power_notebooks/Scenario3_notebook.html
library(tidyverse)
library(lme4)
library(simr)
library(mixedpower)

# View VergAgg in wide form format
VerbAgg %>% 
  dplyr::select(-c(resp, btype, situ, mode, Anger, Gender)) %>% 
  tidyr::pivot_wider(names_from=item, values_from=r2)

# View it in long form format
head(VerbAgg)

# apply regular LLTM for actual data
# The `-1` or `0` avoids that the first item is used as the reference item and the basis for the intercept. 
lltm <- glmer(formula= r2 ~ -1 + btype + mode + situ + (1|id),
              data=VerbAgg, family=binomial)
lltm
lltm@beta
# Actual number of persons 
length(unique(VerbAgg$id))

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
