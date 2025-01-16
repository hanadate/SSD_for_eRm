#===== libraries
library(tidyverse)
library(mixedpower)
library(lme4)

#===== View result
power.rasch <- readRDS("power_rasch.rds") %>% 
  mutate(effect=as.integer(str_remove(effect,"itemI"))) %>% 
  arrange(effect)
mixedpower::multiplotPower(power.rasch)
