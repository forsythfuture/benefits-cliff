##################################################################################################################################

##################################################################################################################################

library(survey)
library(tidyverse)
library(tidycensus)

# example stats
# svyby(~hunbnk,~hryear4,hh_svy,svymean)

total <- svyby(~identifier, ~hryear4, hh_svy, svytotal)

total <- total %>%
  mutate(moe = se * 1.96)

unbanked_total <- svyby(~hunbnk, ~hryear4, hh_svy, svytotal)

unbanked_total <- unbanked_total %>%
  mutate(moe = se * 1.96)

