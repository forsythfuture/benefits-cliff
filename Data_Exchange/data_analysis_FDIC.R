##################################################################################################################################

##################################################################################################################################

library(srvyr)
library(tidyverse)
library(tidycensus)

total_data <- hh %>%
  filter(hunbnk == 1 | hunbnk == 2)

fdic_srvdata_total <- as_survey_rep(total_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# total and median net worth using srvyr
total_test <- fdic_srvdata_total %>%
  group_by(`Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

boo <- fdic_srvdata_total %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

total_test <- total_test[c(1:2,4),]


##################################################################################################################################

unbanked_data <- hh %>%
  filter(hunbnk == 1)

fdic_srvdata_unbanked <- as_survey_rep(unbanked_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# total and median net worth using srvyr
unbanked_total_test <- fdic_srvdata_unbanked %>%
  group_by(`Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

lala <- fdic_srvdata_unbanked %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# output proportions by race/ethnicity
output_unbanked_total_test <- unbanked_total_test %>%
  select(`Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / total_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, total_test$total, total_moe, 
                                                         total_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
  ) %>%
  select(-total, -total_moe, -`Proportion MOE`, -prop)

##################################################################################################################################
