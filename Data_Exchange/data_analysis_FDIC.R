##################################################################################################################################

# this script performs an analysis on the 2019 unbanked NC population by race/ethnicity

##################################################################################################################################

library(srvyr)
library(tidyverse)
library(tidycensus)

# filter for total population - banked and unbanked
total_data <- hh %>%
  filter(hunbnk == 1 | hunbnk == 2)

# create survey object
fdic_srvdata_total <- as_survey_rep(total_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# totals by race/ethnicity for population
total_test <- fdic_srvdata_total %>%
  group_by(hryear4, gestfips, `Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  ungroup()

# filter out 'Other Race' to match outputs below for proportions
total_test <- total_test[c(1:2,4),]


##################################################################################################################################

# filter for unbanked
unbanked_data <- hh %>%
  filter(hunbnk == 1)

# create survey object
fdic_srvdata_unbanked <- as_survey_rep(unbanked_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# totals by race/ethnicity for unbanked population
unbanked_total_test <- fdic_srvdata_unbanked %>%
  group_by(hryear4, gestfips, `Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  ungroup()

# output proportions by race/ethnicity
output_unbanked_total_test <- unbanked_total_test %>%
  select(hryear4, gestfips, `Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / total_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, total_test$total, total_moe, 
                                                         total_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
  ) 

# formatting for shiny output
shiny_output_unbanked <- output_unbanked_total_test %>%
  mutate(year = hryear4,
         geo_description= gestfips,
         type = "Race Ethnicity",
         subtype = `Race Ethnicity`,
         estimate = prop,
         success = "",
         trials = "",
         moe = `Proportion MOE`
  ) %>%
  select(year, geo_description, type, subtype, estimate, success, trials, moe)

#writing the shiny data into csvs in designated locations for each measure variable 
write.csv(shiny_output_unbanked, file = "Data_Exchange/Shiny_Unbanked.csv")

# output totals by race/ethnicity
OR_unbanked_test <- unbanked_total_test %>%
  select(hryear4, gestfips, `Race Ethnicity`, total) %>%
  mutate(unbanked_total = total,
         banked_total = total_test$total - total
  )

test_unbanked <- OR_unbanked_test %>%
  select(`Race Ethnicity`,unbanked_total,banked_total)

test_unbanked_black <- test_unbanked %>%
  filter(`Race Ethnicity` != "Hispanic/Latino")

OR_unbanked_black <- (test_unbanked_black[1,2]*test_unbanked_black[2,3]) / (test_unbanked_black[2,2]*test_unbanked_black[1,3])
colnames(OR_unbanked_black) <- "Odds Ratio"

test_unbanked_hisp <- test %>%
  filter(`Race Ethnicity` != "Black/AA, NH")

OR_unbanked_hisp <- (test_unbanked_hisp[1,2]*test_unbanked_hisp[2,3]) / (test_unbanked_hisp[2,2]*test_unbanked_hisp[1,3])
colnames(OR_unbanked_hisp) <- "Odds Ratio"

unbanked_join_OR <- full_join(OR_unbanked_black, OR_unbanked_hisp, by = "Odds Ratio")
  
unbanked_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White,NH")
