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
  mutate(prop = total / total_test$total,
         `Proportion MOE` = moe_prop(total, total_test$total, total_moe, 
                                                         total_test$total_moe)
  )

#calculate relative risk
relative_risk_unbank_black <- output_unbanked_total_test[1,6]/output_unbanked_total_test[3,6]
relative_risk_unbank_hisp <- output_unbanked_total_test[2,6]/output_unbanked_total_test[3,6]

#full join relative risk
unbanked_join_OR <- full_join(relative_risk_unbank_black, relative_risk_unbank_hisp, by = "prop")
#update column name
colnames(unbanked_join_OR) <- "Relative Risk"

#add race column
unbanked_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White, NH")

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

# # output totals by race/ethnicity
# OR_unbanked_test <- unbanked_total_test %>%
#   select(hryear4, gestfips, `Race Ethnicity`, total) %>%
#   mutate(unbanked_total = total,
#          banked_total = total_test$total - total
#   )
# 
# test_unbanked <- OR_unbanked_test %>%
#   select(`Race Ethnicity`,unbanked_total,banked_total)
# 
# test_unbanked_black <- test_unbanked %>%
#   filter(`Race Ethnicity` != "Hispanic/Latino")
# 
# OR_unbanked_black <- (test_unbanked_black[1,2]*test_unbanked_black[2,3]) / (test_unbanked_black[2,2]*test_unbanked_black[1,3])
# colnames(OR_unbanked_black) <- "Odds Ratio"
# 
# test_unbanked_hisp <- test_unbanked %>%
#   filter(`Race Ethnicity` != "Black/AA, NH")
# 
# OR_unbanked_hisp <- (test_unbanked_hisp[1,2]*test_unbanked_hisp[2,3]) / (test_unbanked_hisp[2,2]*test_unbanked_hisp[1,3])
# colnames(OR_unbanked_hisp) <- "Odds Ratio"
# 
# unbanked_join_OR <- full_join(OR_unbanked_black, OR_unbanked_hisp, by = "Odds Ratio")
#   
# unbanked_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White, NH")

##################################################################################################################################

#row bind all
row_bind <- rbind(net_worth_join_OR, net_worth_zero_join_OR,asset_poverty_join_OR,liquid_poverty_join_OR,unbanked_join_OR)

#add a column for each metric
row_bind$V1 <- c("Median Net Worth", "Median Net Worth", "Zero Net Worth", "Zero Net Worth", "Asset Poverty", "Asset Poverty",
                 "Liquid Poverty", "Liquid Poverty", "Unbanked", "Unbanked")

#pivot wider
row_bind_pivot <- row_bind %>% pivot_wider(names_from = "Race / Ethnicity", values_from = "Relative Risk")
