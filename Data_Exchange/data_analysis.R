################################################################################################

# this script calculates the median net worth for HHs in NC, the proportion of HHs in NC with zero
# or negative net worth, calculates the asset poverty rate in NC HHs, and calculates the liquid
# asset poverty rate in NC HHs all with accompanying MOEs

# data_import.R needs to be ran first before running these analyses

################################################################################################

#load libraries
library(srvyr)
library(tidyverse)
library(tidycensus) #used prop_moe function

##################################################################

# Net Worth

# setting up data for those 15+ in NC
data_worth <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4))

# create survey object
srvdata <- as_survey_rep(data_worth, weights = WPFINWGT, repweights = REPWGT0:REPWGT240, type = "Fay", rho = 0.5,
                          combined.weights = TRUE)

# total and median net worth using srvyr
net_worth_test <- srvdata %>%
  group_by(SPANEL, TEHC_ST, `Race Ethnicity`) %>%
  summarise(total = survey_total(),
            median = survey_median(THNETWORTH)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  mutate(`Median MOE` = median_se * 1.96) %>%
  ungroup()

# formatting for shiny output
shiny_output_net_worth <- net_worth_test %>%
  mutate(year = SPANEL,
         geo_description= TEHC_ST,
         type = "Race Ethnicity",
         subtype = `Race Ethnicity`,
         estimate = median,
         success = "",
         trials = "",
         moe = `Median MOE`
         ) %>%
  select(year, geo_description, type, subtype, estimate, success, trials, moe)

#writing the shiny data into csvs in designated locations for each measure variable 
write.csv(shiny_output_net_worth, file = "Data_Exchange/Shiny_Net_Worth.csv")

# # output median by race/ethnicity
# output_net_worth <- net_worth_test %>%
#   select(`Race Ethnicity`, median, `Median MOE`) %>%
#   mutate(median = paste0("$", format(round(median), big.mark =",")),
#          `Median MOE` = paste0("$", format(round(`Median MOE`), big.mark =",")),
#          Median = paste0(median, " +/- ", `Median MOE`)
#          ) %>%
#   select(-median, -`Median MOE`)

##################################################################

# Housholds with Zero Net Worth

# data set with HHs w/ zero or neg net worth
data_zero <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  filter(THNETWORTH <= 0)

# create survey object
srvdata_zero <- as_survey_rep(data_zero, weights = WPFINWGT, repweights = REPWGT0:REPWGT240, type = "Fay", rho = 0.5,
                         combined.weights = TRUE)

# total net worth of zero or neg
net_worth_zero <- srvdata_zero %>%
  group_by(SPANEL, TEHC_ST, `Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  ungroup()

# output proportions by race/ethnicity
output_net_worth_zero <- net_worth_zero %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / net_worth_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, net_worth_test$total, total_moe, 
                                                         net_worth_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
         )

# formatting for shiny output
shiny_output_worth_zero <- output_net_worth_zero %>%
  mutate(year = SPANEL,
         geo_description= TEHC_ST,
         type = "Race Ethnicity",
         subtype = `Race Ethnicity`,
         estimate = prop,
         success = "",
         trials = "",
         moe = `Proportion MOE`
  ) %>%
  select(year, geo_description, type, subtype, estimate, success, trials, moe)

#writing the shiny data into csvs in designated locations for each measure variable
write.csv(shiny_output_worth_zero, file = "Data_Exchange/Shiny_Worth_Zero.csv")

# output totals by race/ethnicity
OR_net_worth_zero_test <- net_worth_zero %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total) %>%
  mutate(worth_zero_total = total,
         worth_nonzero_total = net_worth_test$total - total
  )

test_net_worth_zero <- OR_net_worth_zero_test %>%
  select(`Race Ethnicity`,worth_zero_total,worth_nonzero_total)

test_net_worth_zero_black <- test_net_worth_zero %>%
  filter(`Race Ethnicity` != "Hispanic/Latino")

OR_net_worth_zero_black <- (test_net_worth_zero_black[1,2]*test_net_worth_zero_black[2,3]) / (test_net_worth_zero_black[2,2]*test_net_worth_zero_black[1,3])
colnames(OR_net_worth_zero_black) <- "Odds Ratio"

test_net_worth_zero_hisp <- test_net_worth_zero %>%
  filter(`Race Ethnicity` != "Black/AA, NH")

OR_net_worth_zero_hisp <- (test_net_worth_zero_hisp[1,2]*test_net_worth_zero_hisp[2,3]) / (test_net_worth_zero_hisp[2,2]*test_net_worth_zero_hisp[1,3])
colnames(OR_net_worth_zero_hisp) <- "Odds Ratio"

net_worth_zero_join_OR <- full_join(OR_net_worth_zero_black, OR_net_worth_zero_hisp, by = "Odds Ratio")

net_worth_zero_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White, NH")

##################################################################

# Asset Poverty Rate Calculation

# 2020 POVERTY GUIDELINES FOR THE 48 CONTIGUOUS STATES AND THE DISTRICT OF COLUMBIA
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2020-poverty-guidelines
HH_poverty <- data.frame(RHNUMPER = seq(1,8),
                         income = (c(12760,17240,21720,26200,30680,35160,39640,44120)/4))

# setting up data for those 15+ in NC
data_asset <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4))

# combine data and thresholds
combine_dfs <- merge(data_asset, HH_poverty, by = "RHNUMPER")

# 1 for those whose liquid income is below the threshold
combined_data_asset <- combine_dfs %>%
  mutate(asset_pov = if_else(THNETWORTH <= income, 1, 0))

# create survey object
srvdata_asset <- as_survey_rep(combined_data_asset, weights = WPFINWGT, repweights = REPWGT0:REPWGT240, type = "Fay", rho = 0.5,
                              combined.weights = TRUE)

# total net worth of zero or neg
asset_poverty <- srvdata_asset %>%
  filter(asset_pov == 1) %>%
  group_by(SPANEL, TEHC_ST, `Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  ungroup()

# output proportions by race/ethnicity
output_asset_poverty <- asset_poverty %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / net_worth_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, net_worth_test$total, total_moe, 
                                                         net_worth_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
  )

# formatting for shiny output
shiny_output_asset_poverty <- output_asset_poverty %>%
  mutate(year = SPANEL,
         geo_description= TEHC_ST,
         type = "Race Ethnicity",
         subtype = `Race Ethnicity`,
         estimate = prop,
         success = "",
         trials = "",
         moe = `Proportion MOE`
  ) %>%
  select(year, geo_description, type, subtype, estimate, success, trials, moe)

#writing the shiny data into csvs in designated locations for each measure variable
write.csv(shiny_output_asset_poverty, file = "Data_Exchange/Shiny_Asset_Poverty.csv")

# output totals by race/ethnicity
OR_asset_poverty_test <- asset_poverty %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total) %>%
  mutate(asset_total = total,
         nonasset_total = net_worth_test$total - total
  )

test_asset_poverty <- OR_asset_poverty_test %>%
  select(`Race Ethnicity`,asset_total,nonasset_total)

test_asset_poverty_black <- test_asset_poverty %>%
  filter(`Race Ethnicity` != "Hispanic/Latino")

OR_asset_poverty_black <- (test_asset_poverty_black[1,2]*test_asset_poverty_black[2,3]) / (test_asset_poverty_black[2,2]*test_asset_poverty_black[1,3])
colnames(OR_asset_poverty_black) <- "Odds Ratio"

test_asset_poverty_hisp <- test_asset_poverty %>%
  filter(`Race Ethnicity` != "Black/AA, NH")

OR_asset_poverty_hisp <- (test_asset_poverty_hisp[1,2]*test_asset_poverty_hisp[2,3]) / (test_asset_poverty_hisp[2,2]*test_asset_poverty_hisp[1,3])
colnames(OR_asset_poverty_hisp) <- "Odds Ratio"

asset_poverty_join_OR <- full_join(OR_asset_poverty_black, OR_asset_poverty_hisp, by = "Odds Ratio")

asset_poverty_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White, NH")

##################################################################

# Liquid Asset Poverty Rate

# setting up data for those 15+ in NC
data_liquid <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  rowwise() %>%
  mutate(liquid_income = sum(THINC_BANK, THVAL_BANK, THINC_BOND, THINC_STMF, TTHR401VAL, TIRAKEOVAL, na.rm = TRUE))

# combine data and thresholds
combine_liquid_dfs <- merge(data_liquid, HH_poverty, by = "RHNUMPER")

# 1 for those whose liquid income is below the threshold
combined_data_liquid <- combine_liquid_dfs %>%
  mutate(liquid_pov = if_else(liquid_income <= income, 1, 0))

# create survey object
srvdata_liquid <- as_survey_rep(combined_data_liquid, weights = WPFINWGT, repweights = REPWGT0:REPWGT240, type = "Fay", rho = 0.5,
                               combined.weights = TRUE)

# total net worth of zero or neg
liquid_poverty <- srvdata_liquid %>%
  filter(liquid_pov == 1) %>%
  group_by(SPANEL, TEHC_ST, `Race Ethnicity`) %>%
  summarise(total = survey_total(liquid_pov)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  ungroup()

# output proportions by race/ethnicity
output_liquid_poverty <- liquid_poverty %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / net_worth_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, net_worth_test$total, total_moe, 
                                                         net_worth_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
  )

# formatting for shiny output
shiny_output_liquid_poverty <- output_liquid_poverty %>%
  mutate(year = SPANEL,
         geo_description= TEHC_ST,
         type = "Race Ethnicity",
         subtype = `Race Ethnicity`,
         estimate = prop,
         success = "",
         trials = "",
         moe = `Proportion MOE`
  ) %>%
  select(year, geo_description, type, subtype, estimate, success, trials, moe)

#writing the shiny data into csvs in designated locations for each measure variable
write.csv(shiny_output_liquid_poverty, file = "Data_Exchange/Shiny_Liquid_Poverty.csv")

# output totals by race/ethnicity
OR_liquid_poverty_test <- liquid_poverty %>%
  select(SPANEL, TEHC_ST, `Race Ethnicity`, total) %>%
  mutate(liquid_total = total,
         illiquid_total = net_worth_test$total - total
  )

test_liquid_poverty <- OR_liquid_poverty_test %>%
  select(`Race Ethnicity`,liquid_total,illiquid_total)

test_liquid_poverty_black <- test_liquid_poverty %>%
  filter(`Race Ethnicity` != "Hispanic/Latino")

OR_liquid_poverty_black <- (test_liquid_poverty_black[1,2]*test_liquid_poverty_black[2,3]) / (test_liquid_poverty_black[2,2]*test_liquid_poverty_black[1,3])
colnames(OR_liquid_poverty_black) <- "Odds Ratio"

test_liquid_poverty_hisp <- test_liquid_poverty %>%
  filter(`Race Ethnicity` != "Black/AA, NH")

OR_liquid_poverty_hisp <- (test_liquid_poverty_hisp[1,2]*test_liquid_poverty_hisp[2,3]) / (test_liquid_poverty_hisp[2,2]*test_liquid_poverty_hisp[1,3])
colnames(OR_liquid_poverty_hisp) <- "Odds Ratio"

liquid_poverty_join_OR <- full_join(OR_liquid_poverty_black, OR_liquid_poverty_hisp, by = "Odds Ratio")

liquid_poverty_join_OR$`Race / Ethnicity` <- c("Black/AA, NH : White, NH", "Hispanic/Latino : White, NH")

#testing 
# test_liquid <- combined_data_liquid %>% select(SSUID,PNUM,RHNUMPER,liquid_income,income,liquid_pov)
# View(test_liquid)
