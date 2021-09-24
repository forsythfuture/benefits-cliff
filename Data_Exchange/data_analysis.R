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
  summarise(total = survey_total(),
            Median = survey_median(THNETWORTH)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  mutate(`Median MOE` = Median_se * 1.96)

# combine median and median MOE
median_net_worth <- paste0("$", format(round(net_worth_test$Median), big.mark =","), " +/- $", 
                           format(round(net_worth_test$`Median MOE`), big.mark =","))
  
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
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# calculate proportion
total_zero_prop <- paste0(format(round(net_worth_zero$total / net_worth_test$total * 100, 2), nsmall = 1), "%")

# calculate proportion MOE
total_zero_prop_moe <- paste0(format(round(moe_prop(net_worth_zero$total, net_worth_test$total, net_worth_zero$total_moe,
                                                   net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")

# combine proportion and proportion MOE
total_zero <- paste0(total_zero_prop, " +/- ", total_zero_prop_moe)

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
  summarise(total = survey_total(asset_pov)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# calculate proportion
asset_pov_prop <- paste0(format(round(asset_poverty$total / net_worth_test$total * 100, 1), nsmall = 1), "%")

# calculate proportion MOE
asset_pov_prop_moe <- paste0(format(round(moe_prop(asset_poverty$total, net_worth_test$total, asset_poverty$total_moe,
                                                   net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")

# combine proportion and proportion MOE
asset_pov <- paste0(asset_pov_prop, " +/- ", asset_pov_prop_moe)

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
  summarise(total = survey_total(liquid_pov)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# calculate proportion
liquid_asset_pov_prop <- paste0(format(round(liquid_poverty$total / net_worth_test$total * 100, 1), nsmall = 1), "%")

# calculate proportion MOE
liquid_asset_pov_prop_moe <- paste0(format(round(moe_prop(liquid_poverty$total, net_worth_test$total, liquid_poverty$total_moe,
                                                           net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")

# combine proportion and proportion MOE
liquid_asset_pov <- paste0(liquid_asset_pov_prop, " +/- ", liquid_asset_pov_prop_moe)


#testing 
# test_liquid <- combined_data_liquid %>% select(SSUID,PNUM,RHNUMPER,liquid_income,income,liquid_pov)
# View(test_liquid)
