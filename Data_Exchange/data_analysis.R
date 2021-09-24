################################################################################################


################################################################################################

# # this script calculates the median net worth of HHs in NC
# net_worth <- data %>%
#   filter(TAGE_EHC >= 15) %>%
#   filter(TEHC_ST == 37) %>%
#   filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
#   summarise(`Median Net Worth` = median(THNETWORTH))
# 
# # format count
# net_worth <- format(net_worth, big.mark = ",")
# 
# # obtain the total HH count in NC
# total_HH <- data %>%
#   filter(TAGE_EHC >= 15) %>%
#   filter(TEHC_ST == 37) %>%
#   filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
#   summarize(count_total = n()) 
# 
# # obtian the total HH count in NC with zero or negative net worth               
# total_zero <- data %>%
#   filter(TAGE_EHC >= 15) %>%
#   filter(TEHC_ST == 37) %>%
#   filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
#   filter(THNETWORTH <= 0) %>%
#   summarize(count_zero = n())
# 
# # calculate proportion
# zero_worth <- paste0(round(total_zero / total_HH * 100, 2), "%")

##################################################################

library(srvyr)
library(tidyverse)

# Net Worth

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
            median = survey_median(THNETWORTH)) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96) %>%
  mutate(median_moe = median_se * 1.96)
  
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

# calculate proportion of nc pop with zero or neg net worth
total_zero_prop <- paste0(format(round(net_worth_zero$total / net_worth_test$total * 100, 2), nsmall = 1), "%")

total_zero_prop_moe <- paste0(format(round(moe_prop(net_worth_zero$total, net_worth_test$total, net_worth_zero$total_moe,
                                                   net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")

##################################################################

# Asset Poverty Rate Calculation

HH_poverty <- data.frame(RHNUMPER = seq(1,8),
                         income = (c(12760,17240,21720,26200,30680,35160,39640,44120)/4))

# data set with HHs w/ net worth under thresholds
data_asset <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4))

combine_dfs <- merge(data_asset, HH_poverty, by = "RHNUMPER")

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

asset_pov_prop <- paste0(format(round(asset_poverty$total / net_worth_test$total * 100, 1), nsmall = 1), "%")

asset_pov_prop_moe <- paste0(format(round(moe_prop(asset_poverty$total, net_worth_test$total, asset_poverty$total_moe,
                                                   net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")

##################################################################

# Liquid Asset Poverty Rate

data_liquid <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  rowwise() %>%
  mutate(liquid_income = sum(THINC_BANK, THVAL_BANK, THINC_BOND, THINC_STMF, TTHR401VAL, TIRAKEOVAL, na.rm = TRUE))

combine_liquid_dfs <- merge(data_liquid, HH_poverty, by = "RHNUMPER")

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

liquid_asset_pov_prop <- paste0(format(round(liquid_poverty$total / net_worth_test$total * 100, 1), nsmall = 1), "%")

liquid_asset_pov_prop_moe <- paste0(format(round(moe_prop(liquid_poverty$total, net_worth_test$total, liquid_poverty$total_moe,
                                                           net_worth_test$total_moe) * 100, 1), nsmall = 1), "%")
