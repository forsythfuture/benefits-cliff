################################################################################################


################################################################################################

# this script calculates the median net worth of HHs in NC
net_worth <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  summarise(`Median Net Worth` = median(THNETWORTH))

# format count
net_worth <- format(net_worth, big.mark = ",")

# obtain the total HH count in NC
total_HH <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  summarize(count_total = n()) 

# obtian the total HH count in NC with zero or negative net worth               
total_zero <- data %>%
  filter(TAGE_EHC >= 15) %>%
  filter(TEHC_ST == 37) %>%
  filter(THHLDSTATUS %in% c(1,2,3,4)) %>%
  filter(THNETWORTH <= 0) %>%
  summarize(count_zero = n())

# calculate proportion
zero_worth <- paste0(round(total_zero / total_HH * 100, 2), "%")

##################################################################

library(srvyr)
library(tidyverse)

#HHs 
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
  mutate(total_moe = total_se * 1.96)

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
total_zero <- paste0(round(net_worth_zero$total / net_worth_test$total * 100, 2), "%")


HH_poverty <- data.frame(persons_HH = rep(1,8),
                         pov_guideline = c())


