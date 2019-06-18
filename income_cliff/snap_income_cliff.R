#####################################################
#
# create cumulative distribution plot of incomes
# for snap recipients and all of Forsyth County
#
######################################################

library(tidyverse)

incomes <- read_csv("https://forsyth-futures.s3.amazonaws.com/benefits_income.csv.gz") %>%
  # filter for Forsyth County
  filter(COUNTYFIP == 67)

# recode weeks worked to be the high-point of the interval
weeks_worked <- c(`0` = 0,
                  `1` = 13,
                  `2` = 26,
                  `3` = 39,
                  `4` = 47,
                  `5` = 49,
                  `6` = 52)

income <- incomes %>%
  # recode weeks worked to the number of weeks
  mutate(WKSWORK2 = recode(WKSWORK2, !!! weeks_worked),
         # calculate estimated monthly income by first estimating 
         # weekly income by divide yearly income by weeks worked,
         # then multiply this amount by 4.35 (weeks per month)
         # to get an estimate of monthly income
         #monthly_income = INCEARN / 52,
         monthly_income = (INCEARN / WKSWORK2) * 4.32,
         # if someone is currently unemployed, set their monthly income to 0
         # this is because we are tyrying to estiamte their current monthly income
         monthly_income = ifelse(EMPSTAT %in% c(2, 3), 0, monthly_income),
         # if someone did not earn income all year, set their monthly income to 0
         monthly_income = ifelse(INCEARN == 0, 0, monthly_income),
         # create boolean of wehther person is 18 or under
         under_18 = ifelse(AGE <= 18, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  summarize(size = n(), # household size
            children = sum(under_18), # number of children in household under 18
            income = sum(monthly_income), # total household income
            food_stamps = max(FOODSTMP), # whether household receives food stamps
            wgt = mean(HHWT) # household weight
            ) %>% 
  ungroup() %>%
  select(-SERIAL) %>%
  # remove incomes under 0
  filter(income >= 0) %>%
  arrange(income) %>%
  # group by famil ysize and
  # create cumulative sum for incomes by using the weight column
  mutate(cum_income = cumsum(wgt))

# import federal poverty guidelines, calcualte monthly guidelines at
# 130%, since this is when SNAP ends
fpg <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  filter(year == 2017) %>%
  mutate(guidelines_month = round((guidelines_year / 12) * 1.3, 0)) %>%
  select(household_size, guidelines_month)

# merge guidelines with income dataset and create a boolean of whether
# income is above guidelines
# theoretically, such households should not receive SNAP
income <- income %>%
  left_join(fpg, by = c("size" = "household_size")) %>%
  mutate(over_snap_limit = ifelse(income > guidelines_month, TRUE, FALSE))

miss <- income %>%
  filter(food_stamps == 2,
         over_snap_limit == T)

wk <- incomes %>%
  filter(WKSWORK2 == 0,
         INCEARN > 1000)

snap <- incomes %>%
  filter(FOODSTMP == 2) %>%
  select(SERIAL, HHWT) %>% 
  distinct() %>%
  summarize(sum(HHWT))