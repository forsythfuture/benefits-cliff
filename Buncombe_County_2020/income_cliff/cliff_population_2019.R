#############################################################################################################################################

# create dataset for creating plots to show individuals either below/above income level, 
# whether they are enrolled in FNS, and race/ethnicity breakouts
# the dataset will show the number of people at each income amount
#
# the codebook for the dataset is in Forsyth_County_2019/income_cliff/total_income_counts.pdf

#############################################################################################################################################

#load library
library(tidyverse)

# read in data from google drive
test <- read_csv("G://Shared drives/Forsyth Futures/Forsyth Futures Projects/JE_210001 Buncombe County Benefits Cliff Microsite/usa_00001.csv.gz")

# filter for 5 year 2017 ACS data by state and county --- matches Shane's data
# test_2017 <- test %>%
#   filter(STATEFIP == 37,
#          COUNTYFIP == 21,
#          YEAR == 2017,
#          MULTYEAR %in% 2013:2017)

# filter for 5 year 2019 ACS data by state and county
test_2019 <- test %>%
  filter(STATEFIP == 37,
         COUNTYFIP == 21,
         YEAR == 2019,
         MULTYEAR %in% 2015:2019)

#############################################################################################################################################

# testing for 2019

income_2019 <- test_2019 %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is in school
  # we'll later remove families where everyone is in school,
  # because such families would generally not be entitled to benefits
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, HHWT, HHINCOME, size) %>%
  distinct() %>%
  # now group by size and income
  # we're grouping by income because we will sum household weights by income
  # so they are aggregated and we don't have multiple rows of the same income
  group_by(size, HHINCOME) %>%
  summarize(HHWT = sum(HHWT)) %>%
  arrange(size, HHINCOME) %>%
  # create cumulative sum for incomes by using the weight column
  mutate(cum_sum = cumsum(HHWT),
         perc_sum = round(percent_rank(cum_sum), 2)) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # add column that is the same thing with all values, so that the nested d3 plot works
  # this column is irrelevant, but lets use recycle the d3 code from the other plots
  mutate(grouping = "group",
         # the y axis of the plot should reflect number of people, not number of households,
         # so, multiply cum_sum by size to convert number of households to number of people
         cum_sum = size * cum_sum) %>%
  ungroup() %>%
  select(size, income = HHINCOME, cum_sum, grouping)

write_csv(income, "Buncombe_County_2020/plots/cliff_cdf.csv")

#############################################################################################################################################

# 2019 Household size Buncombe County counts below/above income level

income_2019_HH <- test_2019 %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is in school
  # we'll later remove families where everyone is in school,
  # because such families would generally not be entitled to benefits
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, HHWT, HHINCOME, size) %>%
  distinct() %>%
  # now group by size and income
  # we're grouping by income because we will sum household weights by income
  # so they are aggregated and we don't have multiple rows of the same income
  group_by(size, HHINCOME) %>%
  summarize(HHWT = sum(HHWT)) %>%
  arrange(size, HHINCOME) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # the y axis of the plot should reflect number of people, not number of households,
  # so, multiply HHWT by size to convert number of households to number of people
  mutate(total = size * HHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above")) %>%
  ungroup() %>%
  select(size, HHINCOME, HHWT, total, FNS_threshold, indicator)

#totals by household size
totals_by_household_size <- income_2019_HH %>%
  group_by(size, indicator) %>%
  summarise(sum = sum(total))

#totals overall
totals_overall <- income_2019_HH %>%
  group_by(indicator) %>%
  summarise(sum = sum(total))

#############################################################################################################################################

# 2019 FNS Buncombe County counts

income_2019_fns <- test_2019 %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is in school
  # we'll later remove families where everyone is in school,
  # because such families would generally not be entitled to benefits
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, HHWT, HHINCOME, size, FOODSTMP) %>%
  distinct() %>%
  # now group by size and income
  # we're grouping by income because we will sum household weights by income
  # so they are aggregated and we don't have multiple rows of the same income
  group_by(size, HHINCOME, FOODSTMP) %>%
  summarize(HHWT = sum(HHWT)) %>%
  arrange(size, HHINCOME, FOODSTMP) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # the y axis of the plot should reflect number of people, not number of households,
  # so, multiply HHWT by size to convert number of households to number of people
  mutate(total = size * HHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above"),
         food_stamps = if_else(FOODSTMP == 2, "On Food Stamps", "Not on Food Stamps")) %>%
  ungroup() %>%
  select(size, food_stamps, HHINCOME, total, FNS_threshold, indicator)

#totals by food stamp status
totals_by_fns<- income_2019_fns %>%
  group_by(food_stamps) %>% 
  summarise(sum = sum(total))

#############################################################################################################################################

# 2019 race/ethnicity Buncombe County counts below/above income level

# boo <- testing_test3 %>%
#   filter(size == 3)
# 
# boo2 <- income_2019_test3 %>%
#   filter(size == 3)
# 
# sum(boo$officialHHWT)
# sum(boo2$HHWT)

income_2019_race <- test_2019 %>%
  #race/ethnicity recode
  mutate(race_ethnicity = if_else(between(HISPAN,1,4), "Hispanic/Latino", 
                                  if_else((HISPAN == 0 | HISPAN == 9) & RACE == 1, "White, NH", 
                                          if_else((HISPAN == 0 | HISPAN == 9) & RACE == 2, "Black/AA, NH", 
                                                  if_else((HISPAN == 0 | HISPAN == 9) & (RACE != 1 | RACE != 2), "Other Race", NA_character_))))) %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is in school
  # we'll later remove families where everyone is in school,
  # because such families would generally not be entitled to benefits
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, size, HHINCOME, HHWT, race_ethnicity)

# function to pull SERIAL #s mentioned 2+ times
myFun <- function(vector, thresh) {
  ind <- ave(rep(1, length(vector)), vector, FUN = length)
  vector[ind > thresh]
}

# vector of serial numbers mentioned at least 2+ times
multiple_serials <- myFun(test_2019$SERIAL, 1) %>% unique()

#pick up the recode
income_2019_race2 <- income_2019_race %>%
  #make a new HHWT variable that divides the HHWT by the size of the household to get the individual weight
  mutate(newHHWT = if_else(SERIAL %in% multiple_serials, HHWT / size, HHWT)) %>%
  group_by(size, HHINCOME, race_ethnicity) %>%
  # now group by size and income and race
  # we're grouping by income because we will sum household weights by income and race
  # so they are aggregated and we don't have multiple rows of the same income
  # sum newHHWT
  summarize(officialHHWT = sum(newHHWT)) %>%
  arrange(size, HHINCOME, race_ethnicity) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # the y axis of the plot should reflect number of people, not number of households,
  # so, multiply HHWT by size to convert number of households to number of people
  mutate(total = size * officialHHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above")) %>%
  ungroup() %>%
  select(size, race_ethnicity, HHINCOME, officialHHWT, total, FNS_threshold, indicator)
  
#totals by race/ethnicity
totals_by_race_ethnicity <- income_2019_race2 %>%
    group_by(race_ethnicity, indicator) %>%
    summarise(sum = sum(total))
  
  