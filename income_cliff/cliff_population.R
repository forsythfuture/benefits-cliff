#####################################################
#
# create dataset for creating cumulative distribution plot
#
######################################################

library(tidyverse)

incomes <- read_csv("https://forsyth-futures.s3.amazonaws.com/total_income_counts.csv.gz") %>%
  # filter for Forsyth County
  filter(COUNTYFIP == 67)

income <- incomes %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is 18 or under
  mutate(under_18 = ifelse(AGE <= 18, TRUE, FALSE),
         # create boolean of whether person is in school
         in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         children = sum(under_18), # number of children in household under 18
         total_school= sum(in_school), # number out of labor force
         adults = size - children # numebr of adults
         ) %>% 
  filter(size != total_school, # remove households where all people are in school
         adults <= 2, # less than or equal to 2 adults
         children <= 3 # less than or equal to three children
         ) %>%
  select(SERIAL, HHWT, HHINCOME, size, children, adults) %>%
  distinct() %>%
  # now group by size of household and number of children
  group_by(size, children) %>%
  arrange(size, children, HHINCOME) %>%
  # group by family size and
  # create cumulative sum for incomes by using the weight column
  mutate(cum_sum = cumsum(HHWT),
         perc_sum = round(percent_rank(cum_sum), 2)) %>%
  # only keep households where size does not equal children
  filter(size != children,
         # remove household incomes less than 7200, which is a monthly income of 6000
         # this is the amount we use for our other charts
         HHINCOME <= 72000) %>%
  # create single column signifying number of adults and children
  # so we can map correct composition text
  mutate(comp = str_c(adults, "-", children),
         # create string of family composition based on adults and children
         composition = recode(comp, `1-0` = "1 adult",
                                    `1-1` = "1 adult, 1 child",
                                    `1-2` = "1 adult, 2 children",
                                    `1-3` = "1 adult, 3 children",
                                    `2-0` = "2 adults",
                                    `2-1` = "2 adults, 1 child",
                                    `2-2` = "2 adults, 2 children",
                                    `2-3` = "2 adults, 3 children")) %>%
  ungroup() %>%
  select(composition, income = HHINCOME, cum_sum, perc_sum)

write_csv(income, "plots/cliff_cdf.csv")
