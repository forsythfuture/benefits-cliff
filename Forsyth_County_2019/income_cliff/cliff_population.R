#####################################################
#
# create dataset for creating cumulative sum plot
# the dataset will show the number of people at each income amount
#
# the codebook for the dataset is in income_cliff/total_income_counts.pdf
#
######################################################

library(tidyverse)

incomes <- read_csv("https://forsyth-futures.s3.amazonaws.com/total_income_counts.csv.gz") %>%
  # filter for Forsyth County
  # list of NC county fips codes is here: https://www.lib.ncsu.edu/gis/countfips
  filter(COUNTYFIP == 67)

income <- incomes %>%
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
  # remove household incomes less than 7200, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # add column that is the same thing with all values, so that the nested d3 plot works
  # this column is irrelevant, but lets use recylce the d3 code from the otehr plots
  mutate(grouping = "group",
         # the y axis of the plot should reflect numebr of people, not number of households,
         # so, multiply cum_sum by size to convert number of households to number of people
         cum_sum = size * cum_sum) %>%
  ungroup() %>%
  select(size, income = HHINCOME, cum_sum, grouping)

write_csv(income, "plots/cliff_cdf.csv")
