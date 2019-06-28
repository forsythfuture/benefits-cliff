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

############################################################################################

# unused code that shows number of people by household with children instead of household size

# income <- incomes %>%
#   # remove household incomes less than 0
#   # we are interested in low-income / low net-worth people, and such people are unlikely
#   # to have negative income
#   filter(HHINCOME >= 0) %>%
#   # create boolean of whether person is 18 or under
#   mutate(under_18 = ifelse(AGE <= 18, TRUE, FALSE),
#          # create boolean of whether person is in school
#          in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
#   # group by household
#   group_by(SERIAL) %>%
#   mutate(size = n(), # household size
#          children = sum(under_18), # number of children in household under 18
#          total_school= sum(in_school), # number out of labor force
#          adults = size - children # numebr of adults
#          ) %>%
#   filter(size != total_school, # remove households where all people are in school
#          adults <= 2, # less than or equal to 2 adults
#          children <= 3 # less than or equal to three children
#          ) %>%
#   select(SERIAL, HHWT, HHINCOME, size, children, adults) %>%
#   distinct() %>%
#   # now group by adults, number of children, and income
#   # we're grouping by income because we will sum household weights by income
#   # so they are aggregated and we don't have multiple rows of the same icome
#   group_by(size, adults, children, HHINCOME) %>%
#   summarize(HHWT = sum(HHWT)) %>%
#   arrange(adults, children, HHINCOME) %>%
#   # group by family size and
#   # create cumulative sum for incomes by using the weight column
#   mutate(cum_sum = cumsum(HHWT),
#          perc_sum = round(percent_rank(cum_sum), 2)) %>%
#   # only keep households where size does not equal children
#   filter(size != children,
#          # remove household incomes less than 7200, which is a monthly income of 6000
#          # this is the amount we use for our other charts
#          HHINCOME <= 72000) %>%
#   # create single column signifying number of adults and children
#   # so we can map correct composition text
#   mutate(comp = str_c(adults, "-", children),
#          # create string of family composition based on adults and children
#          composition = recode(comp, `1-0` = "1 adult",
#                                     `1-1` = "1 adult, 1 child",
#                                     `1-2` = "1 adult, 2 children",
#                                     `1-3` = "1 adult, 3 children",
#                                     `2-0` = "2 adults",
#                                     `2-1` = "2 adults, 1 child",
#                                     `2-2` = "2 adults, 2 children",
#                                     `2-3` = "2 adults, 3 children"),
#          # add column that is the same thing so that the nested d3 plot works
#          grouping = "group") %>%
#   ungroup() %>%
#   select(composition, income = HHINCOME, cum_sum, perc_sum)
