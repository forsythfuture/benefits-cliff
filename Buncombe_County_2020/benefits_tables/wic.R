###############################################################################
#
# This script calculates WIC benefits
#
###############################################################################

library(tidyverse)

wic <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

# average benefit per person in 2021 was 28.08
# https://www.fns.usda.gov/pd/wic-program go to Monthly Data – State Level Participation by Category and Program Costs: FY 2021 (preliminary)
# assume recipients get this amount to get benefits, the children must be under 5 we'll assume that all 
# children are under 5 except for one child in the 3 child house the mother also gets benefits, and we'll 
# assume that in one adult households the adult is the mother given this, the amount of the benefit is 
# the number of children plus one, times 28.08, minus 28.08 if there are three children
wic <- wic %>%
  # only get WIC payment if family has kids
  mutate(payment = ifelse(children > 0, (children + 1) * 28.08, 0),
         payment = ifelse(children == 3, payment - 28.08, payment))

# can receive wic up to 185% of fpl
fpl <- read_rds('Buncombe_County_2020/benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # multiply guideline amount by 1.85 so it is at 185%
  mutate(guidelines_month = guidelines_month * 1.85) %>%
  # only keep 2018
  filter(year == 2021) %>%
  select(size = household_size, guidelines_month)

# add 185% poverty limit to WIC data set
wic <- wic %>%
  left_join(fpl, by='size') %>%
  # set payment to 0 if income is greater than 185% of poverty guideline
  mutate(payment = ifelse(monthly_income > guidelines_month, 0, payment),
         benefit = "WIC") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(wic, 'Buncombe_County_2020/benefits_tables/tables/wic.rds')
