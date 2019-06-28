###############################################################################
#
# This script calculates WIC benefits
#
###############################################################################

library(tidyverse)

wic <- read_rds('benefits_tables/tables/base.rds')

# average benefit per person in 2018 was 42.28
# https://fns-prod.azureedge.net/sites/default/files/resource-files/25wifyavgfd$-5.xls
# assume recipients get this amount
# to get benefits, the children must be under 5
# we'll assume that all children are under 5 except
# for one child in the 3 child house
# the mother also gets benefits, and we'll assume that in one
# adult households the adult is the mother
# given this, the amount of the benefit is the number of children plus one,
# times 42.28, minus 42.28 if there are three children
wic <- wic %>%
  # only get WIC payment if family has kids
  mutate(payment = ifelse(children > 0, (children + 1) * 42.28, 0),
         payment = ifelse(children == 3, payment - 42.28, payment))

# can receive wic up to 185% of fpl
fpl <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # multiply guideline amount by 1.85 so it is at 185%
  mutate(guidelines_month = guidelines_month * 1.85) %>%
  # only keep 2018
  filter(year == 2019) %>%
  select(size = household_size, guidelines_month)

# add 185% poverty limit to WIC data set
wic <- wic %>%
  left_join(fpl, by='size') %>%
  # set payment to 0 if income is greater than 185% of poverty guideline
  mutate(payment = ifelse(monthly_income > guidelines_month, 0, payment),
         benefit = "WIC") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(wic, 'benefits_tables/tables/wic.rds')
