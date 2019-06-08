###############################################################################
#
# This script calculates WIC benefits
#
###############################################################################

library(tidyverse)
head(wic)
wic <- read_rds('benefits_tables/tables/base.rds')

# average food costs per person in 2018 was 42.28
# https://www.fns.usda.gov/pd/wic-program
# assumes recipients get this amount

# can receive wic up to 185% of fpl
fpl <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # multiply guideline amount by 1.85 so it is at 2185%
  mutate(guidelines_month = guidelines_month * 1.48) %>%
  # only keep 2018
  filter(year == 2018) %>%
  select(size = household_size, guidelines_month)

# add 185% poverty limit to WIC data set
wic <- wic %>%
  left_join(fpl, by='size') %>%
  # set payment to 0 if income is greater than 185% of poverty guideline
  mutate(payment = ifelse(monthly_income > guidelines_month, 0, payment),
         benefit = "Child care subsidy") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(care, 'benefits_tables/tables/child_care_subsidy.rds')
