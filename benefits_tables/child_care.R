###############################################################
#
# State and fedreal child care subsidies
#
##############################################################

library(tidyverse)

care <- read_rds('benefits_tables/tables/base.rds')

# Forsyth County market rates for subsides in 2018 are
# $855 for infant and $750 for 3-5
# we will assume that families with 2 or more children have an infant and 3-5 year old
# while families with one child only have a 3-5 year old
# create named vector to map number of children to total market rate amounts
market_rates <- c(`0` = 0,
                  `1` = 750,
                  `2` = 1605, # 855 + 750 (infant plut 3 to 5)
                  `3` = 1605)

# create new column for market rates, which signifies benefit level
care <- care %>%
  mutate(payment = recode(.$children, !!!market_rates),
  # recipients have to pay 10% of income
  # so subtract this amount from
  payment = round(payment - (monthly_income * .10), 2),
  # if payment is negative, convert to 0
  payment = ifelse(payment < 0, 0, payment))

# can receive child care subsidies up to 200% of fpl
fpl <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # double guideline amount so it is at 200%
  mutate(guidelines_month = guidelines_month * 2) %>%
  # only keep 2018
  filter(year == 2018) %>%
  select(size = household_size, guidelines_month)

# add 200% poverty limit to child care data set
care <- care %>%
  left_join(fpl, by='size') %>%
  # set payment to 0 if income is greater than 200% of poverty guideline
  mutate(payment = ifelse(monthly_income > guidelines_month, 0, payment),
         benefit = "Child care subsidy") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(care, 'benefits_tables/tables/child_care_subsidy.rds')
