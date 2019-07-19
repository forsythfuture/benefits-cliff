###############################################################
#
# Smart start child care subsidy
#
##############################################################

library(tidyverse)

care <- read_rds('benefits_tables/tables/base.rds')

# the market value of subsidies are based on the 2019 NC subsidized child care
# market rates for Forsyth County 4-star child care centers
# https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/R/Revised-8-16-Market_Rate_Centers_Eff-10-1-18.pdf?ver=2018-08-28-105655-863

# Forsyth County market rates for $855 for infant and $750 for 3-5
# we will assume that families with 2 or more children have an infant and 3-5 year old
# while families with one child only have a 3-5 year old
# create named vector to map number of children to total market rate amounts
market_rates <- c(`0` = 0,
                  `1` = 750,
                  `2` = 1605, # 855 + 750 (infant plut 3 to 5)
                  `3` = 1605 # for three child families, only two are under 5
                  )

# create new column for market rates, which signifies benefit level
care <- care %>%
  mutate(payment = recode(.$children, !!!market_rates),
  # recipients have to pay 10% of income
  # so subtract this amount from
  payment = round(payment - (monthly_income * .10), 2),
  # if payment is negative, convert to 0
  payment = ifelse(payment < 0, 0, payment))

# can receive child care subsidies up to 200% of fpl, which is the TANF cut-off

# read in federal poverty guidelines
fpg <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # convert guideline amounts to 200% and filter for 2019
  filter(year == 2019) %>%
  mutate(income_limit = round(guidelines_month * 1.3, 0)) %>%
  rename(size = household_size) %>%
  select(size, income_limit)

# add 200% fpl to child care data set
care <- care %>%
  left_join(fpg, by = "size") %>%
  # set payment to 0 if income is greater than 200% of poverty guideline
  mutate(payment = ifelse(monthly_income > income_limit, 0, payment),
         benefit = "Smart Start") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(care, 'benefits_tables/tables/smart_start.rds')
