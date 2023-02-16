###############################################################
#
# State and federal child care subsidies
#
##############################################################

library(tidyverse)

care <- read_rds('~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/base.rds')

# the market value of subsidies are based on the 2022 NC subsidized child care
# market rates for Forsyth County 4-star child care centers
# https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/C/Child_Care_Centers_Rates_2022.pdf?ver=1tlPYuEVmYXfXutOxVRMpQ%3d%3d

# Forsyth County market rates for $855 for infant and $750 for 3-5
# we will assume that families with 2 or more children have an infant and 3-5 year old
# while families with one child only have a 3-5 year old
# create named vector to map number of children to total market rate amounts
market_rates <- c(`0` = 0,
                  `1` = 750,
                  `2` = 1605, # 855 + 750 (infant plus 3 to 5)
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

# can receive benefits up to 200% fpg

# read in federal poverty guidelines
fpg <- read_rds('~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/federal_poverty_guidelines.rds') %>%
  # convert guideline amounts to 200% and filter for 2023
  filter(year == 2023) %>%
  mutate(income_limit = round(guidelines_month * 2, 0)) %>%
  rename(size = household_size) %>%
  select(size, income_limit)

# add 200% fpl to child care data set
care <- care %>%
  left_join(fpg, by = "size") %>%
  # set payment to 0 if income is greater than 200% of poverty guideline
  mutate(payment = ifelse(monthly_income > income_limit, 0, payment),
         benefit = "NC Child Care Subsidy / Smart Start") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(care, '~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/child_care_subsidy.rds')
