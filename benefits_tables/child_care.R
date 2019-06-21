###############################################################
#
# State and fedreal child care subsidies
#
##############################################################

library(tidyverse)

care <- read_rds('benefits_tables/tables/base.rds')

# Forsyth County market rates for subsides in 2019 are
# $855 for infant and $750 for 3-5
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

# can receive child care subsidies up to 200% of fpl, and can keep
# benefits up to 85% of the state median income, which is higher
# since our focus is on when you lose benefits, we will use the 85% state median income

# 85% state median income numbers come from here:
# https://ncchildcare.ncdhhs.gov/Services/Financial-Assistance/Do-I-Qualify
smi <- c(`1` = 2578,
         `2` = 3372,
         `3` = 4165,
         `4` = 4958,
         `5` = 5752,
         `6` = 6542)

# add 200% poverty limit to child care data set
care <- care %>%
  mutate(guidelines_month = recode(.$size, !!! smi)) %>%
  # set payment to 0 if income is greater than 200% of poverty guideline
  mutate(payment = ifelse(monthly_income > guidelines_month, 0, payment),
         benefit = "Child Care Subsidy") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(care, 'benefits_tables/tables/child_care_subsidy.rds')
