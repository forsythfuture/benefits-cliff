################################################################################
#
# section 8 housing vouchers
#
###############################################################################

library(tidyverse)

housing <- read_rds('benefits_tables/tables/base.rds')

# income limits cannot exceed 30% of median in area
# 30% limit amoutns based on family size, as named vector for mapping
# https://www.huduser.gov/portal/datasets/il/il18/IncomeLimits-30-FY18.pdf
income_limits <- c(`1` = 13150,
                   `2` = 15000,
                   `3` = 16900,
                   `4` = 18750,
                   `5` = 20250,
                   `6` = 21750,
                   `7` = 23250,
                   `8` = 24750)

# convert from yearly to monthly
income_limits <- income_limits /12

# create data frame of fair market rent values based on family size
# this is the max rent that can be reimbursed
# https://www.huduser.gov/portal/datasets/fmr.html#2018_data
fmr <- housing %>%
  select(adults, children) %>%
  distinct() %>%
  mutate(fmr = c(594, 734, 734, 1002, 594, 734, 734, 1002),
  # we will assume people's rent amount is 20% of fmr
        rent = round(fmr * .8 , 0)) %>%
  select(-fmr)

# add fmr to data set
housing <- housing %>%
  left_join(fmr, by=c('adults', 'children')) %>%
  # tenants have to pay 30% of their income
  mutate(payment = round(rent - (monthly_income - .3), 0),
        # add income limits
        income_limits = recode(.$size, !!!income_limits),
        # if income exceeds limits, reduce payment to 0
        payment = ifelse(monthly_income > income_limits, 0, payment),
        # if payment is negative, make it 0
        payment = ifelse(payment < 0, 0, payment),
        benefit = "Housing Choice Voucher") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(housing, 'benefits_tables/tables/sec8.rds')
