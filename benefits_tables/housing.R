################################################################################
#
# section 8 housing vouchers
#
###############################################################################

library(tidyverse)

housing <- read_rds('benefits_tables/tables/base.rds')

# income limits cannot exceed 50% of median in area (very low income)
# 50% limit amounts based on family size
# https://www.huduser.gov/portal/datasets/il/il19/Section8-IncomeLimits-FY19.pdf
income_limits <- c(`1` = 21700,
                   `2` = 24800,
                   `3` = 27900,
                   `4` = 30950,
                   `5` = 33450,
                   `6` = 35950,
                   `7` = 38400,
                   `8` = 40900)

# convert from yearly to monthly
income_limits <- income_limits /12

# create data frame of fair market rent values in 2019 based on family size
# this is the max rent that can be reimbursed
# https://www.huduser.gov/portal/datasets/fmr.html#2019_data
fmr <- housing %>%
  select(adults, children) %>%
  distinct() %>%
  mutate(fmr = c(583, 729, 729, 985, 583, 729, 729, 985),
  # we will assume people's rent amount is 20% of fmr
        rent = round(fmr * .8 , 0)) %>%
  select(-fmr)

# add fmr to data set
housing <- housing %>%
  left_join(fmr, by=c('adults', 'children')) %>%
  # tenants have to pay 30% of their income
  mutate(payment = round(rent - (monthly_income * .3), 0),
        # add income limits
        income_limits = recode(.$size, !!!income_limits),
        # if income exceeds limits, reduce payment to 0
        payment = ifelse(monthly_income > income_limits, 0, payment),
        # if payment is negative, make it 0
        payment = ifelse(payment < 0, 0, payment),
        benefit = "Housing Choice Voucher") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(housing, 'benefits_tables/tables/sec8.rds')
