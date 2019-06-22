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

# create function to calculate total tenant rent, which is
# the amount of rent the tenant has to pay
tenant_rent <- function(kids, income) {
  
  # total tenant payment (ttp) is the amount tenants have to pay in rent
  # it is the greater of 30% of their monthly adjusted income,
  # 10% of their monthly gross income, or $24
  # this function calcualtes the amount
  # source: https://www.hud.gov/sites/documents/43503C5HSGH.PDF
  
  # first, we'll calculate monthly adjusted income and multiply by .3
  # calculate adjsuted income by starting with income and subtracting the following deductions:
  #    dependent deduction of $480 (40 / month) for children 18 and under
  #    child care deduction for child care costs 
  #         we'll assume $4000 (333.33 / month) per child, based on the example on pg. 43 of above link
  #         but, 10 year old in 3 person home will not have any chiold care costs
  
  # calculate adjusted income and multiply by .3
  adjusted_income <-(income-(40*kids)-(333.33*kids)) * .3
  
  # multiply gross income by .1
  gross_income <- income * .1
  
  # figure out which number is higher: 
  # adjusted income or gross income and return that value
  ttp <- ifelse(adjusted_income  > gross_income, adjusted_income, gross_income)
  
  # if this amount is less than $25, make it $25
  ttp <- ifelse(ttp < 25, 25, ttp)
  
  # to calculate tentant rent, you subtract a utility allowance from ttp
  # the regulations use $75 as an example utility allowance, so this is the
  # alloawnce we will assume
  # if ttp is smaller than the allowance, the tenant gets a utility reimbursement
  # due to this, we will keep negative numbers negative
  rent_payment <- ttp - 75
  
  return(rent_payment)
  
}

# add fmr to data set
housing <- housing %>%
  left_join(fmr, by=c('adults', 'children'))

# calculate total tenant payment
# we're using an ifelse statement for kids because when there are
# three kids, we're only taking a deduction for two of the kids
# the third kid, who is ten, is assumed not to have any child care costs
housing$tenant_payment <- tenant_rent(ifelse(housing$children == 3, 2, housing$children), 
                                      housing$monthly_income)
housing <- housing %>%
  # benefit amount (payment) is rent minus ttp
  mutate(payment = rent - tenant_payment,
        # add income eligibility limits
        income_limits = recode(.$size, !!!income_limits),
        # if income exceeds limits, reduce payment to 0
        payment = ifelse(monthly_income > income_limits, 0, payment),
        # if payment is negative, make it 0
        payment = ifelse(payment < 0, 0, payment),
        payment = round(payment, 2),
        benefit = "Housing Choice Voucher") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(housing, 'benefits_tables/tables/sec8.rds')
