################################################################################
#
# NC Housing choice vouchers
#
###############################################################################

library(tidyverse)

housing <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

# create data frame of fair market rent values in 2021 based on family size
# this is the max rent that can be reimbursed
# https://www.huduser.gov/portal/datasets/fmr.html#2021
fmr <- housing %>%
  select(adults, children) %>%
  distinct() %>%
  mutate(fmr = c(1099, 1279, 1279, 1751, 1099, 1279, 1279, 1751),
  # we will assume people's rent amount is 80% of fmr
        rent = round(fmr * .8 , 0)) %>%
  select(-fmr)

# create function to calculate total tenant rent, which is
# the amount of rent the tenant has to pay
tenant_rent <- function(kids, income) {

  # total tenant payment (ttp) is the amount tenants have to pay in rent
  # it is the greater of 30% of their monthly adjusted income,
  # 10% of their monthly gross income, or $25
  # this function calculates the amount
  # source: https://www.hud.gov/sites/documents/DOC_35649.PDF

  # first, we'll calculate monthly adjusted income and multiply by .3
  # calculate adjusted income by starting with income and subtracting the following deductions:
  #    dependent deduction of $480 (40 / month) for children 18 and under
  #    child care deduction for child care costs
  #         we'll assume $4000 (333.33 / month) per child, based on the example on pg. 43 of above link
  #         but, 10 year old in 3 person home will not have any child care costs

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
  # $153 is the average Buncombe County utility allowance, so we will use it
  # https://www.huduser.gov/portal/datasets/assthsg.html#null 
  # go to Data, 2020 - Based on Census 2010 geographies, Public Housing Agency
  # if ttp is smaller than the allowance, the tenant gets a utility reimbursement
  # due to this, we will keep negative numbers negative
  rent_payment <- ttp - 153

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
        # if payment is negative, make it 0
        payment = ifelse(payment < 0, 0, payment),
        payment = round(payment, 2),
        benefit = "Housing Choice Voucher") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(housing, 'Buncombe_County_2020/benefits_tables/tables/sec8.rds')
