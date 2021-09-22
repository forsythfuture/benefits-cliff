##############################################################################
#
# NC pre-K
#
# !!! Not used in final report because there is no cliff effect with this benefit
# !!! There is no cliff effect because children only receive the benefit for one
# !!! year, so there is no income recertification for parents.
#
##############################################################################

library(tidyverse)

prek <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

# participants must be under 75% of state median income for family size
# these levels were pulled from page 3-7 of the policy manual:
# https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/2/2018-19_NC_Pre-K_Program_Requirements_September_2018_FINAL.pdf?ver=2018-09-28-182336-967

# make income limits a named vector that can be mapped on to family sizes
income_limits <- c(`1` = 27300,
                   `2` = 35700,
                   `3` = 44100,
                   `4` = 52500,
                   `5` = 60900,
                   `6` = 69300)

# these limits are yearly, so divide by 12 to get monthly limits
income_limits <- income_limits / 12

# amount of reimbursement to school is $650 per month for a private program
# source is on page 6-13 of the manual linked above
# use this as the monthly payment amount (market value of benefit)

# map limits on to prek data frame
prek <- prek %>%
  mutate(income_limit = recode(.$size, !!!income_limits),
          # amount of reimbursement to school is $650 per month for a private program
          # use this as the monthly payment amount
          # all children with families are assumed to have one four-year-old
          payment = ifelse(children > 0, 650, 0),
          # eliminate payments if income is over limits
          payment = ifelse(monthly_income > income_limit, 0, payment),
          benefit = "NC Pre-K") %>%
   select(composition, adults, children, monthly_income, payment, benefit)

 write_rds(prek, 'Buncombe_County_2020/benefits_tables/tables/prek.rds')
