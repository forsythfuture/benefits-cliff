##############################################################################
#
# NC pre-K
#
##############################################################################

library(tidyverse)
library(tidycensus)

prek <- read_rds('benefits_tables/tables/base.rds')

# participants must be under 75% of state median income for family size
# these levels were pulled from here:
# https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/2/2018-19_NC_Pre-K_Program_Eligibility_Form_September_2018.pdf?ver=2018-09-28-182706-393
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
# use this as the monthly payment amount

# map limits on to prek data frame
prek <- prek %>%
  mutate(income_limit = recode(.$size, !!!income_limits),
          # amount of reimbursement to school is $650 per month for a private program
          # use this as the monthly payment amount
          # all children with families are assumed to have one four-year-old
          payment = ifelse(children > 0, 650, 0),
          # eliminate payments if income is over limits
          payment = ifelse(monthly_income > income_limit, 0, payment),
          benefit = "NC pre-K") %>%
   select(composition, adults, children, monthly_income, payment, benefit)

 write_rds(prek, 'benefits_tables/tables/prek.rds')
