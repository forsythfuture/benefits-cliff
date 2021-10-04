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
# these levels were pulled from page 3-5 of the policy manual:
# go to C. Eligibility for Families at or below 75% of State Median Income
# https://ccpfc.org/wp-content/uploads/2021/09/2021-2022-NC-Pre-K-Program-Requirements-and-Guidance_FINAL_September-2021.pdf

# make income limits a named vector that can be mapped on to family sizes
income_limits <- c(`1` = 31488,
                   `2` = 41176,
                   `3` = 50865,
                   `4` = 60554,
                   `5` = 70242,
                   `6` = 79931)

# these limits are yearly, so divide by 12 to get monthly limits
income_limits <- income_limits / 12

# amount of reimbursement to school is $650 per month for a private program
# source is on page 6-18 of the manual linked above
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
