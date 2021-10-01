#############################################################################
#
# create table of TANF (work first) benefits and income thresholds
# Source: https://files.nc.gov/ncdhhs/documents/files/4.3.17%20FINAL%20%26%20APPROVED%202016-2019%20TANF%20STATE%20PLAN.doc
# New Source: https://files.nc.gov/ncdhhs/documents/files/2019-2022%20TANF%20STATE%20PLAN%20DRAFT.pdf
# The numbers are the same
#
#############################################################################

library(tidyverse)

tanf_base <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

# monthly payment is 50% difference between total countable income and need standard (pg. 31)

# table below is need standard
tanf_need_std <- data.frame(household_size = c(2, 3, 4, 5),
                            need_std = c(472, 544, 594, 648))

# merge need standard to tanf dataset
tanf_base <- tanf_base %>%
  left_join(tanf_need_std, by=c('size'='household_size')) %>%
  # calculate payment as 50% difference between income and need std
  mutate(payment = round((need_std - monthly_income)*.5, 0),
         # payment must be $25 or more to recieve benefits
         payment = ifelse(payment >= 25, payment, 0),
         # cannot receive benefits if you don't have children
         payment = ifelse(children == 0, 0, payment))

# create final data set
tanf <- tanf_base %>%
  arrange(monthly_income, adults, children) %>%
  select(composition, adults, children, monthly_income, payment) %>%
  mutate(benefit = 'Work First (TANF)')

write_rds(tanf, 'Buncombe_County_2020/benefits_tables/tables/work_first.rds')
