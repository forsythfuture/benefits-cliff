#############################################################################
#
# create table of TANF (work first) benefits and income thresholds
#
#############################################################################

library(tidyverse)

base <- read_rds('benefits_tables/tables/base.rds')

# must have child in household to get TANF
tanf_base <- base %>%
  filter(children == T)

# monthly payment is 50% difference between total countable income and need standard

# table below is need standard
tanf_need_std <- data.frame(household_size = c(2, 3, 4, 5),
                            need_std = c(472, 544, 594, 648))

# merge need standard to tanf dataset
tanf_base <- tanf_base %>%
  left_join(tanf_need_std, by=c('size'='household_size')) %>%
  # calculate payment as 50% difference between income and need std
  mutate(payment = round((need_std - monthly_income)*.5, 0),
         #payment must be $25 or more to recieve benefits
         payment = ifelse(payment >= 25, payment, 0))

# create final data set
tanf <- tanf_base %>%
  select(composition, monthly_income, payment) %>%
  mutate(benefit = 'Work First (TANF)')

write_rds(tanf, 'benefits_tables/tables/work_first.rds')
