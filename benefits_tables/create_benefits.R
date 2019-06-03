#############################################################
#
# THis script creates tables of benefits
#
#############################################################

library(tidyverse)
# /home/ubuntu/miniconda3/bin/python
# federal poverty guidelines for 2018 and 2019 ---------------------------

fpg <- data.frame(household_size = rep(seq_len(8), times = 2),
                  guidelines_year = c(12490, 16910, 21330, 25750, 30170, 34590, 39010, 43430,
                                     12140, 16460, 20780, 25100, 29420, 33740, 38060, 42380),
                  year = rep(c(2019, 2018), each=8)) %>%
  # add montly guidelines
  mutate(guidelines_month = round(guidelines_year / 12,0)) %>%
  select(household_size, year, everything())

# write out dataset as R object
#write_rds(fpg, 'benefits_tables/data/federal_poverty_guidelines.rds')

# base data frame for all benefits ------------------------------------

# create a base data frame that is the household size, household composition, and income
# columns for all benefit data frames
incomes <- seq(0, 5000, by=10)

# create base composition, and then we will paste sizes on to this
composition <- c("One adult", "One adult, one child", "One adult, two children", "One adult, three children",
                 "Two adults", "Two adults, one child", "Two adults, two children", "Two adults, three children")

# sizes should match composition levels
sizes <- c(seq(1, 4), seq(2, 5))

# we now need a data frame that lists every income for all composition levels

# create data frame that is just the compositin and size, and we will add incomes later
comp_size <- data.frame(composition = composition,
                        size = sizes)

# iterate through each income value, adding that value to the comp_size data frame,
# then add the data frame to the main data frame containing all incomes
base <- map_df(incomes, function(x) mutate(comp_size, monthly_income = x)) %>%
  # add boolean variabe of whether children are in the house
  mutate(children = ifelse(composition %in% c('One adult', 'Two adults'), F, T))

# TANF -----------------------------------------------------------------

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

# made_up <- tanf %>%
#   mutate(payment = round(payment * 1.4, 0),
#          benefit = 'Made up benefit') %>%
#   bind_rows(tanf)
# 
# write_rds(made_up, 'benefits_tables/data/made_up.rds')

write_rds(tanf, 'benefits_tables/data/work_first.rds')
