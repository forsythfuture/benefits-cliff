#############################################################
#
# This script creates tables of benefits
#
#############################################################

library(tidyverse)
# /home/ubuntu/miniconda3/bin/python

# base data frame for all benefits ------------------------------------

# create a base data frame that is the household size, household composition, and income
# columns for all benefit data frames
incomes <- seq(0, 5000, by=10)

# create base composition, and then we will paste sizes on to this
composition <- c("One adult", "One adult, one child", "One adult, two children", "One adult, three children",
                 "Two adults", "Two adults, one child", "Two adults, two children", "Two adults, three children")

# sizes should match composition levels
sizes <- c(seq(1, 4), seq(2, 5))

# create column for number of children
num_children <- rep(c(0, 1, 2, 3), 2)

# we now need a data frame that lists every income for all composition levels

# create data frame that is just the compositin and size, and we will add incomes later
comp_size <- data.frame(composition = composition,
                        size = sizes,
                        num_children = num_children)

# iterate through each income value, adding that value to the comp_size data frame,
# then add the data frame to the main data frame containing all incomes
base <- map_df(incomes, function(x) mutate(comp_size, monthly_income = x)) %>%
  # add boolean variabe of whether children are in the house
  mutate(children = ifelse(composition %in% c('One adult', 'Two adults'), F, T))


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

# make a copy of TANF to start a cumulative benefits dataframe
benefits_master <- tanf

# made_up <- tanf %>%
#   mutate(payment = round(payment * 1.4, 0),
#          benefit = 'Made up benefit') %>%
#   bind_rows(tanf)
#
# write_rds(made_up, 'benefits_tables/data/made_up.rds')

# write_rds(tanf, 'benefits_tables/data/work_first.rds')

# SNAP ------------------------------------------------------------------------

head(base)

snap <- base %>%
  mutate(benefit = "SNAP")

# SNAP income deductions

# utility allowance based on family size
shelter_costs <- data.frame(
    size = seq(1, 5),
    sua = c(437, 480, 528, 576, 628),
    bua = c(246, 270, 297, 324, 353),
    tua = 38,
    # assume rent is $500 for all people
    # this is 25th percentile of Forsyth County
    rent = 500
  ) %>%
    # sum all utility expenses
    mutate(shelter = sua + bua + tua + rent) %>%
    select(size, shelter)

# merge utilitiy allowances to snap dataset
snap <- snap %>%
  left_join(shelter_costs, by="size")

# standard deductions based on family size
std_ded <- c(`1` = 164,
             `2` = 164,
             `3` = 164,
             `4` = 174,
             `5` = 204,
             `6` = 234)

# add column to dataset showing standard deduction amount
snap <- snap %>%
  mutate(std_ded = recode(.$size, !!!std_ded),
    # 20 percent of earned income is deducted,
    # so add column showing this amount
    ded_20 = monthly_income * .2,
    # for dependent care deduction, assume $60 per child per month
    dep_care = num_children * 60)

# calculate SNAP amounts
snap <- snap %>%
  # calculate net income:
  # subtract standard deduction, earnings deducting, and child care deduction
  mutate(net_income = monthly_income - std_ded - dep_care - ded_20,
        # deduct shelter expenses that exceed half of net income
        shelter_ded = shelter - (net_income/2),
        # shelter deduction is maxed out at 552
        shelter_ded = ifelse(shelter_ded > 552, 552, shelter_ded),
        # subtract shelter deduction from net income
        net_income = net_income - shelter_ded,
        # family is expected to contribute 30% of income to food
        family_contribution = net_income * .3,
        # convert this amount to 0 if it is negative
        family_contribution = ifelse(family_contribution < 0, 0, family_contribution))

# SNAP max allotment amounts Oct 2018 - Sep 2019
snap_amounts <- c(`1` = 192,
                  `2` = 353,
                  `3` = 505,
                  `4` = 642,
                  `5` = 762,
                  `6` = 914,
                  `7` = 1011,
                  `8` = 1155)

# maximum income is set at 130% of federal poverty guideline
# convert guideline amounts to 130% and filter for 2018
snap_income_limit <- fpg %>%
  filter(year == 2018) %>%
  mutate(snap_income_limit = round(guidelines_month * 1.3, 0)) %>%
  rename(size = household_size) %>%
  select(size, snap_income_limit)

# add benefit and income limit amounts to dataset
snap <- snap %>%
  mutate(max_allotment = recode(.$size, !!!snap_amounts)) %>%
  left_join(snap_income_limit, by = "size") %>%
  # find benefit amount by subtracting family contribution from maximum benefit
  mutate(snap_amount = max_allotment - family_contribution,
        # for families over 130% of federal poverty line, make benefit 0
        snap_amount = ifelse(monthly_income > snap_income_limit, 0, snap_amount))

# end SNAP ---------------------------------------------------------------------


tail(snap)
