#############################################################################
#
# Create table of SNAP (food stamp) benefits for 2018
#
#############################################################################

library(tidyverse)

base <- read_rds('benefits_tables/tables/base.rds')

snap <- base %>%
  mutate(benefit = "SNAP (food stamps)")

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
# read in federal poverty guidelines
fpg <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds')

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
        payment = ifelse(monthly_income > snap_income_limit, 0, snap_amount)) %>%
  select(composition, monthly_income, payment, benefit)

write_rds(snap, 'benefits_tables/tables/snap.rds')
