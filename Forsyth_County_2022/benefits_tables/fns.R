#############################################################################
#
# Create table of FNS (food stamp) benefits for 2022
# FNS is called SNAP federally, and is often called SNAP in this script
#
# the source of the information for the calculation is primarily from:
# https://fns-prod.azureedge.net/sites/default/files/resource-files/COLA%20Memo%20FY2022%20Without%20Maximum%20Allotments.pdf
# https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf#page=2
#
#############################################################################

library(tidyverse)

base <- read_rds('~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/base.rds')

snap <- base %>%
  mutate(benefit = "FNS (Food Stamps)")

# utility allowance based on family size https://www.fns.usda.gov/snap/eligibility/deduction/standard-utility-allowances
shelter_costs <- tibble(
  size = seq(1, 5),
  sua = c(550, 610, 670, 730, 796),
  bua = c(331, 364, 400, 475, 475),
  tua = 29,
  # rent starts at $600 and each additional person adds $200
  rent = 600 + (200*size)
) %>%
  # make the shelter deduction the standard utility deduction and rent
  mutate(shelter = sua + rent) %>%
  select(size, shelter)

# merge utilitiy allowances to snap dataset
snap <- snap %>%
  left_join(shelter_costs, by="size")

# standard deductions based on family size https://fns-prod.azureedge.net/sites/default/files/resource-files/COLA%20Memo%20FY2022%20Without%20Maximum%20Allotments.pdf
std_ded <- c(`1` = 177,
             `2` = 177,
             `3` = 177,
             `4` = 184,
             `5` = 215,
             `6` = 246)

# add column to dataset showing standard deduction amount
snap <- snap %>%
  mutate(std_ded = recode(.$size, !!!std_ded),
         # 20 percent of earned income is deducted,
         # so add column showing this amount
         ded_20 = monthly_income * .2,
         # for dependent care deduction, assume $200 per child per month
         dep_care = children * 400)

# calculate SNAP amounts
snap <- snap %>%
  # calculate net income:
  # subtract standard deduction, earnings deducting, and child care deduction
  mutate(net_income = monthly_income - std_ded - dep_care - ded_20,
         # deduct shelter expenses that exceed half of net income
         shelter_ded = shelter - (net_income/2),
         # shelter deduction is maxed out at 597 https://fns-prod.azureedge.net/sites/default/files/resource-files/COLA%20Memo%20FY2022%20Without%20Maximum%20Allotments.pdf
         shelter_ded = ifelse(shelter_ded > 597, 597, shelter_ded),
         # subtract shelter deduction from net income
         net_income = net_income - shelter_ded,
         # family is expected to contribute 30% of income to food
         family_contribution = net_income * .3,
         # convert this amount to 0 if it is negative
         family_contribution = ifelse(family_contribution < 0, 0, family_contribution))

# SNAP max allotment amounts Oct 2021 - Sep 2022 https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf#page=2
snap_amounts <- c(`1` = 250,
                  `2` = 459,
                  `3` = 658,
                  `4` = 835,
                  `5` = 992,
                  `6` = 1190,
                  `7` = 1316,
                  `8` = 1504)

# maximum income is set at 200% of federal poverty guideline
# read in federal poverty guidelines
fpg <- read_rds('~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/federal_poverty_guidelines.rds')

# convert guideline amounts to 200% and filter for 2019
snap_income_limit <- fpg %>%
  filter(year == 2022) %>%
  mutate(snap_income_limit = round(guidelines_month * 2, 0)) %>%
  rename(size = household_size) %>%
  select(size, snap_income_limit)

# add benefit and income limit amounts to dataset
snap <- snap %>%
  arrange(monthly_income, adults, children) %>%
  mutate(max_allotment = recode(.$size, !!!snap_amounts)) %>%
  left_join(snap_income_limit, by = "size") %>%
  # find benefit amount by subtracting family contribution from maximum benefit
  mutate(snap_amount = max_allotment - family_contribution,
         # for families over 200% of federal poverty line, make benefit 0
         payment = ifelse(monthly_income > snap_income_limit, 0, snap_amount),
         # families with negative values for payment get zero in benefits
         payment = ifelse(payment < 0, 0, payment),
         # one and two person families must have at least $15 in benefits
         payment = ifelse((size %in% c(1,2) & payment < 15), 0, payment)) %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(snap, '~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/fns.rds')
