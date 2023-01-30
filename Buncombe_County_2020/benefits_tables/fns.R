#############################################################################
#
# Create table of FNS (food stamp) benefits for 2023
# FNS is called SNAP federally, and is often called SNAP in this script
#
# the source of the information for the calculation is primarily from:
# 2023: https://fns-prod.azureedge.us/sites/default/files/resource-files/snap-fy-2023-cola-adjustments.pdf
# 2022: https://fns-prod.azureedge.net/sites/default/files/resource-files/COLA%20Memo%20FY2022%20Without%20Maximum%20Allotments.pdf
# https://fns-prod.azureedge.net/sites/default/files/resource-files/2022-SNAP-COLA-%20Maximum-Allotments.pdf#page=2
#
#############################################################################

library(tidyverse)

base <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

snap <- base %>%
  mutate(benefit = "FNS (Food Stamps)")

# utility allowance based on family size https://www.fns.usda.gov/snap/eligibility/deduction/standard-utility-allowances
shelter_costs <- tibble(
    size = seq(1, 5),
    sua = c(557, 612, 672, 732, 798),
    bua = c(339, 372, 409, 446, 486),
    tua = 40,
    # rent starts at $600 and each additional person adds $200
    rent = 600 + (200 * size)
  ) %>%
    # make the shelter deduction the standard utility deduction and rent
    mutate(shelter = sua + rent) %>%
    select(size, shelter)

# merge utility allowances to snap dataset
snap <- snap %>%
  left_join(shelter_costs, by="size")

# standard deductions based on family size https://fns-prod.azureedge.us/sites/default/files/resource-files/snap-fy-2023-cola-adjustments.pdf
std_ded <- c(`1` = 193,
             `2` = 193,
             `3` = 193,
             `4` = 193,
             `5` = 225,
             `6` = 258)

# add column to dataset showing standard deduction amount
snap <- snap %>%
  mutate(std_ded = recode(.$size, !!!std_ded),
    # 20 percent of earned income is deducted,
    # so add column showing this amount
    ded_20 = monthly_income * .2,
    # for dependent care deduction, assume $400 per child per month
    dep_care = children * 400)

# calculate SNAP amounts
snap <- snap %>%
  # calculate net income:
  # subtract standard deduction, earnings deducting, and child care deduction
  mutate(net_income = monthly_income - std_ded - dep_care - ded_20,
        # deduct shelter expenses that exceed half of net income
        shelter_ded = shelter - (net_income/2),
        # shelter deduction is maxed out at 624 https://fns-prod.azureedge.us/sites/default/files/resource-files/snap-fy-2023-cola-adjustments.pdf
        shelter_ded = ifelse(shelter_ded > 624, 624, shelter_ded),
        # subtract shelter deduction from net income
        net_income = net_income - shelter_ded,
        # family is expected to contribute 30% of income to food
        family_contribution = net_income * .3,
        # convert this amount to 0 if it is negative
        family_contribution = ifelse(family_contribution < 0, 0, family_contribution))

# SNAP max allotment amounts Oct 2022 - Sep 2023 https://fns-prod.azureedge.us/sites/default/files/resource-files/snap-fy-2023-cola-adjustments.pdf
snap_amounts <- c(`1` = 281,
                  `2` = 516,
                  `3` = 740,
                  `4` = 939,
                  `5` = 1116,
                  `6` = 1339,
                  `7` = 1480,
                  `8` = 1691)

# maximum income is set at 200% of federal poverty guideline
# read in federal poverty guidelines
fpg <- read_rds('Buncombe_County_2020/benefits_tables/tables/federal_poverty_guidelines.rds')

# convert guideline amounts to 200% and filter for 2023
snap_income_limit <- fpg %>%
  filter(year == 2023) %>%
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
        # one and two person families must have at least $20 in benefits
        # https://policies.ncdhhs.gov/divisional/social-services/food-and-nutrition-services/policy-manuals/fns-360-determining-benefit-levels.pdf
        payment = ifelse((size %in% c(1,2) & payment < 20), 0, payment)) %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(snap, 'Buncombe_County_2020/benefits_tables/tables/fns.rds')
