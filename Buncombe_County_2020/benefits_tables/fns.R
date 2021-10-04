#############################################################################
#
# Create table of FNS (food stamp) benefits for 2021
# FNS is called SNAP federally, and is often called SNAP in this script
#
# the source of the information for the calculation is primarily from:
# https://fns-prod.azureedge.net/sites/default/files/resource-files/COLAMemoFY2021.pdf
#
#############################################################################

library(tidyverse)

base <- read_rds('Buncombe_County_2020/benefits_tables/tables/base.rds')

snap <- base %>%
  mutate(benefit = "FNS (Food Stamps)")

# utility allowance based on family size https://www.fns.usda.gov/snap/eligibility/deduction/standard-utility-allowances or 
# https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Ffns-prod.azureedge.net%2Fsites%2Fdefault%2Ffiles%2Fmedia%2Ffile%2FSNAP_SUA_Table_FY2021.xlsx&wdOrigin=BROWSELINK
shelter_costs <- tibble(
    size = seq(1, 5),
    sua = c(440, 483, 531, 579, 631),
    bua = c(263, 289, 318, 347, 378),
    tua = 31,
    # rent starts at $600 and each additional person adds $200
    rent = 600 + (200*size)
  ) %>%
    # make the shelter deduction the standard utility deduction and rent
    mutate(shelter = sua + rent) %>%
    select(size, shelter)

# merge utility allowances to snap dataset
snap <- snap %>%
  left_join(shelter_costs, by="size")

# standard deductions based on family size https://fns-prod.azureedge.net/sites/default/files/resource-files/COLAMemoFY2021.pdf
std_ded <- c(`1` = 167,
             `2` = 167,
             `3` = 167,
             `4` = 181,
             `5` = 212,
             `6` = 243)

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
        # shelter deduction is maxed out at 586 https://www.fns.usda.gov/snap/fy-2021-cost-living-adjustments
        shelter_ded = ifelse(shelter_ded > 586, 586, shelter_ded),
        # subtract shelter deduction from net income
        net_income = net_income - shelter_ded,
        # family is expected to contribute 30% of income to food
        family_contribution = net_income * .3,
        # convert this amount to 0 if it is negative
        family_contribution = ifelse(family_contribution < 0, 0, family_contribution))

# SNAP max allotment amounts Oct 2020 - Sep 2021 https://fns-prod.azureedge.net/sites/default/files/resource-files/COLAMemoFY2021.pdf
snap_amounts <- c(`1` = 204,
                  `2` = 374,
                  `3` = 535,
                  `4` = 680,
                  `5` = 807,
                  `6` = 969,
                  `7` = 1071,
                  `8` = 1224)

# maximum income is set at 200% of federal poverty guideline
# read in federal poverty guidelines
fpg <- read_rds('Buncombe_County_2020/benefits_tables/tables/federal_poverty_guidelines.rds')

# convert guideline amounts to 200% and filter for 2021
snap_income_limit <- fpg %>%
  filter(year == 2021) %>%
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

write_rds(snap, 'Buncombe_County_2020/benefits_tables/tables/fns.rds')
