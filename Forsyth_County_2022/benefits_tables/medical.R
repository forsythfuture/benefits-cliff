###############################################################################
#
# This script calculates Medicaid for Infants and Children (mic),
# Medicaid for Families with Dependent Children (maf),
# and NC Health Choices
#
# All eligibility limits are here:
# 2020: https://files.nc.gov/ncdma/documents/files/Basic-Medicaid-Eligibility-Chart-2020.pdf
# 2023: https://policies.ncdhhs.gov/divisional/health-benefits-nc-medicaid/family-and-childrens-medicaid/family-and-childrens-medicaid/ma-3321.pdf
#
###############################################################################

library(tidyverse)

medical <- read_rds('~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/base.rds')

# read in poverty guidelines and filter for 2023
fpl <- read_rds("~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/federal_poverty_guidelines.rds") %>%
  filter(year == 2023) %>%
  select(household_size, guidelines_month)

# the value of the health benefit is the price of a silver plan on the ACA market
# silver plan prices for Forsyth County in 2023 were retrieved from: 
# https://www.kff.org/interactive/subsidy-calculator/

# since children and adults qualify for different programs, calculate the value 
# of their silver plans separately
  # 1. filter for North Carolina and input a Forsyth County zip code
  # 2. put zero for '2. Enter yearly household income as...'
  # 3. put No for '3. Is coverage available from your or your spouse’s job?'

# calculate value of children's silver plans, based on number of children
mic_value <- c(`1` = 267, # one child: 2 years old
                `2` = 535, # two children: 2 and 4 years old
                `3` = 802 # three children: 2, 4, and 10 years old
                )

# calculate value of adults silver plan; adults are non-tobacco users
maf_value <- c(`1` = 397, # one adult: 30 years old
               `2` = 793 # two adults: both 30 years old
               )

# calculate income limits of maf based on number of caretakers in 2023
  # no updates in 2023
# https://policies.ncdhhs.gov/divisional/health-benefits-nc-medicaid/family-and-childrens-medicaid/family-and-childrens-medicaid/ma-3321.pdf
maf_income_limits <- c(`1` = 434,
                       `2` = 569,
                       `3` = 667,
                       `4` = 744,
                       `5` = 824)

# medicaid for children and NC health choices provide generally the same
# benefits, and they create a continuous eligibility stream up to 210%
# of the federal poverty level
# therefore, convert the poverty level to 210%, as a ceiling
fpl$guidelines_month <- fpl$guidelines_month * 2.1

# create columns in dataset for the market value of mic and maf
# and column for maf income thresholds
medical <- medical %>%
  mutate(payment_mic = recode(children, !!!mic_value),
         payment_maf = recode(adults, !!!maf_value),
         maf_threshold = recode(size, !!!maf_income_limits)) %>%
  # merge fpl thresholds
  left_join(fpl, by = c("size" = "household_size")) %>%
  # if family does not qualify for mic or health choice due to high income, make payment 0
  mutate(payment_mic = ifelse(monthly_income > guidelines_month, 0, payment_mic),
         # if family does not qualify for maf due to high income, make payment 0
         payment_maf = ifelse(monthly_income > maf_threshold, 0, payment_maf),
         # cannot receive maf without children
         payment_maf = ifelse(children == 0, 0, payment_maf),
         # sum mic and maf into one payment column
         payment = payment_mic + payment_maf,
         benefit = "NC Medicaid / Health Choice") %>%
  select(composition, adults, children, monthly_income, payment, benefit)

write_rds(medical, '~/benefits-cliff/Forsyth_County_2022/benefits_tables/tables/medical.rds')
