###############################################################################
#
# This script calculates Medicaid for Infants and Children and
# Medicaid for Families with Dependent Children
#
# Note: This script is incomplete at this time!!!!!!!!!!!!!!!!
#
###############################################################################

library(tidyverse)

medical <- read_rds('benefits_tables/tables/base.rds')

# the value of the health benefit is the price of a bronze plan on the ACA market
# bronze plan prices for Forsyth County in 2018 were retrieved from:
# https://www.kff.org/interactive/subsidy-calculator-2018/

# since children and adults qualify for different programs, calcualte the value
# of their bronze plans separately

# calculate value of children's bronze plans, based on number of children
chip_value <- c(`1` = 266, # one child: 2 years old
                `2` = 531, # two children: 2 and 4 years old
                `3` = 797 # three children: 2, 4, and 10 years old
                )

# calculate value of adults bronze plan; adults are non-tobacco users
dep_value <- c(`1` = 394, # one adult: 30 years old
               `2` = 788 # two adults: both 30 years old
               )
