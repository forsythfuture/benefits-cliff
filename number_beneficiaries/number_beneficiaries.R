###################################################################
#
# This script calculates the number of beneficiaries
# in Forsyth County for the various benefits
#
##################################################################

library(tidyverse)

# SNAP ------------------------------------------------------------

# date: July 17
# source: https://www.fns.usda.gov/pd/supplemental-nutrition-assistance-program-snap
# people: 46392
# households: 25079

# Housing Choice Vouchers -------------------------------------------

# date: 2018
# source: https://www.huduser.gov/portal/datasets/assthsg.html#null
# units available: 4524
# percent occupied: 87
# number per unit: 2.6
# number people total: 10402
# average family expenditure: 307
# average hud expenditure: 567
# % female head of household: 87%
# % female head of household with children: 58%
# % AA: 90
# % Hisp: 3
# average months on waitlist: 57
