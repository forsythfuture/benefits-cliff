###############################################################################
#
# This script creates the base table that will be used for all beneftis tables
#
###############################################################################

library(tidyverse)

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

write_rds(base, 'benefits_tables/tables/base.rds')
