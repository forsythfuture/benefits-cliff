###############################################################################
#
# This script creates the base table that will be used for all beneftis tables
#
###############################################################################

library(tidyverse)

# create a base data frame that is the household size, household composition, and income
# columns for all benefit data frames
incomes <- seq(0, 6500, by=10)

# create base composition, and then we will paste sizes on to this
composition <- c("1 adult", "1 adult, 1 child", "1 adult, 2 children", "1 adult, 3 children",
                 "2 adults", "2 adults, 1 child", "2 adults, 2 children", "2 adults, 3 children")

# dataframe for number of adutls and children
adults_children <- data.frame(adults = c(rep(1, 4), rep(2, 4)),
                              children = c(rep(c(0, 1, 2, 3), 2)))

# sizes should match composition levels
sizes <- c(seq(1, 4), seq(2, 5))

# we now need a data frame that lists every income for all composition levels

# create data frame that is just the compositin and size, and we will add incomes later
comp_size <- data.frame(composition = composition,
                        size = sizes) %>%
  bind_cols(adults_children)

# iterate through each income value, adding that value to the comp_size data frame,
# then add the data frame to the main data frame containing all incomes
base <- map_df(incomes, function(x) mutate(comp_size, monthly_income = x))

write_rds(base, 'benefits_tables/tables/base.rds')
