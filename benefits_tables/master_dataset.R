############################################################################
#
# This script combines the individual benefit datasets to create a master
# csv dataset for plotting
#
##############################################################################

library(tidyverse)

file_dir <- "benefits_tables/tables/"

master <- bind_rows(
  list(
    read_rds(str_c(file_dir, 'work_first.rds')),
    read_rds(str_c(file_dir, 'snap.rds')),
    read_rds(str_c(file_dir, 'child_care_subsidy.rds'))
  )) %>%
  arrange(monthly_income, adults, children) %>%
  select(-adults, -children)

write_csv(master, "plots/benefits.csv")
