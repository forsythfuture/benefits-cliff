############################################################################
#
# This script combines the individual benefit datasets to create a master
# csv dataset for plotting with d3
#
##############################################################################

library(tidyverse)

file_dir <- "benefits_tables/tables/"

master <- bind_rows(
  list(
    read_rds(str_c(file_dir, 'work_first.rds')),
    read_rds(str_c(file_dir, 'fns.rds')),
    read_rds(str_c(file_dir, 'child_care_subsidy.rds')),
    read_rds(str_c(file_dir, 'prek.rds')),
    read_rds(str_c(file_dir, 'sec8.rds')),
    read_rds(str_c(file_dir, 'medical.rds')),
    read_rds(str_c(file_dir, 'wic.rds'))
  )) %>%
  arrange(benefit, monthly_income, adults, children)

write_csv(master, "plots/benefits.csv")
write_rds(master, "plots/benefits.rds")
