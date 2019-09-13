##################################################################################
#
# This script creates output for after tax income and each benefit.  The results
# are sent to a json file for input into a java script visualization.
#
##################################################################################

library(tidyverse)
library(jsonlite)

# import data and create base dataset -------------

tax <- read_csv('tax_liability/tax_output.csv') %>%
  # calcualte NC income taxes (flat tax of 5.5% of taxable income)
  mutate(nc_tax = c04800 * .055,
         # recomputed after-tax income
         aftertax_income = round(aftertax_income - nc_tax, 2)) %>%
  select(aftertax_inc = aftertax_income, pretax_inc = c00100, eitc) %>%
  # remove EITC from after tax income, so that it is on the same playing field as benefits
  mutate(aftertax_inc = aftertax_inc - eitc) %>%
  # convert all amounts to monthly amounts by dividing by 12
  mutate_all(list(~(round(. / 12, 2))))

base <- read_rds('benefits_tables/tables/base.rds')

master <- bind_cols(tax, base) %>%
  select(-monthly_income)

# we need to create separate dataframes for after tax and eitc,
# then stack them
after_tax <- master %>%
  select(-eitc) %>%
  mutate(category = 'After-tax income') %>%
  rename(value = aftertax_inc)

eitc <- master %>%
  select(-aftertax_inc) %>%
  mutate(category = 'EITC') %>%
  rename(value = eitc)

master <- bind_rows(after_tax, eitc) %>%
  select(-size, -adults, -children) %>%
  select(composition, category, pretax_inc, value)

# import and attach benefit data ------------------

benefits <- read_rds("plots/benefits.rds") %>%
  # remove smart start because no one will receive child care subsidies and smart start
  # also remove health care because it is tricky
  filter(!(benefit %in% c("Smart Start","NC Medicaid / Health Choice")))

benefits <- benefits %>%
  select(-adults, -children) %>%
  # rename columns to match master
  rename(value = payment, pretax_inc = monthly_income, category = benefit)

# bind master to benefits
master <- bind_rows(master, benefits) %>%
  mutate(hourly = pretax_inc / (40*4.35))

# create column of differences ------------------

difference <- master %>%
  group_by(composition) %>%
  mutate(diff = value - lag(value),
         # if pre-tax income is 0, make difference 0
         diff = ifelse(pretax_inc == 0, 0, diff),
        # Work first is acting wonky, add $1 to all differencecs less than 1 and 
        # greater than 0
        diff = ifelse((category == "Work First (TANF)" & diff < 0), diff + 1, diff))
  

# write out full dataset with hourly to an rds file
write_rds(difference, "tax_liability/income_diff.rds")

# write out condensed json dataset for plotting with java script
difference %>%
  select(-hourly) %>%
  filter(pretax_inc <= 7500) %>%
  write_json("plots/income_diff.json") 
