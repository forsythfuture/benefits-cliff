###############################################################################
#
# This file creates the final data sets that use the tax information
#
###############################################################################

library(tidyverse)

# read in data ----------------------------------------------------------------

base <- read_rds('benefits_tables/tables/base.rds')

tax <- read_csv('tax_liability/tax_outputs.csv') %>%
  # calcualte NC income taxes (flat tax of 5.5% of taxable income)
  mutate(nc_tax = c04800 * .055,
        # recomputed after-tax income
        aftertax_income = round(aftertax_income - nc_tax, 2)) %>%
  select(-FLPDYR, -RECID, -nc_tax) %>%
  # convert all amounts to monthly amounts by dividing by 12
  mutate_all(list(~(round(. / 12, 2))))

# read in dataset of all benefits
benefits <- read_csv("plots/benefits.csv") %>%
  # don't include pre-k subsidies
  filter(benefit != "NC pre-K") %>%
  # sum total benefits for each family type / income
  group_by(composition, monthly_income) %>%
  summarize(payment = sum(payment))

head(benefits)

# clean data -------------------------------------------------------------------

# add tax information to base
# rows are the same
master <- bind_cols(base, tax) %>%
  # don't need c00100 because it is the monthly_income column in base
  # also don't need regular taxable income (c04800)
  select(-c00100, -c04800, -size:-children) %>%
  # add amount received in benefites
  left_join(benefits, by=c("composition", "monthly_income")) %>%
  # make column that is net income after and benefits
  # this will include EITC since EITC is included in aftertax_income
  mutate(net_income = round(aftertax_income + payment, 2)) %>%
  select(-eitc, -payment)

# we need to stack after tax income and net income into long form;
# currently they are in different columns, convert them into different rows

# start by making them di
master %>%
  pivot_longer(c('aftertax_income', 'net_income'))

head(master)



# create dataset of EITC benefits ----------------------------------------------
eitc <- base %>%
  select(composition, adults, children, monthly_income, payment = eitc) %>%
  mutate(benefit = "Earned Income Tax Credit")

write_rds(eitc, 'benefits_tables/tables/eitc.rds')
