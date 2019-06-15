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
  # don't include pre-k subsidies and medical
  filter(benefit != "NC Pre-K",
         benefit != "NC Medicaid / Health Choice")
# clean data -------------------------------------------------------------------

# we want to calculate total benefits for two groups:
# 1) all benefits except child care, and 2) all benefits, including child care
# we want a non-child care group because we want to plot it separately since it is so high
no_child_care <- benefits %>%
  filter(benefit != "Child Care Subsidy")

# create function that sums all benefits
sum_benefits <- function(df, benefit_name) {

  df <- df %>%
    # sum total benefits for each family type / income
    group_by(composition, monthly_income) %>%
    summarize(payment = sum(payment)) %>%
    mutate(benefit = benefit_name)

  return(df)
}

# sum benefits for all benefits, and for benefits without child care
total_benefits <- map2(list(benefits, no_child_care),
                       list("Child care", "No child care"),
                       sum_benefits) %>%
  bind_rows() %>%
  ungroup()

# add tax information to base
# rows are the same
master <- bind_cols(base, tax) %>%
  # don't need c00100 because it is the monthly_income column in base
  # also don't need regular taxable income (c04800)
  select(-c00100, -c04800, -size:-children) %>%
  # add amount received in benefites
  right_join(total_benefits, by=c("composition", "monthly_income")) %>%
  # make column that is net income after taxes, eitc, and benefits
  mutate(net_income = round(aftertax_income + payment, 2)) %>%
  select(-eitc, -payment)

rm(base, benefits, no_child_care, tax, total_benefits)

# we need to stack after tax income and net income into long form;
# currently they are in different columns,
# make after-tax income its own dataset, then bind to master
after_tax <- master %>%
  filter(benefit == "SNAP, TANF, Housing, WIC") %>%
  select(composition, monthly_income, aftertax_income) %>%
  rename(net_income = aftertax_income) %>%
  mutate(benefit = "After-tax income")

# bind after tax income to master
master <- master %>%
  select(-aftertax_income) %>%
  bind_rows(after_tax) %>%
  rename(payment = net_income) %>%
  # replace NA values with
  arrange(composition, monthly_income, payment, desc(benefit))

# write out as csv for plotting
write_csv(master, "plots/total_income.csv")
