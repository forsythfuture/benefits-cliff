################################################################################
#
# This script creates a dataset that is fed to tax-calculator to calculate federal taxes
# and the EITC.
#
# More information on using tax-calculator: https://pslmodels.github.io/Tax-Calculator/
#
################################################################################

library(tidyverse)

base <- read_rds('benefits_tables/tables/base.rds')

# there are 8 different family types, and each family type is a different
# tax filing unit type
# create 8 different tax filing units, and then map them to the family types
filing_unit <- data.frame(composition = unique(base$composition),
                          DSI = 0, # claimed as dependent on anyone else's return
                          EIC = rep(seq(0, 3), 2), # EIC qualifying children
                          FLPDYR = 2018, # caldenar year to calculate taxes
                          MARS = c(rep(1, 4), rep(2, 4)), # filing status
                          XTOT = c(seq(1, 4), seq(2, 5)) # total number of exemptions
                          )

# add tax info to base dataset with income
tax <- base %>%
  left_join(filing_unit, by = "composition") %>%
  select(DSI, EIC, FLPDYR, MARS, XTOT, e00200 = monthly_income) %>%
  # convert monthly incomes to yearly incomes
  mutate(e00200 = e00200 * 12,
         # make primary taxpayer's income 70% of total income, and secondary taxpayer's 30%
         # we need to divide income for payroll tax calculations
         e00200p = ifelse(MARS == 2, e00200 * .7, e00200),
         e00200p = round(e00200p, 2),
         # secondary tax payer's income is household income minus primary taxpayer's
         e00200s = e00200 - e00200p,
        # household ID: row number
         RECID = row_number())

# this csv file is then fed to the tax-calculator command line tool using the command:
# tc tax_inputs.csv 2018 --dump --dvars tax_dump_vars
# the output is renames 'tax_output.csv'
write_csv(tax, "tax_liability/tax_inputs.csv")
