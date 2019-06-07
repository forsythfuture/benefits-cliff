###############################################################################
#
# This file creates the final data sets that use the tax information
#
###############################################################################

library(tidyverse)
library(plotly)

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

# clean data -------------------------------------------------------------------

# add tax information to base
# rows are the same
master <- bind_cols(base, tax) %>%
  # don't need c00100 because it is the monthly_income column in base
  # also don't need regular taxable income (c04800)
  select(-c00100, -c04800, -size:-children) %>%
  # add amount received in benefites
  left_join(benefits, by=c("composition", "monthly_income")) %>%
  # after tax income currently includes EITC
  # remove EITC from it, so we have a column that is jsut after-tax income without eitc
  mutate(aftertax_income = round(aftertax_income - eitc, 2),
        # make column that is net income after and benefits and eitc
        net_income = round(aftertax_income + payment + eitc, 2)) %>%
  select(-eitc, -payment)

# we need to stack after tax income and net income into long form;
# currently they are in different columns, convert them into different rows
master <- master %>%
  pivot_longer(c('aftertax_income', 'net_income'),
               names_to = "benefit", values_to = "payment") %>%
  mutate(benefit = str_replace_all(benefit, "^after.*", "After-tax income"),
         benefit = str_replace_all(benefit, "^net.*", "After-tax income plus benefits"))

# write out as csv for plotting
write_csv(master, "plots/total_income.csv")

# ----------------


# find max x and y values, will be used for range of plot
x_max <- max(master$monthly_income)
y_max <- max(master$payment)

# unique benefits values for drop down
unique_composition <- as.character(unique(master$composition))

# create list of arguments for drop down arrows
drop_down_values <- map(unique_composition, function(x) {
  list(method = "restyle",
       args = list("transforms[0].value", x),
       label = x)
})

# plotly

tooltip_benefits <- function() {
  ~paste0("Benefit: ", benefit,
          "<br>Monthly Payment:  $", payment,
          "<br>Monthly Wages:  $", monthly_income)
}

master %>%
  plot_ly(x=~monthly_income, y=~payment, color=~benefit,
          type = 'scatter', mode = 'lines',
          # tooltip info
          hoverinfo = 'text',
          text = tooltip_benefits(),
          transforms = list(
            list(
              type = 'filter',
              target = ~composition,
              operation = '=',
              value = unique_composition[1])
          )) %>%
  layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = drop_down_values
      )
    ),
    xaxis = list(title="Monthly Wages", tickformat = "$",
                 range=c(0, x_max)),
    yaxis = list(title="Monthly Benefit Payment", tickformat = "$",
                 range=c(0, y_max))
  ) %>%
  config(displayModeBar = FALSE)


# create dataset of EITC benefits ----------------------------------------------
eitc <- base %>%
  select(composition, adults, children, monthly_income, payment = eitc) %>%
  mutate(benefit = "Earned Income Tax Credit")

write_rds(eitc, 'benefits_tables/tables/eitc.rds')
