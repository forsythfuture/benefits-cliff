#########################################################
#
# This script creates Altair plots for the benefits cliff
#
#########################################################

library(tidyverse)
library(plotly)
library(highcharter)

benefits <- read_csv("plots/benefits.csv") %>%
  filter(composition == "One adult, three children")

tanf <- benefits %>%
  filter(benefit == "Work First (TANF)")

row.names(tanf) <- tanf$monthly_income

# highcharts --------------------------------------
hchart(benefits, hcaes(monthly_income, payment, group=benefit),
       type="line")

# plotly ---------------------------------------------

# find max x and y values, will be used for range of plot
x_max <- max(benefits$monthly_income)
y_max <- max(benefits$payment)

# unique benefits values for drop down
unique_composition <- as.character(unique(benefits$composition))

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

benefits %>%
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