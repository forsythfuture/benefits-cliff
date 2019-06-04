#########################################################
#
# This script creates Altair plots for the benefits cliff
#
#########################################################

library(tidyverse)
library(plotly)

benefits <- read_csv("plots/benefits.csv")

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
    xaxis = list(title="Monthly Wages", tickformat = "$"),
    yaxis = list(title="Monthly Benefit Payment", tickformat = "$")
  ) %>%
  config(displayModeBar = FALSE)