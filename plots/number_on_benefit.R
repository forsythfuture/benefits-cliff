###############################################################
#
# This script creates bar charts showing the number of people
# on various benefits in Forsyth County
#
################################################################

library(tidyverse)
library(plotly)

# create dataset of counts showing number of people on benefits ----------

numbers <- tribble(
  ~benefit,                   ~unit,                                           ~number, ~date,
  #------------------------------------------------------------------------------------------------
  "Work First (TANF)",        "Households",                                     334,   "May 2019",
  "Work First (TANF)",        "Individuals",                                    559,   "May 2019",
  "SNAP (food stamps)",       "Households",                                     25079, "July 2017",
  "SNAP (food stamps)",       "Individuals",                                    46392, "July 2017",
  "Housing Choice Vouchers",  "Households",                                     3936,   "2018",
  "Housing Choice Vouchers",  "Individuals",                                    10402, "2018",
  "Child Care Subsidy",       "Children",                                       2914,   "2018",
  "Health Care",              "Aid to Families with Dependent Children",        23965, "May 2019",
  "Health Care",              "Medicaid for Infants and Children",              19067, "May 2019",
  "Health Care",              "NC Health Choice",                               4064,  "May 2019"
)

# create faceted bar chart
enrolles_plot <- ggplot(numbers, aes(unit, number, fill=benefit)) +
  geom_col() +
  facet_wrap(~benefit, ncol=1,scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(title = 'Number of public benefit enrollees in Forsyth County',
       x = '',
       y = "Number of enrollees") +
  theme_minimal() +
  theme(legend.position="none")

# convert to plotly chart so we have java script
enrolles_plot_pltly <- ggplotly(enrolles_plot, tooltip = c("benefit","unit","number","date")) %>%
  config(displayModeBar = FALSE)

htmlwidgets::saveWidget(enrolles_plot_pltly, "number_benefits.html", selfcontained = F)
