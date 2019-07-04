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
  ~Benefit,                   ~Unit,                                           ~Number, ~`Date`,
  #------------------------------------------------------------------------------------------------
  "Work First (TANF)",        "Households",                                     334,   "May 2019",
  "Work First (TANF)",        "Individuals",                                    559,   "May 2019",
  "Food and Nutrition Service","Households",                                     25079, "July 2017",
  "Food and Nutrition Service","Individuals",                                    46392, "July 2017",
  "Housing Choice Vouchers",  "Households",                                     3936,   "2018",
  "Housing Choice Vouchers",  "Individuals",                                    10402, "2018",
  "Child Care Subsidy",       "Children",                                       2914,   "2018",
  "Health Care",              "Aid to Families with Dependent Children",        23965, "May 2019",
  "Health Care",              "Medicaid for Infants and Children",              19067, "May 2019",
  "Health Care",              "NC Health Choice",                               4064,  "May 2019"
)

ggplot(numbers, aes(Unit, Number, fill=Benefit)) +
  geom_col() +
  geom_text(
    aes(label = Number),
    vjust = .5,
    hjust = -.1,
    size = 3.3,
    alpha = .8
  ) +
  facet_wrap(~Benefit, ncol=1,scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(title = 'Number of public benefit enrollees in Forsyth County',
       x = '',
       y = "Number of enrollees"#,
       # caption = "Sources:\n
       # Work First: https://www.ncdhhs.gov/divisions/social-services/program-statistics-and-reviews/work-first-caseload-statistics\n
       # FNS:  https://www.fns.usda.gov/pd/supplemental-nutrition-assistance-program-snap\n
       # Housing Choice Voucher:  http://www.haws.org/index.php/housing/87-housing-choice-voucher-section-8-housing/116-housing-choice-voucher-section-8-program-eligibility\n
       # Child Care:  https://ncchildcare.ncdhhs.gov/Home/DCDEE-Sections/Subsidy-Services/Fact-Sheets\n
       # Health Care:  https://medicaid.ncdhhs.gov/documents/reports/enrollment-reports/medicaid-and-health-choice-enrollment-reports\n
       # "
       ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.caption = element_text(hjust=0))

ggsave(filename="number_enrollees.svg", path = "plots",
       width = 12, height = 12, units = "in")
# ggplotly(benefit_plot, tooltip = c("Number", "Date"))%>%
#   config(displayModeBar = FALSE)

# create function that is a single plotly bar chart
# we will then create plots for each benefit, and plot as subplts
# num_benefit_plots <- function(df) {
#   
#   plot_ly(df, x=~number, y=~unit, type="bar", color = ~benefit,
#           hoverinfo = 'text',
#           text = ~paste("</br>", unit, ": ", number,
#                         "</br>Current as of: ",date)) %>%
#     layout(annotations = list(text = unique(df$benefit),
#                               x = 0.05, y = 1, showarrow = F, xref='paper', yref='paper',
#                               font = list(size = 20)),
#            xaxis = list(title = "Number of enrollees"),
#            yaxis = list(title = "")) %>% 
#     config(displayModeBar = F)
#   
# }
# 
# unique_benefits <- unique(numbers$benefit)
# 
# # create unique plots for each benefit type
# separate_plots <- map(unique_benefits, 
#                       function(x) num_benefit_plots(numbers[numbers$benefit == x,]))
# 
# separate_plots %>%
#   subplot(nrows = length(unique_benefits), shareX = TRUE)
# 
# small_num <- numbers[numbers$benefit == "Work First (TANF)",]


# convert to plotly chart so we have java script
# enrolles_plot_pltly <- ggplotly(enrolles_plot, tooltip = c("benefit","unit","number","date")) %>%
#   config(displayModeBar = FALSE) unique(small_num$benefit)
# 
# htmlwidgets::saveWidget(enrolles_plot_pltly, "number_benefits.html", selfcontained = F)
