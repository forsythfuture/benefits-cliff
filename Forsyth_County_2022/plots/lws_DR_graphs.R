
library(tidyverse)
library(FFplot)

benefits <- read_csv('~/benefits-cliff/Forsyth_County_2022/plots/data/benefits.csv')

benefits_wage <- benefits %>% 
  mutate(hourly_wage = monthly_income / (40 * 4.35)) %>% 
  # only displaying 3 person HH, 1 adult 2 children
  filter(composition == "1 adult, 2 children") %>% 
  mutate(benefit = case_when(
    benefit == "NC Child Care Subsidy / Smart Start" ~ "NC Child Care Subsidy", 
    TRUE ~ benefit
  ))

# create vectors for the benefit, benefit hourly wage terimination, benefit payment loss due to termination
benefits_filter <- c("FNS (Food Stamps)", "NC Child Care Subsidy", "Housing Choice Voucher", "WIC")
benefits_hourly_wage <- c(20.44,20.44,18.85,18.91)
benefits_payment_loss <- c(106.36,'1,249.40',0,126.84)

# function to create benefit plot
benefits_plot <- function(benefit, wage, loss){
  
  p <- benefits_wage %>% 
    filter(benefit == {{benefit}}) %>% 
    ggplot((aes(x = hourly_wage, y = payment))) +
    geom_line(color = "#004c89", size = 1.5) +
    scale_x_continuous(labels = scales::dollar_format(prefix = "$"), breaks = seq(0, 40,5)) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
    labs(y = "Benefit Payment", 
         x = "Full-Time Hourly Wage",
         title = "Benefit Payment by Full-Time Hourly Wage in a 1 Adult, 2 Children\nUnder 5 Years Old Household",
         subtitle = glue::glue("After reaching an hourly wage of ${wage}, the recipient experiences the cliff and\nloses ${loss} worth of monthly benefits."),
         caption = "The amount of benefit loss varies based on household size.") +
    ff_style() +
    theme(plot.caption = element_text(size = 9))
  
  # left align text
  ggpubr::ggarrange(left_align(p, c("subtitle", "title", "caption")))
    
}

# output plots for the four benefit programs
final_plots <- pmap(list(benefits_filter, benefits_hourly_wage, benefits_payment_loss), ~ benefits_plot(..1,..2,..3))
