library(tidyverse)

income <- read_rds("tax_liability/income_diff.rds") %>%
  ungroup()

calc_scenerios <- function(input_df, family_comp, benefit) {
  
  df <- input_df %>%
    # filter for family composition
    filter(composition == !! family_comp,
           # filter for benefit
           category %in% c("After-tax income", !! benefit)) %>%
    select(-composition, -hourly, -diff) %>%
    # convert to wide form where after tax income and benefit are on different columns
    pivot_wider(names_from = "category")
  
  colnames(df) <- c("pretax_inc", "aftertax_inc", "benefit")
  
  df <- df %>%
    mutate(total_inc = aftertax_inc + benefit,
           hourly = round(pretax_inc / (40*4.35),  2),
           hourly_two = round(hourly / 2, 2))
  
  # find row where benefits end and subtract one
  # these are the rows we want to keep
  benefits_end <- which(df$benefit == 0)[1] - 1
  df <- df[benefits_end:nrow(df),]
  
  # filter out rows that have higher total incomes than the initial total income plus benefits
  beg_income <- df[[1, "total_inc"]]
  df <- df[df$total_inc <= beg_income,]
  
  return(df)
}

fns <- calc_scenerios(income, "1 adult, 3 children", "FNS (Food Stamps)")
