###############################################################################
#
# This file calculates the total income (income plus benefits) for single benefits
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
  # don't include pre-k subsidies, medical, and smart start
  filter(!(benefit %in% c("WIC", "Housing Choice Voucher", "Smart Start", "Work First (TANF)")))

# clean data -------------------------------------------------------------------

# we want to calculate total benefits and income for all unique benefits
# create seperate datasets for each unique benefit
unique_benefit <- map(unique(benefits$benefit),
                      function(x) filter(benefits, benefit == !! x))

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
total_benefits <- map2(unique_benefit,
                       unique(benefits$benefit),
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
  mutate(net_income = round(aftertax_income + payment, 2),
         # add hourly pay
         hourly = monthly_income / (40*4.35),
         # if it's two adults, divide hourly by two to represent both working
         hourly = ifelse(str_detect(composition,"2 adults"), hourly / 2, hourly)) %>%
  select(-eitc, -payment)

# map string of households to number representing household size

composition <- c("1 adult", "1 adult, 1 child", "1 adult, 2 children", "1 adult, 3 children",
                 "2 adults", "2 adults, 1 child", "2 adults, 2 children", "2 adults, 3 children")

size <- c(1, 2, 3, 4, 2, 3, 4, 5)

size_mapping <- data.frame(composition = composition,
                           size = size)

master <- master %>%
  left_join(size_mapping, by = "composition")

# add benefit thresholds -------------------

# 85% SMI thresholds for child care
smi <- c(`1` = 2826,
         `2` = 3695,
         `3` = 4565,
         `4` = 5435,
         `5` = 6304,
         `6` = 7174)

# federal poverty guidelines for FNS and medicaid
fpg <- read_rds('benefits_tables/tables/federal_poverty_guidelines.rds')

# fns is 130% and NC Health Choice is 210%
create_fpg_mapping <- function(threshold) {
  
  fpg %>%
    mutate(guidelines_month = guidelines_month * threshold) %>%
    filter(year == 2019) %>%
    select(household_size, guidelines_month) %>%
    rename(size = household_size)
  
  }

fns_threshold <- create_fpg_mapping(1.3)
hc_threshold <- create_fpg_mapping(2.1)

# benefits we are using
unique(master$benefit)

# these families don't get child care or health choice, regardless of income
no_benefit <- c("1 adult", "2 adults")

# child care

child_care <- master %>%
  filter(benefit == "Child Care Subsidy") %>%
  mutate(threshold = recode(size, !!! smi),
         get_benefit = ifelse((monthly_income > threshold) | (composition %in% no_benefit), 
                              FALSE, TRUE))

# fns
fns <- master %>%
  filter(benefit == "FNS (Food Stamps)") %>%
  left_join(fns_threshold, by = "size") %>%
  mutate(get_benefit = ifelse(monthly_income > guidelines_month, FALSE, TRUE))

# NC Child Care
health_choice <- master %>%
  filter(benefit == "NC Medicaid / Health Choice") %>%
  left_join(hc_threshold, by = "size") %>%
  mutate(get_benefit = ifelse((monthly_income > guidelines_month) | (composition %in% no_benefit), 
                       FALSE, TRUE))
