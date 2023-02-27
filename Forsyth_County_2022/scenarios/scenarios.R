######################################################################################################

# This script creates mock situations where the benefits cliff affects different types of households.
# The three situations are modeled after the 'How might the Benefits Cliff affect Buncombe County residents?' 
# stories found at https://www.justeconomicswnc.org/buncombe-benefits/. The benefits and the household
# types are the same in the Forsyth County analysis below.

######################################################################################################

library(tidyverse)

# read in benefit payments by household composition and income
dat <- readr::read_rds('~/benefits-cliff/Forsyth_County_2022/plots/data/benefits.rds')

# types of benefits used in the scenarios
benefits <- c('NC Child Care Subsidy / Smart Start', rep('FNS (Food Stamps)', 2))
# household compositions used in the scenarios
household <- c('1 adult, 3 children', '1 adult', '2 adults, 3 children')

map2(household, benefits, ~ dat %>% 
       # filter by benefit and household composition when payments are zero 
       filter(benefit == .y,
              composition == .x,
              payment > 0) %>% 
       # select the last row before benefit payment amount goes to zero
       tail(1) %>% 
       # calculate hourly wage (monthly income divided by 4.35 weeks divided by 40 hours)
        # same calculation used in benefits cliff repo created by Shane
       mutate(hourly_wage = (monthly_income / 4.35) / 40 )
     ) %>% 
  reduce(bind_rows)

# monthly incomes we will use based on our findings above
  # these monthly amounts equate to whole number hourly wages
monthly_income <- c(4872, 5046, 2088, 2262, 5742, 5916)
# types of benefits used in the scenarios
benefits2 <- c(rep('NC Child Care Subsidy / Smart Start', 2), rep('FNS (Food Stamps)', 4))
# household compositions used in the scenarios
household2 <- c(rep('1 adult, 3 children', 2), rep('1 adult', 2), rep('2 adults, 3 children', 2))

pmap(list(household2, benefits2, monthly_income), ~ dat %>% 
       # filter by benefit, household composition, and monthly income
       filter(benefit == ..2,
              composition == ..1,
              monthly_income == ..3) %>% 
       # calculate hourly wage (monthly income divided by 4.35 weeks divided by 40 hours)
       # same calculation used in benefits cliff repo created by Shane
       mutate(hourly_wage = round((monthly_income / 4.35) / 40 ))) %>% 
  reduce(bind_rows) %>% 
  rowwise() %>% 
  # add monthly income and benefit payments together to get a combined income
  mutate(income_and_benefits = round(sum(monthly_income, payment))) %>% 
  ungroup() %>% 
  group_by(composition, benefit) %>% 
  # find the net loss, or gain, when income increases and benefits decrease
  mutate(net_loss = lead(income_and_benefits) - income_and_benefits) %>% 
  ungroup() %>% 
  # reformat net loss column for better clarity
  mutate(net_loss = ifelse(is.na(net_loss), "", net_loss),
         payment = round(payment)) %>% 
  select(`Household Composition` = composition, `Benefit Program` = benefit, `Monthly Income` = monthly_income, 
         `Hourly Wage` = hourly_wage, `Benefit Payment` = payment, `Combined Income and Benefit Amount ` = income_and_benefits, 
         `Net Loss` = net_loss) %>% 
  write_csv('~/benefits-cliff/Forsyth_County_2022/scenarios/scenarios_forsyth_2023.csv')
