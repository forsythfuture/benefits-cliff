
# calculate a benefit terminations table for the Benefit Terminations subsection for the Buncombe County microsite

# read in federal poverty guidelines
fpl <- read_rds('Buncombe_County_2020/benefits_tables/tables/federal_poverty_guidelines.rds')

# table displaying for each benefit at what monthly income threshold the benefit terminates 
benefit_terminations <- fpl %>% 
  # use the most up-to-date FPG
  filter(year == 2023) %>% 
  mutate(FNS = guidelines_month * 2,
         Work_First = c(362, 472, 544, 594, 648, 698, 746, 772),
         WIC = guidelines_month * 1.85,
         Child_Care = guidelines_month * 2,
         Smart_Start = guidelines_month * 2,
         MAF = c(NA, 569, 667, 744, 824, NA, NA, NA),
         MIF_Under6 = guidelines_month * 2.1,
         MIF_6_18 = guidelines_month * 1.33,
         Health_Choice = guidelines_month * 2.11) %>% 
  select(-guidelines_month, - guidelines_year, -year) %>% 
  mutate(across(everything(), round))

# these benefit programs do not apply for households of size 1
benefit_terminations[1, 4:10] <- NA
  
write_csv(benefit_terminations, '~/benefits-cliff/Buncombe_County_2020/benefits_tables/benefit_terminations.csv')
