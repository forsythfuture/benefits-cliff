################################################################################
#
# create federal poverty guidelines
#
###############################################################################

library(tidyverse)

# federal poverty guidelines for 2021, 2022, 2023 ---------------------------

# 2023: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
# 2022: https://aspe.hhs.gov/sites/default/files/documents/4b515876c4674466423975826ac57583/Guidelines-2022.pdf
# 2021: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines

fpg <- data.frame(household_size = rep(seq_len(8), times = 3),
                  guidelines_year = c(14580, 19720, 24860, 30000, 35140, 40280, 45420, 50560,
                                      13590, 18310, 23030, 27750, 32470, 37190, 41910, 46630,
                                      12880, 17420, 21960, 26500, 31040, 35580, 40120, 44660),
                  year = rep(c(2023, 2022, 2021), each = 8)) %>%
  # add montly guidelines
  mutate(guidelines_month = round(guidelines_year / 12, 0)) %>%
  select(household_size, year, everything())

write_rds(fpg, 'Buncombe_County_2020/benefits_tables/tables/federal_poverty_guidelines.rds')
