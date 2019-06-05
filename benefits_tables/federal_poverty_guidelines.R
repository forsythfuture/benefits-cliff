################################################################################
#
# create federal poverty guidelines
#
###############################################################################

library(tidyverse)

# federal poverty guidelines for 2018 and 2019 ---------------------------

fpg <- data.frame(household_size = rep(seq_len(8), times = 2),
                    guidelines_year = c(12490, 16910, 21330, 25750, 30170, 34590, 39010, 43430,
                                       12140, 16460, 20780, 25100, 29420, 33740, 38060, 42380),
                    year = rep(c(2019, 2018), each=8)) %>%
    # add montly guidelines
    mutate(guidelines_month = round(guidelines_year / 12,0)) %>%
    select(household_size, year, everything())


write_rds(fpg, 'benefits_tables/tables/federal_poverty_guidelines.rds')
