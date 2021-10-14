
# read in data from google drive
test <- read_csv("G://Shared drives/Forsyth Futures/Forsyth Futures Projects/JE_210001 Buncombe County Benefits Cliff Microsite/usa_00001.csv.gz")

# filter for 5 year 2017 ACS data by state and county --- matches Shane's data
test_2017 <- test %>%
  filter(STATEFIP == 37,
         COUNTYFIP == 21,
         YEAR == 2017,
         MULTYEAR %in% 2013:2017)

# filter for 5 year 2019 ACS data by state and county
test_2019 <- test %>%
  filter(STATEFIP == 37,
         COUNTYFIP == 21,
         YEAR == 2019,
         MULTYEAR %in% 2015:2019)

income_2019 <- incomes %>%
  # remove household incomes less than 0
  # we are interested in low-income / low net-worth people, and such people are unlikely
  # to have negative income
  filter(HHINCOME >= 0) %>%
  # create boolean of whether person is in school
  # we'll later remove families where everyone is in school,
  # because such families would generally not be entitled to benefits
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  # group by household
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, HHWT, HHINCOME, size) %>%
  distinct() %>%
  # now group by size and income
  # we're grouping by income because we will sum household weights by income
  # so they are aggregated and we don't have multiple rows of the same income
  group_by(size, HHINCOME) %>%
  summarize(HHWT = sum(HHWT)) %>%
  arrange(size, HHINCOME) %>%
  # create cumulative sum for incomes by using the weight column
  mutate(cum_sum = cumsum(HHWT),
         perc_sum = round(percent_rank(cum_sum), 2)) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # add column that is the same thing with all values, so that the nested d3 plot works
  # this column is irrelevant, but lets use recycle the d3 code from the other plots
  mutate(grouping = "group",
         # the y axis of the plot should reflect number of people, not number of households,
         # so, multiply cum_sum by size to convert number of households to number of people
         cum_sum = size * cum_sum) %>%
  ungroup() %>%
  select(size, income = HHINCOME, cum_sum, grouping)

write_csv(income, "Buncombe_County_2020/plots/cliff_cdf.csv")

fns_ <- income_2019 %>%
  group_by(size) %>%
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(income < FNS_threshold, "Below", "Above")) %>%
  group_by(size, indicator) %>%
  summarise(total = sum(cum_sum))
















