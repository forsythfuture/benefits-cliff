
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


income_2019_test3 <- incomes %>%
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
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # the y axis of the plot should reflect number of people, not number of households,
  # so, multiply HHWT by size to convert number of households to number of people
  mutate(total = size * HHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above")) %>%
  ungroup() %>%
  select(size, HHINCOME, HHWT, total, FNS_threshold, indicator)

#totals by household size
totals_by_household_size <- income_2019_test3 %>%
  group_by(size, indicator) %>%
  summarise(sum = sum(total))

#totals overall
totals_overall <- income_2019_test3 %>%
  group_by(indicator) %>%
  summarise(sum = sum(total))

income_2019_test5 <- incomes %>%
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
  select(SERIAL, HHWT, HHINCOME, size, FOODSTMP) %>%
  distinct() %>%
  # now group by size and income
  # we're grouping by income because we will sum household weights by income
  # so they are aggregated and we don't have multiple rows of the same income
  group_by(size, HHINCOME, FOODSTMP) %>%
  summarize(HHWT = sum(HHWT)) %>%
  arrange(size, HHINCOME, FOODSTMP) %>%
  # remove household incomes less than 72000, which is a monthly income of 6000
  # this is the amount we use for our other charts
  filter(HHINCOME <= 72000) %>%
  # the y axis of the plot should reflect number of people, not number of households,
  # so, multiply HHWT by size to convert number of households to number of people
  mutate(total = size * HHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above"),
         food_stamps = if_else(FOODSTMP == 2, "On Food Stamps", "Not on Food Stamps")) %>%
  ungroup() %>%
  select(size, food_stamps, HHINCOME, total, FNS_threshold, indicator)

#totals by household size
totals_by_food_stamps<- income_2019_test5 %>%
  group_by(food_stamps) %>% 
  summarise(sum = sum(total))


boo <- testing_test3 %>%
  filter(size == 3)

boo2 <- income_2019_test3 %>%
  filter(size == 3)

sum(boo$officialHHWT)
sum(boo2$HHWT)

#4463845

testing_test <- incomes %>%
  mutate(race_ethnicity = if_else(between(HISPAN,1,4), "Hispanic/Latino", 
                                  if_else((HISPAN == 0 | HISPAN == 9) & RACE == 1, "White, NH", 
                                          if_else((HISPAN == 0 | HISPAN == 9) & RACE == 2, "Black/AA, NH", 
                                                  if_else((HISPAN == 0 | HISPAN == 9) & (RACE != 1 | RACE != 2), "Other Race", NA_character_))))) %>%
  filter(HHINCOME >= 0) %>%
  mutate(in_school = ifelse(SCHOOL == 2, TRUE, FALSE)) %>%
  group_by(SERIAL) %>%
  mutate(size = n(), # household size
         total_school= sum(in_school), # total number in school
  ) %>%
  filter(size != total_school, # remove households where all people are in school
         size <= 5, # only keep households with 5 or fewer people, for plotting
  ) %>%
  select(SERIAL, size, HHINCOME, HHWT, race_ethnicity)

myFun <- function(vector, thresh) {
  ind <- ave(rep(1, length(vector)), vector, FUN = length)
  vector[ind > thresh]
}

bang <- myFun(incomes$SERIAL, 1) %>% unique()

testing_test3 <- testing_test %>%
  mutate(newHHWT = if_else(SERIAL %in% bang, HHWT / size, HHWT)) %>%
  group_by(size, HHINCOME, race_ethnicity) %>%
  summarize(officialHHWT = sum(newHHWT)) %>%
  arrange(size, HHINCOME, race_ethnicity) %>%
  filter(HHINCOME <= 72000) %>%
  mutate(total = size * officialHHWT) %>%
  ungroup() %>%
  group_by(size) %>%
  # group by size and add column with corresponding FNS thresholds
  # then add a column stating whether the HHINCOME is below or above the threshold
  mutate(FNS_threshold = case_when(size == 1 ~ 17040,
                                   size == 2 ~ 21599,
                                   size == 3 ~ 27159,
                                   size == 4 ~ 32718,
                                   size == 5 ~ 38277,
                                   TRUE ~ 0),
         indicator = if_else(HHINCOME < FNS_threshold, "Below", "Above")) %>%
  ungroup() %>%
  select(size, race_ethnicity, HHINCOME, officialHHWT, total, FNS_threshold, indicator)
  

totals_by_race_ethnicity <- testing_test3 %>%
    group_by(race_ethnicity, indicator) %>% 
    # filter(race_ethnicity != "Other Race") %>%
    summarise(sum = sum(total))
  
  