#####################################################
#
# create dataset excel outputs for creating cumulative sum plot
# the dataset will show the number of people at each income amount
#
# run cliff_population_2019.R script before running these scripts
#
######################################################

#HH size of 1
cliff_pop_size1 <- test_2019 %>% 
  filter(size == 1) %>%
  select(income, cum_sum) %>%
  mutate(output = paste0("[",income,",",cum_sum,"],")) %>%
  select(output) %>%
  writexl::write_xlsx("C:\\Users\\daniel ludolf\\Documents\\benefits-cliff\\Buncombe_County_2020\\income_cliff\\cliff_pop_size1.xlsx")

#HH size of 2
cliff_pop_size2 <- test_2019 %>% 
  filter(size == 2) %>%
  select(income, cum_sum) %>%
  mutate(output = paste0("[",income,",",cum_sum,"],")) %>%
  select(output) %>%
  writexl::write_xlsx("C:\\Users\\daniel ludolf\\Documents\\benefits-cliff\\Buncombe_County_2020\\income_cliff\\cliff_pop_size2.xlsx")

#HH size of 3
cliff_pop_size3 <- test_2019 %>% 
  filter(size == 3) %>%
  select(income, cum_sum) %>%
  mutate(output = paste0("[",income,",",cum_sum,"],")) %>%
  select(output) %>%
  writexl::write_xlsx("C:\\Users\\daniel ludolf\\Documents\\benefits-cliff\\Buncombe_County_2020\\income_cliff\\cliff_pop_size3.xlsx")

#HH size of 4
cliff_pop_size4 <- test_2019 %>% 
  filter(size == 4) %>%
  select(income, cum_sum) %>%
  mutate(output = paste0("[",income,",",cum_sum,"],")) %>%
  select(output) %>%
  writexl::write_xlsx("C:\\Users\\daniel ludolf\\Documents\\benefits-cliff\\Buncombe_County_2020\\income_cliff\\cliff_pop_size4.xlsx")

#HH size of 5
cliff_pop_size5 <- test_2019 %>% 
  filter(size == 5) %>%
  select(income, cum_sum) %>%
  mutate(output = paste0("[",income,",",cum_sum,"],")) %>%
  select(output) %>%
  writexl::write_xlsx("C:\\Users\\daniel ludolf\\Documents\\benefits-cliff\\Buncombe_County_2020\\income_cliff\\cliff_pop_size5.xlsx")


