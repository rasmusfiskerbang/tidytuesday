library(tidyverse)
library(broom)

prison <- read_csv("prison_population.csv")
pretrial <- read_csv("pretrial_population.csv")

# Cleaning data -----------------------------------------------------------

# Creating state names
prison <- prison %>% 
  left_join(data.frame(state = state.abb, state_name = state.name)) %>% 
  mutate(state = state_name) %>% select(-state_name)

# Sorting
prison <- prison %>% 
  arrange(county_name, year)

# removing NAs
prison <- prison %>% 
  drop_na()

# Those that are in prison are also part of the population
prison <- prison %>% 
  mutate(population = population + prison_population)

# Splitting into gender and race
prison_gender <- prison %>% 
  filter(pop_category %in% c("Male","Female"))

prison_race <- prison %>% 
  filter(!pop_category %in% c("Total","Male","Female"))

# Aggregation function
agg_function <- function(agg_var){
  agg_var <- enquo(agg_var)
  
  prison_race %>% group_by(year, !!agg_var, pop_category) %>% 
    summarise(population = sum(population),
              prison_population = sum(prison_population)) %>% 
    group_by(year, !!agg_var) %>% 
    mutate(population_rel = population/sum(population),
           prison_population_rel = prison_population/sum(prison_population)) 
}

large_change <- agg_function(state) %>% 
  filter(pop_category == "Black") %>% 
  group_by(state) %>% nest() %>% 
  mutate(fit = map(data, ~ lm(prison_population_rel ~ year, data = .)),
         coef = map(fit, tidy)) %>% 
  unnest(coef) %>% 
  filter(term == "year") %>% 
  arrange(p.value) %>% head(9) %>% pull(state)
  
agg_function(state) %>% 
  filter(pop_category == "Black",
         state %in% large_change) %>% 
  ggplot(aes(year, prison_population_rel, color = state)) +
  geom_line()
  





