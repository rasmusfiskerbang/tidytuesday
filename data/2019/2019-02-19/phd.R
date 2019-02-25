# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading data
phd <- read_csv("phd_by_field.csv")


# Questions ---------------------------------------------------------------

# 1. Which fields has the most Ph.d in 2017?
# 2. Which major fields have experienced the largest growth?



# Answer 1 ----------------------------------------------------------------

phd %>% filter(year == max(year)) %>% 
  arrange(desc(n_phds)) %>% top_n(20, n_phds) %>% 
  mutate(field = fct_reorder(field, n_phds)) %>% 
  ggplot(aes(field, n_phds)) + geom_col() +
  coord_flip()

# Answer 2 ----------------------------------------------------------------

phd %>% group_by(broad_field, year) %>% 
  summarise(n_phds = sum(n_phds, na.rm = TRUE)) %>% 
  group_by(broad_field) %>% 
  mutate(index = n_phds/first(n_phds)-1) %>% 
  ungroup() %>% 
  mutate(broad_field = fct_reorder(broad_field, index, last)) %>% 
  ggplot(aes(year, index, color = broad_field)) + 
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.3,0.6,0.1)) +
  scale_x_continuous(breaks = 2008:2017) +
  guides(color = guide_legend(reverse = TRUE))




