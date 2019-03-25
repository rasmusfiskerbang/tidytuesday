# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(skimr)
library(colorspace)
library(broom)
library(hrbrthemes)
library(ggfortify)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading data
games <- read_csv("board_games.csv")

# Data exploration
overview <- games %>% skim_to_wide()


# Questions ---------------------------------------------------------------

# 1. How has the average rating evolved over time?
# 2. How does rating depend on the advised playing time?
# 3. How does rating depend on minimum age?
# 4. Which game categories gets the highest ratings?
# 5. Which principal components drives the variation between the games?


# Answer 1 ----------------------------------------------------------------

games %>% mutate(decade = year_published - year_published %% 10) %>% 
  group_by(decade) %>% 
  summarise(sd = sd(average_rating),
            average_rating = mean(average_rating),
            total = n()) %>% 
  mutate(lower = average_rating-1.96*sd/sqrt(total),
         upper = average_rating+1.96*sd/sqrt(total)) %>% 
  ggplot(aes(decade, average_rating)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_point() +
  geom_line()

# Answer 2 ----------------------------------------------------------------

games %>% transmute(playing_time)

min_labels <- c("0-5 min","5-10 min","10-20 min","20-30 min","30-45 min",
                "45 min - 1 hour","1 to 2 hours","+2 hours")

games %>% mutate(playing_time = cut(playing_time, c(0,5,10,20,30,45,60,120,Inf), include.lowest = TRUE,
                                    labels = min_labels)) %>% 
  ggplot(aes(playing_time, average_rating)) + geom_boxplot()


# Answer 3 ----------------------------------------------------------------

games %>% mutate(min_age = cut(min_age, c(0,5,8,10,12,15,Inf), include.lowest = TRUE,
                               labels = c("0-5","5-8","8-10","10-12","12-15","15+"))) %>% 
  ggplot(aes(min_age, average_rating)) + geom_boxplot()
  

# Answer 4 ----------------------------------------------------------------

games_category <- games %>% select(game_id, name, average_rating, category) %>% 
  drop_na(category) %>% 
  mutate(category = str_split(category, ",")) %>% 
  unnest() %>% 
  mutate(category = fct_lump(category, 29)) %>% 
  mutate(value = TRUE, ID_before = row_number()) %>% 
  spread(category, value, fill = FALSE) %>% 
  select(-ID_before) %>% 
  group_by(game_id, name, average_rating) %>% 
  summarise_all(sum) %>% ungroup()

games_fit <- games_category %>% select(-game_id, -name) %>% 
  lm(average_rating ~ ., data = .)

bind_cols(tidy(games_fit),confint_tidy(games_fit)) %>% 
  filter(term != "(Intercept)") %>% 
  arrange(desc(estimate)) %>% 
  mutate(term = str_remove_all(term, "`"),
         term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term)) + 
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  labs(title = "Board Game Categories Influences on Average Rating",
       subtitle = "The errorbars mark 95% confidence intervals",
       x = "Regression coefficients", y = "Categories") +
  theme_ft_rc()

ggsave("regression_coefficients.png", scale = 1.2)









