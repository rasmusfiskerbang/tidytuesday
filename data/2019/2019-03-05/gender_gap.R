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

earnings_female <- read_csv("earnings_female.csv")
employed_gender <- read_csv("employed_gender.csv")
jobs_gender <- read_csv("jobs_gender.csv")

earnings_female %>% 
  filter(!str_detect(group, "Total")) %>% 
  ggplot(aes(Year, percent, color = group)) + geom_line() +
  geom_text(data = subset(earnings_female, Year == max(Year) & 
                           !str_detect(group, "Total")), aes(label = group),
             nudge_x = 5, check_overlap = TRUE) +
  guides(color = FALSE) +
  expand_limits(x = 2020)

