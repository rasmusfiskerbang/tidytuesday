# Setup -------------------------------------------------------------------

# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(skimr)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading data
whales_raw <- read_csv("allCetaceanData.csv")

# Cleaning ----------------------------------------------------------------

whales <- whales_raw %>% 
  # Removing X1 column
  select(-X1) %>% 
  # creating NA's
  mutate(sex = na_if(sex, "U"),
         acquisition = na_if(acquisition, "Unknown"),
         currently = na_if(currently, "Unknown"),
         status = na_if(status, "Realeased?")) %>% 
  # Removing unrecognized charcters in notes
  mutate(notes = str_remove_all(notes,"�"))

# EDA ---------------------------------------------------------------------


# Questions ---------------------------------------------------------------

# What kind of species do we have?

# Answer 1 ----------------------------------------------------------------

whales %>% count(species, sort = TRUE) %>% View()




  





