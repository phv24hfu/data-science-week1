# string manipulation using stringr

# Fix 1: cleaning string names

# Cleaning strings with dplyr
# finding the names in site that need cleaned
mosquito_egg_data <- read_csv(here("data", "mosquito_egg_data.csv"))
mosquito_egg_data |>  
  distinct(site)

# cleaning the names in site
mosquito_egg_data <- mosquito_egg_data |> 
  mutate(site = case_when(
    site == "site_a" ~ "Site_A",
    site == "Site-A" ~ "Site_A",
    site == "Site A" ~ "Site_A",
    site == "site_b" ~ "Site_B",
    site == "Site-B" ~ "Site_B",
    site == "Site B" ~ "Site_B",
    site == "site_c" ~ "Site_C",
    site == "Site-C" ~ "Site_C",
    site == "Site C" ~ "Site_C",
    .default = as.character(site)
  )
  )
#checking correct changes were made
mosquito_egg_data |>  
  distinct(site)

# doing the same for the collector name
mosquito_egg_data |>  
  distinct(collector)

mosquito_egg_data <- mosquito_egg_data |> 
  mutate(collector = case_when(
    collector == "Garci" ~ "Garcia",
    collector == "Smyth" ~ "Smith",
    .default = as.character(collector)
  )
  )
# checking correct changes were made
mosquito_egg_data |>  
  distinct(collector)

# doing the same for treatment
mosquito_egg_data |>  
  distinct(treatment)

mosquito_egg_data <- mosquito_egg_data |> 
  mutate(treatment = case_when(
    treatment == "High_dose" ~ "High",
    treatment == "high_dose" ~ "High",
    treatment == "HIGH_DOSE" ~ "High",
    treatment == "Medium_dose" ~ "Medium",
    treatment == "MEDIUM_DOSE" ~ "Medium",
    treatment == "medium_dose" ~ "Medium",
    treatment == "Low_dose" ~ "Low",
    treatment == "low_dose" ~ "Low",
    treatment == "LOW_DOSE" ~ "Low",
    treatment == "control" ~ "Control",
    treatment == "CONTROL" ~ "Control",
    .default = as.character(treatment)
  )
  )
# checking correct changes were made
mosquito_egg_data |>  
  distinct(treatment)

# What changed and why it matters.

# The variables in each section now have similar names so that it is easier to read and be easier to complete test on when called apon



# Fix 2: checking for duplicate rows
library(tidyverse)
# check for whole duplicate 
# rows in the data
mosquito_egg_data <- mosquito_egg_data |> 
  filter(!duplicated(across(everything())))

mosquito_egg_data |> 
  summarise(
    n = n(),
    n_distinct(female_id)
  ) 

# What changed and why it matters.

# I made the data set contain no duplicate entries. This means that the data will not show repetative results or bias towards one result.


# Fix 3: Getting rid of NA values

# finding all NA values
mosquito_egg_data |> 
  filter(if_any(everything(), is.na))

# Getting rid of NA values
mosquito_egg_data <- mosquito_egg_data |>
  drop_na()

# verifying the change

mosquito_egg_data |> 
  filter(if_any(everything(), is.na))


# What changed and why it matters.

# All NA values were dropped. This means that when testing the data, the NA values will not be involved.

# Creating a new dataset name with clean values

mosquito_egg_clean <- mosquito_egg_data
