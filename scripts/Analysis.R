#### Preamble ####
# Purpose: Proform analysis on the ACS data
# Author: David Qi
# Date: 3 October 2024
# Contact: david.qi@mail.utoronto.ca
# License: MIT
# Pre-requisites: ACS data is in data/raw/usa_00001.csv
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### read data ####
data <- read.csv("data/raw/usa_00001.csv")

estimate <- data %>%
  filter(EDUCD == 116) %>%
  count(STATEICP)

estimate = estimate %>%
  mutate(estimated_respondents = floor( n * 391171/6336))

actural_n <- data %>%
  count(STATEICP)%>%
  rename(actural_respondents = n)

merged_df <- merge(estimate, actural_n, by = "STATEICP")

merged_df <- merged_df %>% mutate(error = estimated_respondents - actural_respondents)
merged_df <- merged_df %>% mutate(percent_error = error/actural_respondents)

# Create the STATENAME column using case_when()
merged_df  <- merged_df  %>%
  mutate(STATENAME = case_when(
    STATEICP == 01 ~ "Connecticut",
    STATEICP == 02 ~ "Maine",
    STATEICP == 03 ~ "Massachusetts",
    STATEICP == 04 ~ "New Hampshire",
    STATEICP == 05 ~ "Rhode Island",
    STATEICP == 06 ~ "Vermont",
    STATEICP == 11 ~ "Delaware",
    STATEICP == 12 ~ "New Jersey",
    STATEICP == 13 ~ "New York",
    STATEICP == 14 ~ "Pennsylvania",
    STATEICP == 21 ~ "Illinois",
    STATEICP == 22 ~ "Indiana",
    STATEICP == 23 ~ "Michigan",
    STATEICP == 24 ~ "Ohio",
    STATEICP == 25 ~ "Wisconsin",
    STATEICP == 31 ~ "Iowa",
    STATEICP == 32 ~ "Kansas",
    STATEICP == 33 ~ "Minnesota",
    STATEICP == 34 ~ "Missouri",
    STATEICP == 35 ~ "Nebraska",
    STATEICP == 36 ~ "North Dakota",
    STATEICP == 37 ~ "South Dakota",
    STATEICP == 40 ~ "Virginia",
    STATEICP == 41 ~ "Alabama",
    STATEICP == 42 ~ "Arkansas",
    STATEICP == 43 ~ "Florida",
    STATEICP == 44 ~ "Georgia",
    STATEICP == 45 ~ "Louisiana",
    STATEICP == 46 ~ "Mississippi",
    STATEICP == 47 ~ "North Carolina",
    STATEICP == 48 ~ "South Carolina",
    STATEICP == 49 ~ "Texas",
    STATEICP == 51 ~ "Kentucky",
    STATEICP == 52 ~ "Maryland",
    STATEICP == 53 ~ "Oklahoma",
    STATEICP == 54 ~ "Tennessee",
    STATEICP == 56 ~ "West Virginia",
    STATEICP == 61 ~ "Arizona",
    STATEICP == 62 ~ "Colorado",
    STATEICP == 63 ~ "Idaho",
    STATEICP == 64 ~ "Montana",
    STATEICP == 65 ~ "Nevada",
    STATEICP == 66 ~ "New Mexico",
    STATEICP == 67 ~ "Utah",
    STATEICP == 68 ~ "Wyoming",
    STATEICP == 71 ~ "California",
    STATEICP == 72 ~ "Oregon",
    STATEICP == 73 ~ "Washington",
    STATEICP == 81 ~ "Alaska",
    STATEICP == 82 ~ "Hawaii",
    STATEICP == 83 ~ "Puerto Rico",
    STATEICP == 96 ~ "State groupings (1980 Urban/rural sample)",
    STATEICP == 97 ~ "Military/Mil. Reservations",
    STATEICP == 98 ~ "District of Columbia",
    STATEICP == 99 ~ "State not identified",
    TRUE ~ "Unknown"  # Default case
  ))

#### compute mean and variance error
print(mean(merged_df$error))
print(sd(merged_df$error))
print(mean(merged_df$percent_error))

#### Write_csv
write_csv(merged_df, file = "data/analysis/estimate.csv")