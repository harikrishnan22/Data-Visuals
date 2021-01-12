# Library
library(tidyr)
library(dplyr)
library(magrittr)

# Question 1
# Import Data
crime <- read.csv(file = "c:/Computation and Vizualization/Assignment 5/BostonCrimeData.csv", sep = ",", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Filtering, Grouping & Summarizing
crime_boston <- crime %>%
  filter(DISTRICT != "External") %>%
  group_by(DISTRICT, MONTH) %>%
  summarise(CrimeEvents = n()) %>%
  drop_na()

# Export Data
write.csv(crime_df, "Boston_Crime_Data.csv")

# Question 2
# Import Data
mobility <- read.csv(file = "c:/Computation and Vizualization/Assignment 5/Global_Mobility_Report.csv", sep = ",", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Filtering, Grouping & Summarizing
mobility_us <- mobility %>%
  filter(country_region == "United States") %>%
  select(-c(1, 3, 4, 5, 6, 7)) %>%
  group_by(country_region, date) %>%
  summarise_at(vars(1, 2, 3, 4, 5, 6), mean, na.rm = TRUE)

# Export Data
write.csv(mobility_us, "Mobility_US.csv")

# Question 3
# Import Data
agri_emp <- read.csv(file = "c:/Computation and Vizualization/Assignment 5/Agriculture_Employment.csv", sep = ",", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Filtering, Grouping & Summarizing
agriculture <- agri_emp %>% select(-c(2, 3, 4))

# Export Data
write.csv(agriculture, "agriculture_employment.csv")

# Question 4
# Import Data
economy <- read.csv(file = "c:/Computation and Vizualization/Assignment 5/Employment Combined - National - Daily.csv", sep = ",", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Filtering, Grouping & Summarizing
economy$Date <- as.Date(paste(economy$year, economy$month, economy$day, sep = "-"))
economy_us <- economy %>% select(-c(1, 2, 3))

# Export Data
write.csv(economy_us, "Economy.csv")
