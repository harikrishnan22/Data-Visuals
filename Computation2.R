library(tidyr)
library(dplyr)
library(magrittr)

migrant <- read.csv("Migrant_Data_US.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "")

colnames(migrant)[1] <- "Year"

migrant_df <- migrant %>% pivot_longer(-c(Year, Destination), names_to = "Origin", values_to = "Migrants") %>% drop_na()

geo <- read.csv("Geo_Location.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "")

colnames(geo)[1] <- "Country_ID"

location_code <- geo[,c(1,4)]

migrant_final <- inner_join(migrant_df, location_code, c("Origin" = "name"))

colnames(migrant_final)[5] <- "Origin_ID"

migrant_final <- inner_join(migrant_final, location_code, c("Destination" = "name"))

colnames(migrant_final)[6] <- "Destination_ID"

write.csv(migrant_final, "Migrants_Flourish.csv")

write.csv(geo, "Locations_Flourish.csv")

