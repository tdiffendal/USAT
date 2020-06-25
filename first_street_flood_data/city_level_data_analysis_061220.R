# This file examines the city-level flood data provided
# by First Street.
# This data does not contain estimates for the numbers of
# properties deemed at risk under FEMA's model. It only
# contains first street's data. 

# load libraries
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("viridis")
# install.packages("leaflet")
# install.packages("ggalt")
# install.packages("tigris")
library(tidyverse)
library(ggalt)
library(sf)
library(viridis)
library(leaflet)
library(tigris)

# Set the working directory
setwd("~/GitHub/USAT/first_street_flood_data")

# load the data
flood.cities <- read_csv("cities/City_combined.csv",
                         col_types = "ciididididcddc")

# check the structure
str(flood.cities)


# Load census data for additional attributes
census.places <- read_csv("census/nhgis0070_csv/nhgis0070_ds239_20185_2018_place.csv")

# Some of the leading zeroes got trimmed from the
# city codes in the First Street city data 
flood.cities %>%
  mutate(length = nchar(city_id)) %>%
  group_by(length) %>%
  count()
# They should all be 7 characters in length

census.places %>%
  mutate(length = nchar(PLACEA)) %>%
  group_by(length) %>%
  count()
# All city IDs are 5 characters long, plus
# the state FIPS = 7 total characters

# create a new variable with the fixed city_id
flood.cities <- flood.cities %>%
  mutate(city_id2 = case_when(nchar(city_id) == 6 ~ paste0("0", city_id),
                                            nchar(city_id) == 7 ~ city_id))

# create the city_id2 variable in the census data
census.places <- census.places %>%
  mutate(city_id2 = paste0(STATEA, PLACEA))

# Join the census and flood data to get total populations
flood.cities <- flood.cities %>%
  left_join(census.places, by = c("city_id2" = "city_id2")) %>%
  select(city:city_id2, total_pop = AJWME001)

# How many cities by state:
flood.cities %>%
  group_by(state_iso2) %>%
  count()
# 25 states, 2,821 cities

# some general stats about the size of the cities by state
flood.cities %>%
  group_by(state_iso2) %>%
  summarise(avg_props = mean(total_prop),
            median_props = median(total_prop),
            min_props = min(total_prop),
            max_props = max(total_prop))

#----------------------------
#----------------------------
# Analysis of city data
#----------------------------
#----------------------------

# Organize the city data for export and sharing
flood.cities %>%
  select(city, state = state_iso2, city_id2, total_population = total_pop, 
         total_properties = total_prop, at_risk_2020 = risk20, at_risk_2020_pct = risk20_pct, 
         at_risk_2035 = risk35, at_risk_2035_pct = risk35_pct, at_risk_2050 = risk50, 
         at_risk_2050_pct = risk50_pct) %>%
  write_csv("cities/cities_25_states_for_USAT_network_061220.csv")






