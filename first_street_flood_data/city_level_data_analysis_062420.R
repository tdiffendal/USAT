# This file examines the city-level flood data provided
# by First Street.
# This data does not contain estimates for the numbers of
# properties deemed at risk under FEMA's model. It only
# contains first street's data

## Kevin Crowe
## edits by Theresa Diffendal

library(tidyverse)
library(ggalt)
library(sf)
library(viridis)
library(leaflet)
library(tigris)
library(janitor)

# Set the working directory
setwd("~/GitHub/USAT/first_street_flood_data")

#load data to be cleaned
flood.cities <- read_csv("cities/City_level_risk_FEMA_FSF_full.csv", col_types = "cciidididididicdd") %>%
  `colnames<-` (c("city",
           "state",
           "total_properties",
           "fema_risk",
           "pct_fema_risk",
           "risk_2020",
           "pct_risk_2020",
           "risk_2035",
           "pct_risk_2035",
           "risk_2050",
           "pct_risk_2050",
           "risk_change_20_50",
           "pct_risk_change_20_50",
           "fs_fema_risk_deviation_2020",
           "city_id",
           "lat",
           "long")) %>%
  mutate(fs_fema_risk_deviation_2020_pct = (fs_fema_risk_deviation_2020 * 100) / fema_risk)


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

# create a new variable with the fixed city_id
flood.cities <- flood.cities %>%
  mutate(city_id = case_when(nchar(city_id) == 6 ~ paste0("0", city_id),
                                            nchar(city_id) == 7 ~ city_id))


census.places %>%
  mutate(length = nchar(PLACEA)) %>%
  group_by(length) %>%
  count()
# All city IDs are 5 characters long, plus
# the state FIPS = 7 total characters

# create the city_id variable in the census data
census.places <- census.places %>%
  mutate(city_id = paste0(STATEA, PLACEA))

# Join the census and flood data to get total populations
flood.cities <- flood.cities %>%
  left_join(census.places, by = c("city_id" = "city_id")) %>%
  select(city:state, city_id, total_population = AJWME001, total_properties:fs_fema_risk_deviation_2020,fs_fema_risk_deviation_2020_pct, lat, long)

rm(census.places)

# How many cities by state:
flood.cities %>%
  group_by(state) %>%
  count() 
# 49 states, 28,160 cities (# rows)


# some general stats about the size of the cities by state
flood.cities %>%
  group_by(state) %>%
  summarise(avg_props = mean(total_properties),
            median_props = median(total_properties),
            min_props = min(total_properties),
            max_props = max(total_properties))

#----------------------------
#----------------------------
# Analysis of city data
#----------------------------
#----------------------------

#turn nas to 0
flood.cities[is.na(flood.cities)] <- 0

#get number of states for naming
num_states = length(unique(flood.cities$state))

#write csv
write_csv(flood.cities, paste0("cities/cities_", num_states, "_states_for_USAT_network_061220.csv"))

flood.cities <- read.csv("cities/cities_49_states_for_USAT_network_061220.csv")


#### pull out stats for cities we want to look at
interest <- flood.cities %>%
  filter(city == 'Chicago' |
           (city == 'Los Angeles' &
           state == 'ca') |
  city == 'New York' |
  city == 'Rand' |
  (city == 'Charleston' &
         state == 'wv') |
  city == 'Fort Myers' |
  (city == 'Jacksonville' &
         state == 'fl') |
  city == 'New Orleans') %>%
  arrange(city)

##### FS' arizona numbers low?

arizona <- flood.cities %>% 
  filter(state=='az')

#### biggest share according to Fema

femaest <- flood.cities %>%
  filter(total_properties >= 100000) %>%
  arrange(desc(pct_fema_risk)) %>%
  count()

######################
## biggest states by pop

biggestPop <- flood.cities %>%
  arrange(desc(total_population)) %>%
  slice(1:10)

biggestProp <- flood.cities %>%
  arrange(desc(total_properties)) %>%
  slice(1:10)

### biggest differences between fema and fs 2020 -- checking for Rand, WV
biggestDif <- flood.cities %>%
  mutate(pct_diff_2020 = pct_risk_2020 - pct_fema_risk ) %>%
  #filter(pct_diff_2020) %>%
  arrange(desc(pct_diff_2020)) %>%
  slice(1:50)

## find by what percent the total # of houses at risk per fs is bigger/smaller than fema total#
print(
  (sum(biggest$fs_fema_risk_deviation_2020) 
   * 100) 
  / sum(biggest$fema_risk)
)
# answer: 155.2198%

# Organize the city data for export and sharing
#flood.cities %>%
  #select(city, state, city_id2, total_population = total_pop,       total_properties, at_risk_2020 = risk20, at_risk_2020_pct = risk20_pct, at_risk_2035 = risk35, at_risk_2035_pct = risk35_pct, at_risk_2050 = risk50, at_risk_2050_pct = risk50_pct) %>%
  






