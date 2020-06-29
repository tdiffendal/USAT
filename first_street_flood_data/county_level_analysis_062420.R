# This is an analysis of county-level data from First Street
# At this point, we have 25 states

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
library(janitor)

# Set the working directory
setwd("~/GitHub/USAT/first_street_flood_data")

#### read in county data file
flood.counties <- read_csv("counties/County_level_risk_FEMA_FSF_updated_062320.csv", col_types = "cccddiididididid") %>%
  clean_names() %>%
  select(county, state,
         total_properties,
         fema_2020 = fema_properties_at_risk_2020_total,
         fema_2020_pct = fema_properties_at_risk_2020_pct,
         fs_2020 = fs_properties_at_risk_2020_total,
         fs_2020_pct = fs_properties_at_risk_2020_pct,
         fs_2035 = fs_properties_at_risk_2035_total,
         fs_2035_pct = fs_properties_at_risk_2035_pct,
         fs_2050 = fs_properties_at_risk_2050_total,
         fs_2050_pct = fs_properties_at_risk_2050_pct,
         fs_fema_diff = fs_fema_difference_2020_total,
         fs_fema_diff_pct = fs_fema_difference_2020_pct,
         county_fips,
         latitude,
         longitude)

# Check data types -- looks good
str(flood.counties)


# Load the census data that has population, household income,
# race and poverty. 
census.county <- read_csv("census/nhgis0068_csv/nhgis0068_ds239_20185_2018_county.csv",
                          col_types = cols(
                     GISJOIN = col_character(),
                     YEAR = col_character(),
                     REGIONA = col_character(),
                     DIVISIONA = col_character(),
                     STATE = col_character(),
                     STATEA = col_character(),
                     COUNTY = col_character(),
                     COUNTYA = col_character(),
                     COUSUBA = col_skip(),
                     PLACEA = col_skip(),
                     TRACTA = col_skip(),
                     BLKGRPA = col_skip(),
                     CONCITA = col_skip(),
                     AIANHHA = col_skip(),
                     RES_ONLYA = col_skip(),
                     TRUSTA = col_skip(),
                     AITSCEA = col_skip(),
                     ANRCA = col_skip(),
                     CBSAA = col_skip(),
                     CSAA = col_skip(),
                     METDIVA = col_skip(),
                     NECTAA = col_skip(),
                     CNECTAA = col_skip(),
                     NECTADIVA = col_skip(),
                     UAA = col_skip(),
                     CDCURRA = col_skip(),
                     SLDUA = col_skip(),
                     SLDLA = col_skip(),
                     ZCTA5A = col_skip(),
                     SUBMCDA = col_skip(),
                     SDELMA = col_skip(),
                     SDSECA = col_skip(),
                     SDUNIA = col_skip(),
                     PUMA5A = col_skip(),
                     BTTRA = col_skip(),
                     BTBGA = col_skip(),
                     NAME_E = col_character(),
                     AJWME001 = col_integer(),
                     AJWNE001 = col_integer(),
                     AJWNE002 = col_integer(),
                     AJWNE003 = col_integer(),
                     AJWNE004 = col_integer(),
                     AJWNE005 = col_integer(),
                     AJWNE006 = col_integer(),
                     AJWNE007 = col_integer(),
                     AJWNE008 = col_integer(),
                     AJWNE009 = col_integer(),
                     AJWNE010 = col_integer(),
                     AJWVE001 = col_integer(),
                     AJWVE002 = col_integer(),
                     AJWVE003 = col_integer(),
                     AJWVE004 = col_integer(),
                     AJWVE005 = col_integer(),
                     AJWVE006 = col_integer(),
                     AJWVE007 = col_integer(),
                     AJWVE008 = col_integer(),
                     AJWVE009 = col_integer(),
                     AJWVE010 = col_integer(),
                     AJWVE011 = col_integer(),
                     AJWVE012 = col_integer(),
                     AJWVE013 = col_integer(),
                     AJWVE014 = col_integer(),
                     AJWVE015 = col_integer(),
                     AJWVE016 = col_integer(),
                     AJWVE017 = col_integer(),
                     AJWVE018 = col_integer(),
                     AJWVE019 = col_integer(),
                     AJWVE020 = col_integer(),
                     AJWVE021 = col_integer(),
                     AJY4E001 = col_integer(),
                     AJY4E002 = col_integer(),
                     AJY4E003 = col_integer(),
                     AJY4E004 = col_integer(),
                     AJY4E005 = col_integer(),
                     AJY4E006 = col_integer(),
                     AJY4E007 = col_integer(),
                     AJY4E008 = col_integer(),
                     AJZAE001 = col_integer(),
                     NAME_M = col_character(),
                     AJWMM001 = col_integer(),
                     AJWNM001 = col_integer(),
                     AJWNM002 = col_integer(),
                     AJWNM003 = col_integer(),
                     AJWNM004 = col_integer(),
                     AJWNM005 = col_integer(),
                     AJWNM006 = col_integer(),
                     AJWNM007 = col_integer(),
                     AJWNM008 = col_integer(),
                     AJWNM009 = col_integer(),
                     AJWNM010 = col_integer(),
                     AJWVM001 = col_integer(),
                     AJWVM002 = col_integer(),
                     AJWVM003 = col_integer(),
                     AJWVM004 = col_integer(),
                     AJWVM005 = col_integer(),
                     AJWVM006 = col_integer(),
                     AJWVM007 = col_integer(),
                     AJWVM008 = col_integer(),
                     AJWVM009 = col_integer(),
                     AJWVM010 = col_integer(),
                     AJWVM011 = col_integer(),
                     AJWVM012 = col_integer(),
                     AJWVM013 = col_integer(),
                     AJWVM014 = col_integer(),
                     AJWVM015 = col_integer(),
                     AJWVM016 = col_integer(),
                     AJWVM017 = col_integer(),
                     AJWVM018 = col_integer(),
                     AJWVM019 = col_integer(),
                     AJWVM020 = col_integer(),
                     AJWVM021 = col_integer(),
                     AJY4M001 = col_integer(),
                     AJY4M002 = col_integer(),
                     AJY4M003 = col_integer(),
                     AJY4M004 = col_integer(),
                     AJY4M005 = col_integer(),
                     AJY4M006 = col_integer(),
                     AJY4M007 = col_integer(),
                     AJY4M008 = col_integer(),
                     AJZAM001 = col_integer()))

# Create a FIPS column in the census data
census.county <- census.county %>%
  mutate(FIPS = paste0(STATEA, COUNTYA)) 

census.county$FIPS <- as.character(census.county$FIPS)

# Some of the FIPS codes in the flood data had leading 
# zeroes cut off
flood.counties %>%
  mutate(length = nchar(county_fips)) %>%
  group_by(length) %>%
  count()
# There are 275 counties that had a leading zero cut

# Create a new column "FIPS" that has the appropriate codes
flood.counties <- flood.counties %>%
  mutate(fips = case_when(nchar(county_fips) == 4 ~ paste0("0", county_fips),
                                nchar(county_fips) == 5 ~ county_fips))


# add state name, etc., to the flood data
flood.counties <- flood.counties %>%
  inner_join(census.county, by = c("fips" = "FIPS")) %>%
  select(county:state, total_population = AJWME001, total_properties:fips, state_name=STATE, STATEA, COUNTYA)
  
# Format and export the county-level data for 
# the network
#number states for naming
num_states = length(unique(flood.counties$state_name))

#flood.counties %>%
#  select(state = state_name, #countyName, FIPS = FIPS, #total_population = #total_population,
#         everything()) %>%
#  mutate_at(vars(fema_2020_pct, fs_2020_pct, fs_2035_pct, fs_2050_pct, FSfemaDiffpct), funs(round(., 1))) %>%
#  as.data.frame(gsub("\\.0", '', as.matrix())) %>%
  write_csv(flood.counties, paste0("counties/counties_", num_states, "_clean_062520.csv"))


#----------------------------
#----------------------------
# Get some state by state
# figures
#----------------------------
#----------------------------
  
## read in clean data
flood.counties <- read.csv(paste0("counties/counties_", num_states, "_clean_062520.csv"))

# Aggregate figures by state and check against
# data Matthew Elby sent

state.totals <- flood.counties %>%
    group_by(state_name) %>%
    summarise(total_properties = sum(total_properties),
              fema_2020 = sum(fema_2020),
              fema_2020_pct = round((sum(fema_2020)/sum(total_properties)),3),
              fs_2020 = sum(fs_2020),
              fs_2020_pct = round((sum(fs_2020)/sum(total_properties)),3),
              fs_2035 = sum(fs_2035),
              fs_2035_pct = round((sum(fs_2035)/sum(total_properties)),3),
              fs_2050 = sum(fs_2050),
              fs_2050_pct = round((sum(fs_2050)/sum(total_properties)),3),
              fs_fema_diff = sum(fs_2020) - sum(fema_2020),
              fs_fema_diff_pct = round((sum(fs_2020) - sum(fema_2020))/sum(fema_2020),3))
  
  #number of states where at least 1 in 10 props at risk
  state.totals %>%
    filter(fs_2050_pct >= .1) %>%
    count()


# Write the state-level summary stats out to a CSV:
state.totals %>%
  arrange(desc(fs_2020_pct)) %>%
  write_csv(paste0("summary_stats/aggregates_", num_states, "_states_risk_percentages_062520.csv"))


# Make a dumbbell plot of the state totals
flood.counties %>%
  group_by(state = state_name) %>%
  summarise(total_properties = sum(total_properties),
            fema_2020 = sum(fema_2020),
            fema_2020_pct = round((sum(fema_2020)/sum(total_properties))*100,1),
            fs_2020 = sum(fs_2020),
            fs_2020_pct = round((sum(fs_2020)/sum(total_properties))*100,1)) %>%
  arrange(fs_2020_pct) %>%
  mutate(state_label = factor(state, unique(state))) %>%
  ggplot(aes(x = fema_2020_pct, xend = fs_2020_pct, y = state_label)) +
  geom_dumbbell(color = "#a3c4dc", size_x = 3.5, size_xend = 3.5, colour_x="#edae52", colour_xend = "#9fb059") +
  geom_text(color="black", size=3, hjust= 1.5,
            aes(x=fema_2020_pct, label=paste0(fema_2020_pct,"%"))) +
  geom_text(aes(x=fs_2020_pct, label= paste0(fs_2020_pct,"%")), 
            color="black", size=3, hjust=-0.35) +
  labs(x= "Percentage of properties at risk", y=NULL, 
       title="FEMA Risk vs First Street Risk", 
       subtitle="Percentage of properties at 1% annual risk of flooding in each state\nunder FEMA's model (yellow) and First Street's model (green)",
       colour = "Estimates") +
  theme_gray()


# What are the total figures so far for the 49 states?
national.totals <- flood.counties %>%
  summarise(total_properties = sum(total_properties),
            fema_2020 = sum(fema_2020),
            fema_2020_pct = round((sum(fema_2020)/sum(total_properties)),3),
            fs_2020 = sum(fs_2020),
            fs_2020_pct = round((sum(fs_2020)/sum(total_properties)),3),
            fs_2035 = sum(fs_2035),
            fs_2035_pct = round((sum(fs_2035)/sum(total_properties)),3),
            fs_2050 = sum(fs_2050),
            fs_2050_pct = round((sum(fs_2050)/sum(total_properties)),3),
            fs_fema_diff = sum(fs_2020) - sum(fema_2020),
            fs_fema_diff_pct = round((sum(fs_2020) - sum(fema_2020))/sum(fema_2020),3)) 

#----------------------------
#----------------------------
# Analysis of Counties
#----------------------------
#----------------------------

# tease out the most populous places that show big
# increases in percentages of properties at risk.

# Some general stats about U.S. Counties
census.county %>%
  summarise(n = n(),
            median_pop = median(AJWME001),
            avg_pop = mean(AJWME001))

# How many counties with pops > 100,000
census.county %>%
  filter(AJWME001 >= 100000) %>%
  count()
# 599 counties 

census.county %>%
  filter(AJWME001 >= 250000) %>%
  count()
# 269 counties

census.county %>%
  filter(AJWME001 >= 500000) %>%
  count()
# 138 counties

# What does the flood risk look like in
# the largest counties in the U.S.?
flood.counties %>%
  filter(total_population >= 500000) %>%
  summarise(n = n(),
            total_properties = sum(total_properties),
            fema_2020 = sum(fema_2020),
            fema_2020_pct = round((sum(fema_2020)/sum(total_properties)),3),
            fs_2020 = sum(fs_2020),
            fs_2020_pct = round((sum(fs_2020)/sum(total_properties)),3),
            fs_2035 = sum(fs_2035),
            fs_2035_pct = round((sum(fs_2035)/sum(total_properties)),3),
            fs_2050 = sum(fs_2050),
            fs_2050_pct = round((sum(fs_2050)/sum(total_properties)),3),
            fs_fema_diff = sum(fs_2020) - sum(fema_2020),
            fs_fema_diff_pct = round((sum(fs_2020) - sum(fema_2020))/sum(fema_2020),3),
            total_population = sum(total_population)) %>%
  view()

# 39 largest counties
# FEMA: 295,677 (2%) properties at risk
# FS 2020: 792,042 (5.4%) properties at risk

#in how many counties are FS numbers greater than FEMA
flood.counties %>%
  filter(fs_2020 > fema_2020) %>%
  summarise(n=n())
# 2586, or 85%

#----------------------------
#----------------------------
# Mapping the county data
#----------------------------
#----------------------------

# I haven't done anything with mapping this stuff just yet. 

# Set options and load data
options(tigris_class = "sf")

















