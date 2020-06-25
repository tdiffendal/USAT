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

temp <- read_csv("counties/county_combined.csv") %>%
  clean_names() %>%
  mutate(fs_fema_difference_2020_pct = (fs_fema_difference_2020_total*100)/fema_properties_at_risk_2020_total) %>%
  select(location_name, 
         total_properties,
         fema_properties_at_risk_2020_total,
         fema_properties_at_risk_2020_pct,
         fs_properties_at_risk_2020_total,
         fs_properties_at_risk_2020_pct,
         fs_properties_at_risk_2035_total,
         fs_properties_at_risk_2035_pct,
         fs_properties_at_risk_2050_total,
         fs_properties_at_risk_2050_pct,
         fs_fema_difference_2020_total,
         fs_fema_difference_2020_pct,
         county_fips,
         latitude,
         longitude)

write_csv(temp, "counties/county_combined_clean.csv")

rm(temp)

################################
### clean final version
colnames(temp)
temp <- read_csv('counties/county_combined_final.csv',col_types = "cciddiididididid") %>%
  clean_names() %>%
  select(county, 
         total_properties,
         fema_properties_at_risk_2020_total,
         fema_properties_at_risk_2020_pct,
         fs_properties_at_risk_2020_total,
         fs_properties_at_risk_2020_pct,
         fs_properties_at_risk_2035_total,
         fs_properties_at_risk_2035_pct,
         fs_properties_at_risk_2050_total,
         fs_properties_at_risk_2050_pct,
         fs_fema_difference_2020_total,
         fs_fema_difference_2020_pct,
         county_fips,
         latitude,
         longitude) %>%
  write_csv("counties/county_combined_final_clean.csv")

rm(temp)

####################################

# load the data
flood.counties <- read_csv("counties/county_combined_final_clean.csv",
                           col_types = "ciidididididcdd")

# Check data types -- looks good
str(flood.counties)

# Change the column headers for the flood data
colnames(flood.counties) <- c("countyName","totalProperties","FEMAatRisk2020","FEMAatRisk2020pct",
                              "FSatRisk2020","FSatRisk2020pct","FSatRisk2035","FSatRisk2035pct",
                              "FSatRisk2050","FSatRisk2050pct","FSfemaDiff","FSfemaDiffpct",
                              "countyFIPS","latitude","longitude")

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

# Some of the FIPS codes in the flood data had leading 
# zeroes cut off
flood.counties %>%
  mutate(length = nchar(countyFIPS)) %>%
  group_by(length) %>%
  count()
# There are 217 counties that had a leading zero cut

# Create a new column "FIPS" that has the appropriate codes
flood.counties <- flood.counties %>%
  mutate(FIPS = case_when(nchar(countyFIPS) == 4 ~ paste0("0", countyFIPS),
                                nchar(countyFIPS) == 5 ~ countyFIPS))


# add state name, etc., to the flood data
flood.counties <- flood.counties %>%
  inner_join(census.county, by = c("FIPS" = "FIPS")) %>%
  select(countyName:FIPS, STATE, STATEA, COUNTY, COUNTYA, totalPop = AJWME001)
  
# Format and export the county-level data for 
# the network
#number states for naming
num_states = length(unique(flood.counties$STATE))

flood.counties %>%
  select(state = STATE, countyName, FIPS = FIPS, total_population = totalPop,
         everything()) %>%
  mutate_at(vars(FEMAatRisk2020pct, FSatRisk2020pct, FSatRisk2035pct, FSatRisk2050pct, FSfemaDiffpct), funs(round(., 1))) %>%
  as.data.frame(gsub("\\.0", '', as.matrix())) %>%
  write_csv(paste0("counties/counties_", num_states, "_states_for_USAT_network_061220.csv"))
  
  colnames(flood.counties)


#----------------------------
#----------------------------
# Get some state by state
# figures
#----------------------------
#----------------------------

# Aggregate figures by state and check against
# data Matthew Elby sent
flood.counties %>%
  group_by(STATE) %>%
  summarise(totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties))*100,1),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties))*100,1),
            FSatRisk2035 = sum(FSatRisk2035),
            FSatRisk2035pct = round((sum(FSatRisk2035)/sum(totalProperties))*100,1),
            FSatRisk2050 = sum(FSatRisk2050),
            FSatRisk2050pct = round((sum(FSatRisk2050)/sum(totalProperties))*100,1)) %>%
  arrange(desc(FSatRisk2020pct))


# Write the state-level summary stats out to a CSV:
flood.counties %>%
  group_by(STATE) %>%
  summarise(totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties)),3),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties)),3),
            FSatRisk2035 = sum(FSatRisk2035),
            FSatRisk2035pct = round((sum(FSatRisk2035)/sum(totalProperties)),3),
            FSatRisk2050 = sum(FSatRisk2050),
            FSatRisk2050pct = round((sum(FSatRisk2050)/sum(totalProperties)),3),
            Diff_FEMA_FS_props_2020 = sum(FSatRisk2020) - sum(FEMAatRisk2020),
            Diff_FEMA_FS_props_2020_pct = round((sum(FSatRisk2020) - sum(FEMAatRisk2020))/sum(FEMAatRisk2020),3)) %>%
  arrange(desc(FSatRisk2020pct)) %>%
  write_csv(paste0("summary_stats/aggregates_", num_states, "_states_risk_percentages_061120.csv"))


# Make a dumbbell plot of the state totals
flood.counties %>%
  group_by(state = STATE) %>%
  summarise(totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties))*100,1),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties))*100,1)) %>%
  arrange(FSatRisk2020pct) %>%
  mutate(state_label = factor(state, unique(state))) %>%
  ggplot(aes(x = FEMAatRisk2020pct, xend = FSatRisk2020pct, y = state_label)) +
  geom_dumbbell(color = "#a3c4dc", size_x = 3.5, size_xend = 3.5, colour_x="#edae52", colour_xend = "#9fb059") +
  geom_text(color="black", size=3, hjust= 1.5,
            aes(x=FEMAatRisk2020pct, label=paste0(FEMAatRisk2020pct,"%"))) +
  geom_text(aes(x=FSatRisk2020pct, label= paste0(FSatRisk2020pct,"%")), 
            color="black", size=3, hjust=-0.35) +
  labs(x= "Percentage of properties at risk", y=NULL, 
       title="FEMA Risk vs First Street Risk", 
       subtitle="Percentage of properties at 1% annual risk of flooding in each state\nunder FEMA's model (yellow) and First Street's model (green)",
       colour = "Estimates") +
  theme_gray()


# What are the total figures for far for the 25 states?
national.totals <- flood.counties %>%
  summarise(totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties)),3),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties)),3),
            FSatRisk2035 = sum(FSatRisk2035),
            FSatRisk2035pct = round((sum(FSatRisk2035)/sum(totalProperties)),3),
            FSatRisk2050 = sum(FSatRisk2050),
            FSatRisk2050pct = round((sum(FSatRisk2050)/sum(totalProperties)),3),
            Diff_FEMA_FS_props_2020 = sum(FSatRisk2020) - sum(FEMAatRisk2020),
            Diff_FEMA_FS_props_2020_pct = round((sum(FSatRisk2020) - sum(FEMAatRisk2020))/sum(FEMAatRisk2020),3)) 

# State totals
state.totals <- flood.counties %>%
  group_by(STATE) %>%
  summarise(totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties)),3),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties)),3),
            FSatRisk2035 = sum(FSatRisk2035),
            FSatRisk2035pct = round((sum(FSatRisk2035)/sum(totalProperties)),3),
            FSatRisk2050 = sum(FSatRisk2050),
            FSatRisk2050pct = round((sum(FSatRisk2050)/sum(totalProperties)),3),
            Diff_FEMA_FS_props_2020 = sum(FSatRisk2020) - sum(FEMAatRisk2020),
            Diff_FEMA_FS_props_2020_pct = round((sum(FSatRisk2020) - sum(FEMAatRisk2020))/sum(FEMAatRisk2020),3))
  



#----------------------------
#----------------------------
# Analysis of Counties
#----------------------------
#----------------------------
# Since we only have 25 states right now, I'm going
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
  filter(totalPop >= 500000) %>%
  summarise(n = n(),
            totalProperties = sum(totalProperties),
            FEMAatRisk2020 = sum(FEMAatRisk2020),
            FEMAatRisk2020pct = round((sum(FEMAatRisk2020)/sum(totalProperties)),3),
            FSatRisk2020 = sum(FSatRisk2020),
            FSatRisk2020pct = round((sum(FSatRisk2020)/sum(totalProperties)),3),
            FSatRisk2035 = sum(FSatRisk2035),
            FSatRisk2035pct = round((sum(FSatRisk2035)/sum(totalProperties)),3),
            FSatRisk2050 = sum(FSatRisk2050),
            FSatRisk2050pct = round((sum(FSatRisk2050)/sum(totalProperties)),3),
            Diff_FEMA_FS_props_2020 = sum(FSatRisk2020) - sum(FEMAatRisk2020),
            Diff_FEMA_FS_props_2020_pct = round((sum(FSatRisk2020) - sum(FEMAatRisk2020))/sum(FEMAatRisk2020),3))
# 39 largest counties
# FEMA: 295,677 (2%) properties at risk
# FS 2020: 792,042 (5.4%) properties at risk

#----------------------------
#----------------------------
# Mapping the county data
#----------------------------
#----------------------------

# I haven't done anything with mapping this stuff just yet. 

# Set options and load data
options(tigris_class = "sf")






















#----------------------------
#----------------------------
# Analysis of ZIP code data
#----------------------------
#----------------------------

# load the data:
flood.zips <- read_csv("/Users/kcrowe/Documents/climate_change/flood_data/first_street/data/ZIP/zip3.csv",
                       col_types = "ciidididididd")

# Double check the data types
sapply(flood.zips, class)

# create new column names
colnames(flood.zips) <- c("zipCode", "totalProperties", "FEMAatRisk2020", "FEMAatRisk2020pct", "FSatRisk2020", 
                           "FSatRisk2020pct", "FSatRisk2035", "FSatRisk2035pct", "FSatRisk2050", "FSatRisk2050pct", 
                           "FSfemaDiff", "FSfemaDiffpct", "index")

colnames(flood.zips)
















