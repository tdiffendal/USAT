# This file examines the ZIP code-level flood data provided
# by First Street.


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
setwd("/Users/kcrowe/Documents/climate_change/flood_data/first_street/first_street_flood_data")

# load the data
flood.zips <- read_csv("ZIP/Zipcode_combined_w_state.csv",
                       col_types = "ciidididididcc")
# check the structure
str(flood.zips)

# import the census data for ZIP codes
census.zips <- read_csv("census/nhgis0071_csv/nhgis0071_ds239_20185_2018_zcta.csv",
                        col_types = cols(
                          GISJOIN = col_character(),
                          YEAR = col_character(),
                          REGIONA = col_skip(),
                          DIVISIONA = col_skip(),
                          STATEA = col_skip(),
                          COUNTYA = col_skip(),
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
                          ZCTA5A = col_character(),
                          SUBMCDA = col_skip(),
                          SDELMA = col_skip(),
                          SDSECA = col_skip(),
                          SDUNIA = col_skip(),
                          PUMA5A = col_skip(),
                          BTTRA = col_skip(),
                          BTBGA = col_skip(),
                          NAME_E = col_character(),
                          AJWME001 = col_integer(),
                          AJWNE001 = col_skip(),
                          AJWNE002 = col_skip(),
                          AJWNE003 = col_skip(),
                          AJWNE004 = col_skip(),
                          AJWNE005 = col_skip(),
                          AJWNE006 = col_skip(),
                          AJWNE007 = col_skip(),
                          AJWNE008 = col_skip(),
                          AJWNE009 = col_skip(),
                          AJWNE010 = col_skip(),
                          AJWVE001 = col_skip(),
                          AJWVE002 = col_skip(),
                          AJWVE003 = col_skip(),
                          AJWVE004 = col_skip(),
                          AJWVE005 = col_skip(),
                          AJWVE006 = col_skip(),
                          AJWVE007 = col_skip(),
                          AJWVE008 = col_skip(),
                          AJWVE009 = col_skip(),
                          AJWVE010 = col_skip(),
                          AJWVE011 = col_skip(),
                          AJWVE012 = col_skip(),
                          AJWVE013 = col_skip(),
                          AJWVE014 = col_skip(),
                          AJWVE015 = col_skip(),
                          AJWVE016 = col_skip(),
                          AJWVE017 = col_skip(),
                          AJWVE018 = col_skip(),
                          AJWVE019 = col_skip(),
                          AJWVE020 = col_skip(),
                          AJWVE021 = col_skip(),
                          AJY4E001 = col_skip(),
                          AJY4E002 = col_skip(),
                          AJY4E003 = col_skip(),
                          AJY4E004 = col_skip(),
                          AJY4E005 = col_skip(),
                          AJY4E006 = col_skip(),
                          AJY4E007 = col_skip(),
                          AJY4E008 = col_skip(),
                          AJZAE001 = col_skip(),
                          NAME_M = col_skip(),
                          AJWMM001 = col_skip(),
                          AJWNM001 = col_skip(),
                          AJWNM002 = col_skip(),
                          AJWNM003 = col_skip(),
                          AJWNM004 = col_skip(),
                          AJWNM005 = col_skip(),
                          AJWNM006 = col_skip(),
                          AJWNM007 = col_skip(),
                          AJWNM008 = col_skip(),
                          AJWNM009 = col_skip(),
                          AJWNM010 = col_skip(),
                          AJWVM001 = col_skip(),
                          AJWVM002 = col_skip(),
                          AJWVM003 = col_skip(),
                          AJWVM004 = col_skip(),
                          AJWVM005 = col_skip(),
                          AJWVM006 = col_skip(),
                          AJWVM007 = col_skip(),
                          AJWVM008 = col_skip(),
                          AJWVM009 = col_skip(),
                          AJWVM010 = col_skip(),
                          AJWVM011 = col_skip(),
                          AJWVM012 = col_skip(),
                          AJWVM013 = col_skip(),
                          AJWVM014 = col_skip(),
                          AJWVM015 = col_skip(),
                          AJWVM016 = col_skip(),
                          AJWVM017 = col_skip(),
                          AJWVM018 = col_skip(),
                          AJWVM019 = col_skip(),
                          AJWVM020 = col_skip(),
                          AJWVM021 = col_skip(),
                          AJY4M001 = col_skip(),
                          AJY4M002 = col_skip(),
                          AJY4M003 = col_skip(),
                          AJY4M004 = col_skip(),
                          AJY4M005 = col_skip(),
                          AJY4M006 = col_skip(),
                          AJY4M007 = col_skip(),
                          AJY4M008 = col_skip(),
                          AJZAM001 = col_skip()))

# Set the column names of both tables
census.zips <- census.zips %>%
  select(everything(), total_population = AJWME001)

colnames(flood.zips) <- c("ZIPcode","totalProperties","FEMAatRisk2020","FEMAatRisk2020pct",
  "FSatRisk2020","FSatRisk2020pct","FSatRisk2035","FSatRisk2035pct",
  "FSatRisk2050","FSatRisk2050pct","FSfemaDiff","FSfemaDiffpct",
  "state", "index")

# How many ZIPs from the flood data join with the
# census data?
flood.zips %>%
  inner_join(census.zips, by = c("ZIPcode" = "ZCTA5A"))
# Looks like all 15,596 records matched

# Join into new dataframe
flood.zips <- flood.zips %>%
  inner_join(census.zips, by = c("ZIPcode" = "ZCTA5A")) %>%
  select(ZIPcode:state, total_population)

# Count the states and make sure we have 25
flood.zips %>%
  group_by(state) %>%
  count() %>%
  arrange(desc(n))

# Format and export data for the network
flood.zips %>%
  mutate(state = toupper(state)) %>%
  select(ZIPcode, state, totalPopulation = total_population, everything()) %>%
  write_csv("ZIP/ZIPs_25_states_for_USAT_network_061220.csv")

#----------------------------
#----------------------------
# Analysis
#----------------------------
#----------------------------

# For now, I'm just going to pull out a set of
# ZIPs that have high(ish) populations and a big
# delta between FEMA and First Street flood risk
# for properties

# What is a high-population ZIP for each state?
flood.zips %>%
  group_by(state) %>%
  summarise(n = n(),
            mean_pop = mean(total_population),
            median_pop = median(total_population),
            max_pop = max(total_population))

# high numbers of properties and high risk
state.counts <- flood.zips %>%
  group_by(state) %>%
  filter(totalProperties >= 1000,
         FSatRisk2020pct >= 25) %>%
  count()

# Write the file out
flood.zips %>%
  filter(totalProperties >= 1000,
         FSatRisk2020pct >= 25) %>%
  select(ZIPcode, state, everything(), -total_population) %>%
  arrange(state) %>%
  write_csv("ZIP/high_risk_high_pop_ZIPs_25_states_061220.csv") 
  
  
# create a file with the quantiles by state I can use later to tease
# out big population ZIPs. 
state.quants <- flood.zips %>%
  tbl_df() %>%
  nest(-state) %>%
  mutate(Quantiles = map(data, ~ quantile(.$total_population)),
         Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather()))










