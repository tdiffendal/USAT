# This file examines the ZIP code-level flood data provided
# by First Street.

###Theresa Edits

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
library(stringr)

# Set the working directory
setwd("~/GitHub/USAT/first_street_flood_data")
 
#Create and read in zip data as received, no percentages
flood.zips <- read_csv("ZIP/Zip_level_risk_FEMA_FSF_full_final.csv",
                       col_types = "cciididididid") %>%
  clean_names()

# check the structure
str(flood.zips)

##check for nas
sapply(flood.zips, function(x) sum(is.na(x)))

##726 fs_fema_difference_2020_pct nas
# returning na bc both fema # and fs # props are 0, can't divide by 0
# change to 0s
flood.zips <- flood.zips %>%
  replace(is.na(.), 0)
# yay no more nas


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
                          AJWNE002 = col_integer(),
                          AJWNE003 = col_integer(),
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
  select(everything(), total_population = AJWME001, white_pop = AJWNE002, black_pop = AJWNE003) %>%
  clean_names() %>%
  mutate(white_pct = (white_pop*100) / total_population,
         black_pct = (black_pop*100) / total_population)

# How many ZIPs from the flood data join with the census data?

#Some of the zipcodes in the flood AND census data had leading zeroes cut off
flood.zips %>%
  mutate(length = nchar(zipcode)) %>%
  group_by(length) %>%
  count()

# Correct leading 0s issue
flood.zips <- flood.zips %>%
  mutate(zipcode = case_when(nchar(zipcode) == 4 ~ paste0("0", zipcode),
                             nchar(zipcode) == 5 ~ zipcode))

census.zips %>%
  mutate(length = nchar(zcta5a)) %>%
  group_by(length) %>%
  count()

# Correct leading 0s issue
census.zips <- census.zips %>%
  mutate(zcta5a = case_when(nchar(zcta5a) == 3 ~ paste0("00", zcta5a),
                          nchar(zcta5a) == 4 ~ paste0("0", zcta5a),
                          nchar(zcta5a) == 5 ~ zcta5a))



#### Join flood with census data

flood.zips %>%
  left_join(census.zips, by = c("zipcode" = "zcta5a"))
# Looks like all 32,752 records matched


# compare print of this length to above

# Join into new dataframe
flood.zips <- flood.zips %>%
  inner_join(census.zips, by = c("zipcode" = "zcta5a")) %>%
  select(zipcode:state, total_population, everything())
## also returns 32752 obs!

rm(census.zips)

# Count the states and make sure we have 49
flood.zips %>%
  group_by(state) %>%
  count() %>%
  arrange(desc(n))
## yes sirreeee

# get # states in data now
num_states = length(unique(flood.zips$state))

# Format and export data for the network
#flood.zips %>%
  #mutate(state = toupper(state)) %>%
  #select(zipcode, state, totalPopulation = total_population, everything()) %>%
  write_csv(flood.zips, paste0("ZIP/ZIPs_", num_states, "_states_for_USAT_network_061220.csv"))

#----------------------------
#----------------------------
# Analysis
#----------------------------
#----------------------------

#clear environment
rm(list=ls())

#load merged flood and census data
flood.zips <- read.csv("ZIP/ZIPs_49_states_for_USAT_network_061220.csv")

## home
flood.zips %>%
  filter(zipcode == '20782' | 
           zipcode == '15057') %>%
  view

# For now, I'm just going to pull out a set of
# ZIPs that have high(ish) populations and a big
# delta between FEMA and First Street flood risk
# for properties

arizona <- flood.zips %>%
  filter(state == 'az') %>%
  summarise(sum(fs_fema_difference_2020_total))

# What is a high-population ZIP for each state?
# get average, median, maximum pop for all zipcodes in each state
flood.zips %>%
  group_by(state) %>%
  summarise(n = n(),
            mean_pop = mean(total_population),
            median_pop = median(total_population),
            max_pop = max(total_population)) %>%
  print(n=50)

# high numbers of properties and high risk
state.counts <- flood.zips %>%
  group_by(state) %>%
  filter(total_properties >= 1000,
         fs_properties_at_risk_2020_pct >= 25) %>%
  count() %>%
  mutate(pct = (n*100)/1594) %>%
  arrange(desc(pct))
#returns 1594 obs

# Write the file out
flood.zips %>%
  filter(total_properties >= 1000,
         fs_properties_at_risk_2020_pct >= #25) %>%
  select(zipcode, state, everything(), -total_population) %>%
  arrange(state) %>%
  write_csv(paste0("ZIP/high_risk_high_pop_ZIPs_", num_states, "_states_061220.csv"))

# create a file with the quantiles by state I can use later to tease
# out big population ZIPs. 
#state.quants <- flood.zips %>%
#  tbl_df() %>%
#  nest(-state) %>%
#  mutate(Quantiles = map(data, ~ quantile(.$total_population)),
#         Quantiles = map(Quantiles, ~ bind_rows(.) %>% gather()))



#create temp df to mess with
temp <- flood.zips

temp %>%
  arrange(desc(fs_fema_difference_2020_pct)) %>%
  slice(1:100) %>%
  #print()
  group_by(state) %>%
  count() %>%
  arrange(desc(n)) %>%
  sum(n)
  summarise(avg_pop = mean(total_population))
  count()
### washington has 95 of the top 100 zipcodes with the greatest percentage difference betwwen FS and FEMA. but lol avg_pop is 8918. Though not far off from national average 9924
  
mean(flood.zips$total_population)


##################chicago

illinois <- flood.zips %>%
  filter(state == "il")
temp <- read.csv('ZIP/chicago_zip_codes.csv') %>%
  clean_names() %>%
  mutate(zipcode = as.character(zip))

#create list of zips for filtering
chicago_zips <- temp$zipcode
#61 zipcodes in chicago
length(chicago_zips)
#get rid of temp df
rm(temp)

## filter illinois for just chicago zips
chicago <- illinois %>%
##read in chicago zip codes
  filter(zipcode %in% chicago_zips)


#total properties: 626519
sum(chicago$total_properties)
#total population: 2773813
sum(chicago$total_population)


#fema props at risk: 1745
i = sum(chicago$fema_properties_at_risk_2020_total)
#fs props at risk: 80324
n = sum(chicago$fs_properties_at_risk_2020_total)
#chicago fs-fema total difference: 78579
print(n-i)
#chicago fs-fema pct difference: 45%
print((n-i)/i)
rm(i,n)

#how many inf values are there? or fema says 0 at risk but fs says > 0
chicago %>%
  #group_by(fs_fema_difference_2020_total) %>%
  filter(fema_properties_at_risk_2020_total == 0) %>%
  #summarise(sum(fs_fema_difference_2020_total)) #%>%
  print(n=35)

#ten worst cities by prop and stats and fs-fema
top.pop <- chicago %>%
  arrange(desc(total_population)) %>%
  slice(1:10)

top.prop <- chicago %>%
  arrange(desc(total_properties)) %>%
  slice(1:10)

top.num.risk <- chicago %>%
  arrange(desc(fs_properties_at_risk_2020_total)) %>%
  slice(1:10)

top.pct.risk <- chicago %>%
  arrange(desc(fs_properties_at_risk_2020_pct)) %>%
  slice(1:10)

#any overlaps between top pop, prop, risk?
temp <- inner_join(top.pop, top.prop, on="zipcode") %>%
  inner_join(top.num.risk, on="zipcode") %>%
  #inner_join(top.pct.risk, on="zipcode") %>%
  print()

rm(temp, top.num.risk, top.pct.riskk, top.pop, top.prop)


# higher rank = higher percentage difference
chicago %>%
  mutate(diff_rank = rank(desc(fs_fema_difference_2020_pct)))

#black neighborhoods at risk?

high_black <- chicago %>%
  arrange(desc(black_pct)) %>%
  slice(1:10)

mean(high_black$fs_fema_difference_2020_pct)

high_black$zipcode

high_black %>%
  filter(fs_fema_difference_2020_pct == Inf) %>%
  summarise(sum(fs_fema_difference_2020_total))

# returns error, but this is the idea
chicago[fs_fema_difference_2020_pct, -high_black]
