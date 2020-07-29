# This is an analysis of household pulse survey data
# from the U.S. Census
# Source: https://www.census.gov/programs-surveys/household-pulse-survey/data.html

# Load libraries
library(readxl)
library(httr)
library(tidyverse)
library(janitor)
library(ggalt)
library(scales)
#library(hrbrthemes)

# Grab the excel file from the census website
# Currently working on Week 10: https://www.census.gov/data/tables/2020/demo/hhp/hhp10.html#techdoc
url.string = "https://www2.census.gov/programs-surveys/demo/tables/hhp/2020/wk10/employ1_week10.xlsx"

# Store that in a temp file
GET(url.string, write_disk(employ1.tf <- tempfile(fileext = ".xlsx")))

# Read in the sheets from the employment table 1
sheets.list <- excel_sheets(employ1.tf)

# This loop goes through each sheet and
# pulls in the data, tosses it into a dataframe,
# changes the row names, adds variables for area and
# characteristics, 
# then adds that dataframe to a list of dataframes
# we can work with

#Create the empty list to store the dataframes
emp.df.list <- list()

for(i in 1:length(sheets.list)){
  
  x = sheets.list[i]
  
  x.emp <- read_excel(employ1.tf,
                      sheet = x, 
                      range = cell_limits(c(4, 1), c(70, 8)))
  # Change the row names
  emp.row.names = c("characteristic", "total", "loss_income_since_march_YES","loss_income_since_march_NO",
                    "loss_income_since_march_DNR", "expect_loss_income_YES", "expect_loss_income_NO",
                    "expect_loss_income_DNR")
  colnames(x.emp) <- emp.row.names
  # Create the characteristic_type field
  x.emp <- x.emp %>%
    mutate(characteristic_type = case_when(is.na(total) ~ characteristic))
  # Copy that value down through the field
  x.emp <- fill(x.emp, characteristic_type)
  
  # Add the geography
  x.emp <- x.emp %>%
    mutate(area = x)
  
  x.emp <- x.emp %>%
    filter(!is.na(total)) %>%
    select(area, characteristic, characteristic_type, everything())
  
  # Add the df to the list
  emp.df.list[i] = list(x.emp)
}

# Check some of the files
emp.df.list[3]

# Combine the dataframes in the list
employ1.df <-  do.call(rbind, emp.df.list)

# Check the structure of that dataframe
# Most of the numeric data is formatted
# as character right now
str(employ1.df)

# Convert character data to numeric where
# we need to
employ1.df <- employ1.df %>%
  mutate(loss_income_since_march_YES = as.numeric(loss_income_since_march_YES),
         loss_income_since_march_NO = as.numeric(loss_income_since_march_NO),
         loss_income_since_march_DNR = as.numeric(loss_income_since_march_DNR),
         expect_loss_income_YES = as.numeric(expect_loss_income_YES),
         expect_loss_income_NO = as.numeric(expect_loss_income_NO),
         expect_loss_income_DNR = as.numeric(expect_loss_income_DNR))

# Check the structure
str(employ1.df)

# Check the state total figures and see if they 
# add up to the national totals
us.totals.check <- employ1.df %>%
  filter(nchar(area) == 2, area != "US", characteristic_type == "Hispanic origin and Race") %>%
  group_by(characteristic) %>%
  summarise(total = sum(total),
            loss_income_since_march_YES = sum(loss_income_since_march_YES, na.rm = TRUE),
            loss_income_since_march_NO = sum(loss_income_since_march_NO, na.rm = TRUE),
            loss_income_since_march_DNR = sum(loss_income_since_march_DNR, na.rm = TRUE),
            expect_loss_income_YES  = sum(expect_loss_income_YES, na.rm = TRUE),
            expect_loss_income_NO = sum(expect_loss_income_NO, na.rm = TRUE),
            expect_loss_income_DNR = sum(expect_loss_income_DNR, na.rm = TRUE))
# Some of the totals are off by between 1 and 4 people. 
# I don't think we need to be concerned about that too much. 


# Create some new variables to get the percentages
# of responses in each category
employ1.df <- employ1.df %>%
  mutate(loss_income_since_march_YES_PCT = round((loss_income_since_march_YES/total*100),1),
         loss_income_since_march_NO_PCT = round((loss_income_since_march_NO/total*100),1),
         loss_income_since_march_DNR_PCT = round((loss_income_since_march_DNR/total*100),1),
         expect_loss_income_YES_PCT = round((expect_loss_income_YES/total*100),1),
         expect_loss_income_NO_PCT = round((expect_loss_income_NO/total*100),1),
         expect_loss_income_DNR_PCT = round((expect_loss_income_DNR/total*100),1)) %>%
  mutate(state_name = state.name[match(area, state.abb)])
# Write the processed file out to CSV and RDS
write_csv(employ1.df, "Employment_table_1_week_10_071720.csv")
write_rds(employ1.df, "Employment_table_1_week_10_071720.RDS")

#-----------------------
#-----------------------
# Analysis and viz
#-----------------------
#-----------------------

# Questions to answer:
# - What are some of the gaps along racial lines in terms of
# who has lost income?
# - Show the gaps between white and black and white and hispanic
#   by state.
# - Show the same gaps in some of the metro areas.

###### Black - white expecting income loss graph

white<-employ1.df %>%
  filter(nchar(area) == 2, area != "US", 
         characteristic_type == "Hispanic origin and Race") %>%
  select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
  spread(key = characteristic, value = expect_loss_income_YES_PCT) %>%
  select(state_name, "White alone, not Hispanic") %>%
  clean_names()

temp<-employ1.df %>%
  filter(nchar(area) == 2, area != "US", 
         characteristic_type == "Hispanic origin and Race") %>%
  select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
  #spread(key = characteristic, value = expect_loss_income_YES_PCT) %>%
  filter(characteristic != "White alone, not Hispanic") %>%
  left_join(white, by="state_name")

## replace NAs with missing DC
temp[("state_name")][is.na(temp[("state_name")])] <- "District of Columbia"

#make these characters numeric
num.cols <- c("expect_loss_income_YES_PCT","white_alone_not_hispanic")
temp[num.cols] <- sapply(temp[num.cols], as.numeric)


# actually plot this beast
ggplot(temp,
       aes(x=expect_loss_income_YES_PCT,
           xend=white_alone_not_hispanic,
           y=reorder(state_name, +white_alone_not_hispanic))) +
  geom_dumbbell(color = "gray45",
                size_x = 3.5,
                size_xend = 3.5,
                colour_x="#edae52",
                colour_xend = "#9fb059") +
  geom_text(color="black",
            size=3,
            aes(x=expect_loss_income_YES_PCT,
                label=paste0(expect_loss_income_YES_PCT,"%")),
            hjust=ifelse(as.double(temp$expect_loss_income_YES_PCT) >= 
                           as.double(temp$white_alone_not_hispanic),
                         -0.25, 1.3)) +
  geom_text(color="black",
            size=3,
            aes(x=white_alone_not_hispanic,
                label=paste0(white_alone_not_hispanic,"%")),
            hjust=ifelse(as.double(temp$white_alone_not_hispanic) <= 
                           as.double(temp$expect_loss_income_YES_PCT),
                         1.3, -0.25)) +
  labs(title = 'Racial Differences in Expected Future Income Loss',
       subtitle = "Percentage of racial/ethnic respondents (yellow) expecting income loss \nin the next month compared to percentage of white respondents (green)",
       x = "Percent Expecting Income Loss Next Month",
       y = NULL) +
  theme(axis.text.y=element_text(size = 10, margin = margin(t=.5,r=5,b=0,l=0)),
        axis.title.x=element_text(margin(t=5,r=0,b=0,l=0))) +
  theme_gray(base_size = 16) +
  scale_x_continuous(limits = c(0,100), breaks=c(20,40,60,80,100)) +
  facet_wrap(~characteristic, nrow=1)


ggsave("race_vs_white_expect_income_loss.png", width=22, height=17, units="in", dpi=600)#+
  #theme(panel.grid.major = element_line(colour = "white"))


## scatter plot bc sometimes I feel like a nut

avg <- employ1.df %>%
  filter(nchar(area) == 2, area != "US", 
         characteristic_type == "Hispanic origin and Race") %>%
  select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
  clean_names() %>%
  group_by(characteristic) %>%
  summarise(mean = mean(expect_loss_income_yes_pct, na.rm=TRUE))
            
            
ggplot(employ1.df %>%
  filter(nchar(area) == 2, area != "US", 
         characteristic_type == "Hispanic origin and Race") %>%
  select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
  clean_names(),
  (aes(x=state_name,
       y=expect_loss_income_yes_pct,
       colour = characteristic))) +
  geom_point() +
  geom_segment(aes(x=-0.4,
                   xend=state_name+0.4,
                   y=mean,
                   yend=mean),
               data=avg,
               colour="red") +
  labs(title = 'Racial Differences in Expected Future Income Loss',
       subtitle = "Percentage of racial/ethnic respondents (yellow) expecting income loss \nin the next month compared to percentage of white respondents (green)",
       y = "Percent Expecting Income Loss Next Month",
       x = NULL,
       colour = "Race/Ethnicity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0,100), breaks=c(20,40,60,80,100))
  geom_line(aes(x=state_name, y=mean, colour=characteristic))


  geom_smooth(aes(characteristic), method="lm", se=F)




#### worked

temp<-employ1.df %>%
  filter(nchar(area) == 2, area != "US", 
         characteristic_type == "Hispanic origin and Race",
         total >= 50000) %>%
  select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
  spread(key = characteristic, value = expect_loss_income_YES_PCT) %>%
  clean_names() %>%
  na.omit()

ggplot(temp, 
       aes(x=black_alone_not_hispanic, 
           xend=white_alone_not_hispanic, 
           y=reorder(state_name, +black_alone_not_hispanic), 
           yend=reorder(state_name, +white_alone_not_hispanic))) +
  geom_dumbbell(color = "#a3c4dc", 
                size_x = 3.5, 
                size_xend = 3.5, 
                colour_x="#edae52", 
                colour_xend = "#9fb059") +
  geom_text(color="black", 
            size=3, 
            aes(x=black_alone_not_hispanic, 
                label=paste0(black_alone_not_hispanic,"%")),
            hjust=ifelse(as.double(temp$black_alone_not_hispanic) >= 
                           as.double(temp$white_alone_not_hispanic),
                         -0.35, 1.3)) +
  geom_text(color="black", 
            size=3, 
            aes(x=white_alone_not_hispanic, 
                label=paste0(white_alone_not_hispanic,"%")),
            hjust=ifelse(as.double(temp$white_alone_not_hispanic) <= 
                           as.double(temp$black_alone_not_hispanic),
                         1.3, -0.35)) +
  labs(title = 'Racial Differences in Expected Future Income',
           subtitle = "Percentage of Black respondents (yellow) expecting income loss in the next month compared \nto percentage of white respondents (green) in states with 50,000 or more Black respondents",
           x = "Percent Expecting Income Loss Next month",
           y = NULL) +
  theme(axis.text.y=element_text(size = 10, margin = margin(t=.5,r=5,b=0,l=0)),
        axis.title.x=element_text(margin(t=5,r=0,b=0,l=0)))  +
  scale_x_continuous(labels=percent, limits=c(0,1)) +
  theme_gray(base_size = 16)

ggsave("black_vs_white_expect_income_loss.png", width=10, height=12, units="in", dpi=600)

## total % expecting income loss per state
employ1.df %>%
  filter(nchar(area) == 2,
         area != 'US',
         characteristic == "Total") %>%
  arrange(area) %>%
  select(area,expect_loss_income_YES_PCT) %>%
  view()
  write_csv("state_average_expect_income_loss.csv")

#facet wrap with asian, white, black, hispanic
#filter black respondents > 50,000

#health status
  
  ggplot(employ1.df %>%
           filter(nchar(area) == 2, area != "US", 
                  characteristic_type == "Health Status") %>%
           select(state_name, characteristic, expect_loss_income_YES_PCT) %>%
           clean_names()%>%
           view(),
         (aes(x=state_name,
              y=expect_loss_income_yes_pct,
              colour = characteristic))) +
    geom_point() +
    geom_segment(aes(x=-0.4,
                     xend=state_name+0.4,
                     y=mean,
                     yend=mean),
                 data=avg,
                 colour="red") +
    labs(title = 'Racial Differences in Expected Future Income Loss',
         subtitle = "Percentage of racial/ethnic respondents (yellow) expecting income loss \nin the next month compared to percentage of white respondents (green)",
         y = "Percent Expecting Income Loss Next Month",
         x = NULL,
         colour = "Race/Ethnicity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(limits = c(0,100), breaks=c(20,40,60,80,100))
  geom_line(aes(x=state_name, y=mean, colour=characteristic))
  
  
  geom_smooth(aes(characteristic), method="lm", se=F)
