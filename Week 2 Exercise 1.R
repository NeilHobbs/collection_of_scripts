##CHIC402, Week 2, Exercise 1

#1. Set working directory
setwd("~/Lancaster/CHIC402/CHIC402 D2E1")

##2. Load required packages: tidyverse and readr
library(tidyverse)
library(readr)

##3. Import population.csv data
population = read_csv("population.csv")

##4. Import UN migration 1995 data
unmigration1995 = read_csv("unmigration1995.csv")

##5. Import UN migration 2015 data
unmigration2015 = read_csv("unmigration2015.csv")

##6. Import GDP dataset
GDP = read_csv("GDP.csv")

##7. Intial tidying of UN Migration 1995 dataset
tidy1995 = unmigration1995%>%
  select(-"Sort order", -"Notes", -"Country code", -"Other North", -"Other South" )%>% ##Remove unrequired columns
  gather(key = "origin", value = "num_migrants", "Afghanistan":"Zimbabwe")%>% ##create a column containg country names, and one containing number of migrants
  mutate(Total = as.numeric(str_replace_all(Total, " ", "")))%>% ##remove spaces and convert from string to numeric
  mutate(num_migrants = as.numeric(str_replace_all(num_migrants, " ", ""))) %>% ##remove spaces and convert from string to numeric
  filter(!is.na(num_migrants))%>% ##Remove NAs
  filter(!is.na(Total)) %>% ##Remove NAs
  filter(!is.na(`Type of data (a)`))%>% ##Remove NAs
  rename(Country = "Major area, region, country or area of destination")%>% ##rename Country column to match other dataframes
  distinct(Country, .keep_all = TRUE)%>% ##remove country duplicates
  mutate(Year = c((1995))) ##make Year column

##6. Intial tidying of UN Migration 2015 dataset
tidy2015 = unmigration2015%>%
  select(-"Sort order", -"Notes", -"Country code", -"Other North", -"Other South")%>% ##Remove unrequired columns
  gather(key = "origin", value = "num_migrants", "Afghanistan":"Zimbabwe")%>% ##create a column containg country names, and one containing number of migrants
  mutate(Total = as.numeric(str_replace_all(Total, " ", "")))%>% ##remove spaces and convert from string to numeric
  mutate(num_migrants = as.numeric(str_replace_all(num_migrants, " ", ""))) %>% ##remove spaces and convert from string to numeric
  filter(!is.na(num_migrants))%>% ##Remove NAs
  filter(!is.na(Total)) %>% ##Remove NAs
  filter(!is.na(`Type of data (a)`))%>% ##Remove NAs
  rename(Country = "Major area, region, country or area of destination")%>% ##rename Country column to match other dataframes
  distinct(Country, .keep_all = TRUE)%>% ##remove country duplicates
  mutate(Year = (c(2015)))

 ##7. Organising of the Population Dataset
tidy_population = population%>%
  select("Country", "1995", "2015")%>% ## use only countries and required Years
  gather(key = "Year", value = "Population_Size", "1995", "2015")%>% ## make Year column (containing Years) and Population_Size (containing total population)
  mutate(Year = as.numeric(str_replace_all(Year, " ", "")))

##9. Organising of GDP dataset
tidy_GDP = GDP %>%
  select("Country", "1995", "2015")%>% ## use only countries and required Years
  gather(key = "Year", value = "GDP", "1995", "2015")%>%
  mutate(Year = as.numeric(str_replace_all(Year, " ", ""))) ## make years numeric
  filter(!is.na(GDP)) ##remove countries with NA GDP

##10. Join tidy1995, tidy2015, tidypopulation and tidyGDP
All_Info = tidy1995%>%
  union(tidy2015)%>%
  inner_join(tidy_population)%>%
  inner_join(tidy_GDP)%>%
  mutate(GDP_Per_Capita = (GDP / Population_Size))%>%
  mutate(migration_percent = (Total / Population_Size * 100))%>%
  mutate(Year = as.character(Year)) ##convert Year to character
 
##11. Plot Graph of GDP per capita versus migration percentage
ggplot(All_Info, aes(x=GDP_Per_Capita, y=migration_percent)) + 
  geom_point(mapping = aes(colour = Year))
  
