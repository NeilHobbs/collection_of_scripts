##CHIC402 Week2, Exercise 2

#1. Set Working Directory
setwd("~/Lancaster/CHIC402/CHIC402 D2E2")

#2. Load required packages
library(tidyverse)
library(readr)

##3. Import unemployment dataset
unemployment_data = read_csv("unemployment_data.csv")
View(unemployment_data)

##4.Tidy Unemployment dataset
tidy_unemployment = unemployment_data%>%
  select("Country Name", "Indicator Name", "2010":"2015")%>%
  gather(key = "Year", value = "Unemployment", "2010":"2015")%>%
  rename(Country = "Country Name")%>%
  rename(Indicator = "Indicator Name")%>%
  as.factor(Indicator)%>%
  filter(!is.na(Year))%>%
  filter(!is.na(Unemployment))%>%
  mutate(Year = as.numeric(str_replace_all(Year, " ", "")))
  
View(tidy_unemployment)

##5Average unemployment from each indicator, from each Country. 

##6Vector to contain average unemployment values
average_unemployment = c()

##7function to determine mean unemployment grouped by country and indicator
#I've got stuck on this point and am unsure how to go about converting what I want to do
#I want to calculate the mean for values in the column Employment that is grouped by Country and
#Indicator. 
average_unemployment = c()

calc_average_unemployment = function(Location, StartYear, EndYear){
  tidy_unemployment%>%
    filter("Country" == Location, Year >= StartYear % Year <= EndYear)%>%
           group_by(Indicator) %>%
             summarise((average_unemployment = (mean(Unemployment, na.rm = TRUE))))
           return(average_unemployment)
}
  









