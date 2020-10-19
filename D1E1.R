##CHIC402 DAY 1, Exercise 1
##1. Set Working Directory
setwd(setwd("~/Lancaster/CHIC402/CHIC402 D1E1"))

##2. Load required packages.
library(readr)    
library(tidyverse)

##3. Import dataset "arkansas.csv"
arkansas <- read.csv("arkansas.csv")

##4. Basic Exploration of Data by opening table of arkansas data
View(arkansas)

##5. Create predicted_abundance values using y = a + (b*t). t is time.
a = -810
b = 0.41
t = c(arkansas$year)  ## create a vector (t) containing only years
predicted_abundance = a + (b * t) ##new vector of predicted values

##6. Add predicted_abundance vector as a column to arkansas
arkansas["predicted_abundance"] = predicted_abundance

##7. view arkansas data set to confirm column added
view(arkansas)

##8. Plot scatter plot with trendline
ggplot(arkansas, aes(x=year, y=mayfly_abundance)) + 
  geom_point() +
  geom_line(aes(y=predicted_abundance))

##9 Creating the Better Model
##10 Create a new vector containing the t values, which are the year values
t = c(arkansas$year)
Better_Model = c(t)

##11. Create loop to assign equation values based on year
for(i in 1:90){
 if(Better_Model[i] <= 1996) Better_Model[i] = -2301 +(1.158 * Better_Model[i]) else 
 if(Better_Model[i] > 1996) Better_Model[i] = 346 + (-0.167 * Better_Model[i])
 }

##12 Confirm loop worked
arkansas["Better_Model"] = Better_Model
View(arkansas)

    
##13 Plot Graph, with mayfly_abundance and Better_ModelA + B regression lines
ggplot(arkansas, aes(x=year, y=mayfly_abundance)) +
  geom_point() +
  geom_line(aes(x= year, y=Better_Model)) 
  


###########################################################################

##9. Making a better mode, such that both new regression lines are separate 
## for years up to 1996, a=-2301, b=1.158
##for years after 1996, a=346, b=-0.167

##10.select years only pre and post 1996
pre =  filter(arkansas, (year <= 1996)) ##years 1996 and before
post = filter(arkansas, (year >= 1997)) ##years after 1996

##11.select only year values
before_year = pre$year   ##years 1996 and before
after_year = post$year  ##years after 1996

##12. apply model to selected year data
before_model = (-2301 + (1.158 * before_year))  ##years 1996 and before
after_model = (346 + (-0.167 * after_year))  ##years after 1996

##13.create data frames for new predicted values and corresponding year
before_frame <- data.frame("before_year" = before_year, "before_model" = before_model)
after_frame <- data.frame ("after_year" = after_year, "after_model" = after_model)

##14. create plot including data points and new regression lines
ggplot(data = arkansas, aes(x=year, y=,mayfly_abundance)) +
  geom_point() +
  geom_line(data = after_frame, mapping = aes(x=after_year, y=after_model)) +
  geom_line(data=before_frame, mapping = aes(x=before_year, y=before_model))