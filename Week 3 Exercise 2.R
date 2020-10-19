##CHIC402, Week 3, Exercise 2

##1. Load required packages

library(tidyverse)
library(readr)


##2. Import data set, name life_expectancy_data
life_expectancy_data = read_csv("life_expectancy_data.csv")

##3. Create dataframe of Countries with a mean life expectacy >=65
over_65_sample = life_expectancy_data%>%
  group_by(Country)%>%
  mutate(Average = mean(`Life expectancy`))%>%
  filter(Average >= 65)%>%
  mutate(Life_Expectancy_Category = "Over_65")

##4. Create a list of all the unique Countries with a mean life expectancy >=65
unique_countries_over = unique(over_65_sample$Country)

##5. Sample 5 Countries from list created at Step 4
sample_countries_over = sample(unique_countries_over, 5)

##6. Make a dataframe containing only sampled countries with mean >= 65
over_65_subsample = over_65_sample %>%
  filter(Country %in% sample_countries_over)


##7. Create dataframe of Countries with a mean life expectacy <65
under_65_sample = life_expectancy_data%>%
  group_by(Country)%>%
  mutate(Average = mean(`Life expectancy`))%>%
  filter(Average < 65)%>%
  mutate(Life_Expectancy_Category = "Under_65")
  

##8. Updated life_expectancy_data table
updated_life_expectancy = under_65_sample%>%
  union(over_65_sample)


##9. Create a list of all the unique Countries with a mean life expectancy <65
unique_countries_under = unique(under_65_sample$Country)

##10. Sample 5 Countries from list created at Step 9
sample_countries_under = sample(unique_countries_under, 5)

##11. Make a dataframe containing only sampled countries with mean <65
under_65_subsample = under_65_sample%>%
  filter(Country %in% sample_countries_under)


##12. Join both sampled life expectancy data
sampled_life_expectancy = bind_rows(over_65_subsample, under_65_subsample)


##13. Plot life expectancy over time for sampled Countries
ggplot(sampled_life_expectancy, aes(x=Year, y=`Life expectancy`)) +
  geom_line(aes(colour = Country)) +
  facet_wrap(~Life_Expectancy_Category) 

##14. Pearson's Correlation 
Pearson_Correlation = updated_life_expectancy %>% ##new dataframe
  group_by(Country)%>%
    mutate(Correlation = (cor(`Life expectancy`,`percentage expenditure`, 
                              method=c("pearson"), use="pairwise.complete.obs")))%>%
 distinct(Country, .keep_all = TRUE) ## remove duplicate Correlation values
  
##15. Plot boxplot of Pearson Correleation values, grouped by mean life expectancy
ggplot(Pearson_Correlation, aes(x=Life_Expectancy_Category, y=Correlation)) +
  geom_boxplot()

