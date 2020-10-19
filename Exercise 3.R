##CHIC402 Week 3, Exercise 3

##1. Load required packages
library(tidyverse)
library(readr)

##2. Import Data Set Life Expectancy Data
Life_Expectancy_Data = read_csv('Life Expectancy Data.csv')

##3.Function to filter dataset by year(x)
FUNC_Year_Filter = function(x)
  return(Life_Expectancy_Data%>%
           filter(Year == x))

##4. Apply FUNC_Year_Filter to produce filtered data frames (see step 3)
Life_Expectancy_2000 = FUNC_Year_Filter(2000)
Life_Expectancy_2007 = FUNC_Year_Filter(2007)
Life_Expectancy_2012 = FUNC_Year_Filter(2012)

##5.Create Function for linear models to apply to filtered dataframes
##6.Linear Model Version 1 Function
FUNC_linearmodel_Ver1 = function(dataframe){
  return(lm(`Life expectancy` ~ Alcohol + BMI + `Hepatitis B` + 
              Measles + Polio + Diphtheria + `HIV/AIDS`, 
              dataframe))
}

##7.Linear Model Version 2 Function
FUNC_linearmodel_Ver2 = function(dataframe){
  return(lm(`Life expectancy` ~ GDP + Schooling + 
            `Total expenditure` + Population, 
              dataframe))
}

##8.Linear Model Version 3 Function 
FUNC_linearmodel_Ver3 = function(dataframe){
  return(lm(`Life expectancy`~ Status + 
            `percentage expenditure`, 
             dataframe))
}

###9. Run Linear Model Version 1 Function for all filtered dataframes (see step 6)

LM_2000_Ver1 = FUNC_linearmodel_Ver1(Life_Expectancy_2000)
LM_2007_Ver1 = FUNC_linearmodel_Ver1(Life_Expectancy_2007)
LM_2012_Ver1 = FUNC_linearmodel_Ver1(Life_Expectancy_2012)

###10. Run Linear Model Version 2 Function for all filtered dataframes (see step 7)
LM_2000_Ver2 = FUNC_linearmodel_Ver2(Life_Expectancy_2000)
LM_2007_Ver2 = FUNC_linearmodel_Ver2(Life_Expectancy_2007)
LM_2012_Ver2 = FUNC_linearmodel_Ver2(Life_Expectancy_2012)

##11. Run Linear Model Version 3 Function for all filtered dataframes (see step 8)
LM_2000_Ver3 = FUNC_linearmodel_Ver3(Life_Expectancy_2000)
LM_2007_Ver3 = FUNC_linearmodel_Ver3(Life_Expectancy_2007)
LM_2012_Ver3 = FUNC_linearmodel_Ver3(Life_Expectancy_2012)


##12. Vectorise linear models
LM_Vector = c(LM_2000_Ver1,
              LM_2007_Ver1,
              LM_2012_Ver1,
              LM_2000_Ver2,
              LM_2007_Ver2,
              LM_2012_Ver2,
              LM_2000_Ver3,
              LM_2007_Ver3,
              LM_2012_Ver3)

##13 Create dataframe containing data from 2014 (see step 3)
Life_Expectancy_2014 = FUNC_Year_Filter(2014)

#14 Calculate Mean Standard Error; create function.
FUNC_MSE = function(MODEL, dataframe){
  return(
    mean((predict(MODEL, dataframe) - dataframe$`Life expectancy`)^2,
         na.rm=TRUE))
}

##15. Run MSE function on Linear Models (steps 9-11)
##16. Mean Standard Error With Model Versions 1
LM_2000_Ver1_MSE = FUNC_MSE(LM_2000_Ver1, Life_Expectancy_2014)
LM_2007_Ver1_MSE = FUNC_MSE(LM_2007_Ver1, Life_Expectancy_2014)
LM_2012_Ver1_MSE = FUNC_MSE(LM_2012_Ver1, Life_Expectancy_2014)

##17. Mean Standard Error with Model Versions 2 
LM_2000_Ver2_MSE = FUNC_MSE(LM_2000_Ver2 ,Life_Expectancy_2014)
LM_2000_Ver2_MSE = FUNC_MSE(LM_2007_Ver2, Life_Expectancy_2014)
LM_2012_Ver2_MSE = FUNC_MSE(LM_2012_Ver2, Life_Expectancy_2014)

##18. Mean Standard Error with Model Versions 3
LM_2000_Ver3_MSE = FUNC_MSE(LM_2000_Ver3, Life_Expectancy_2014)
LM_2007_Ver3_MSE = FUNC_MSE(LM_2007_Ver3, Life_Expectancy_2014)
LM_2012_Ver3_MSE = FUNC_MSE(LM_2012_Ver3, Life_Expectancy_2014)


#19. Create vector containing MSE values
MSE_Values = (c(LM_2000_Ver1_MSE,
                LM_2007_Ver1_MSE,
                LM_2012_Ver1_MSE,
                LM_2000_Ver2_MSE,
                LM_2000_Ver2_MSE,
                LM_2012_Ver2_MSE,
                LM_2000_Ver3_MSE,
                LM_2007_Ver3_MSE,
                LM_2012_Ver3_MSE
                ))

##20. Find lowest MSE value
Best_Model = min(MSE_Values)    

#21. Plot predictions from best model only; doesn't work. Unsure why?

for(i in 1:length(MSE_Values)){
  if(Best_Model == MSE_Values[i])
    qplot(predict(LM_Vector[i], Life_Expectancy_2014), Life_Expectancy_2014$`Life expectancy`)
}

