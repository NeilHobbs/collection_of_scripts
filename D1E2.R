##CHIC402 DAY 1, Exercise 2

##1. Load required packages.
library(readr)    
library(tidyverse)

##2. Set Working Directory
setwd("~/Lancaster/CHIC402/CHIC402 D1E2")

##3. Attach and View the iris dataset
data(iris)
View(iris)

##4. create a new vector of petal_sepal_width_ratio
petal_sepal_width_ratio = (iris$Petal.Width / iris$Sepal.Width)

##5. add petal_sepal_width_ratio to iris dataframe and confirm column added
iris["petal_sepal_width_ratio"] = petal_sepal_width_ratio
View(iris)  

##6.Create flower_size vector containig width ratio values
flower_size_width = c(petal_sepal_width_ratio)

##7. Convert the width size ratio to a size label
for (i in 1:150)
  if(flower_size_width[i] >= 0.25 & flower_size_width[i]  <= 0.58){ 
    flower_size_width[i] = "medium_flower"
  } else if (flower_size_width[i] > 0.58){
    flower_size_width[i] = "large_flower" 
     }else if (flower_size_width[i] < 0.25) 
       flower_size_width[i] = "small_flower"

##8. Add flower_size to iris dataset
iris["flower_size_width"] = flower_size_width

##9. Confirm flower_size added to iris dataset       
View(iris)

##10. Graphs of petal_length vs sepal_width
## Colour is species, panel is size

ggplot(iris, aes(x=Petal.Length, y=Sepal.Width)) + 
  geom_point(aes(colour=Species)) +
  facet_wrap(~flower_size_width)



##11.Better Method separation method using length ratio as proxy for size?
##12. Create vector containing length ration
petal_sepal_length_ratio = (iris$Petal.Length) / (iris$Sepal.Length)

##13. Create new vector containing length ratio values to add labels to
flower_size_length = c(petal_sepal_length_ratio)

##14. Create loop to convert length ratio to size
for (i in 1:150)
  if(flower_size_length[i] >= 0.25 & flower_size_length[i]  <= 0.58){ 
    flower_size_length[i] = "medium_flower"
  } else if (flower_size_length[i] > 0.58){
    flower_size_length[i] = "large_flower" 
  }else if (flower_size_length[i] < 0.25) 
    flower_size_length[i] = "small_flower"

##15. Add flower_size_length to iris datset
iris["flower_size_length"] = flower_size_length
View(iris)

##16.Plot length vs width ratio, colour by species and separate by size (defined by length categories)
ggplot(iris, aes(x=petal_sepal_length_ratio, y =petal_sepal_width_ratio)) + 
  geom_point(aes(colour = Species)) 





