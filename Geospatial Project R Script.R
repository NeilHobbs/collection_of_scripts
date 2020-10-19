##Attach required Packages
library(raster)
library(sf)
library(pals)
library(tmap)
library(dplyr)
library(ggplot2)
library(cartography)

##DATA IMPORTATION
##Raster data for Elevation
Malawi_Elevation = raster("./Data/Elevation/malawi_dem.tif")

##Malaria Prevalence Survey data 
Malaria_Prevalence = st_read("./Data/Malaria/malaria.gpkg")

##Malwawi Boundaries
Country_Boundary = st_read("./Data/Bounds/mwi_0.gpkg")
Regions = st_read("./Data/Bounds/mwi_1.gpkg")
Districts = st_read("./Data/Bounds/mwi_2.gpkg")

##Malawi Road Network
Roads = st_read("./Data/malawi_roads/malawi_roads.shp")


##DATA MANIPULATION, ANALYSIS AND PLOTTING

##Total number of People Surveyed per Year.
##Create a new dataset containing only years 2000 to 2014
Malaria_Prevalence_Post_2000 = Malaria_Prevalence%>%
  filter(Year >= 2000)%>%
  group_by(Year)%>%
  mutate(Total_People = sum(N_people))

##Extract Water Bodies and Lakes for use in plotting individually
Lakes = Districts%>%
  filter(ENGTYPE_2 == "Water body")%>%
  st_union()

##Create a base Lakes Map
Lakes_Map = tm_shape(Lakes) +
  tm_fill("skyblue", alpha = 0.5)

##Count number of Malaria Prevalence Surveys (all years)
nrow(Malaria_Prevalence)

##Figure 1a, Map of Malawi with Region Names and include Lakes:
Regions_Name_Map = 
tm_shape(Regions) + 
  tm_borders(lwd = 2, 
             col = "green") + 
    tm_text(size = 0.5, 
            "NAME_1", 
            col = "black") + 
      tm_legend(title = "a") +
  Lakes_Map

##Figure 1b, Map of Administrative Districts and include Lakes
District_Map = 
tm_shape(Districts) + 
  tm_borders(col = "red", 
             lwd = 2) + 
    tm_legend(title = "b") +
      tm_compass(position = c("left", "top"))+
        tm_scale_bar(breaks = c(0, 50, 100, 200), 
                     text.size = 1, 
                     position = c("left", "bottom")) +
    Lakes_Map

##Figure 1a and 1b
tmap_arrange(Regions_Name_Map, District_Map)

##Figure 2, Surveys per Year for all years
Surveys_Per_Year_Plot = ggplot(Malaria_Prevalence, mapping = aes(x=Year)) +
  geom_bar(stat = "count", fill = "blue", colour = "grey") + 
  ylab("Number of Surveys")
Surveys_Per_Year_Plot

##Figure 3, Distribution of Size of Surveys for years 2000 to 2014
ggplot(Malaria_Prevalence_Post_2000, aes(x= N_people)) + 
  geom_histogram(binwidth = 10, fill = "blue", colour = "red") +
  xlab("Number of People Surveyed") +
  ylab("Frequency")

##Summaries of Malaria Surveys 2000 to 2014
summary(Malaria_Prevalence_Post_2000)

#Total number of people surveyed
sum(Malaria_Prevalence_Post_2000$N_people)

##Mean and Standard Deviation of Number of People Surveyed
mean(Malaria_Prevalence_Post_2000$N_people)
sd(Malaria_Prevalence_Post_2000$N_people)

##Range of number of people surveyed
range(Malaria_Prevalence_Post_2000$N_people)

##Count total number of Prevelance surveys 2000 to 2014
nrow(Malaria_Prevalence_Post_2000)

##Figure 4: Scatter Plot of number of people surveyed per year
ggplot(Malaria_Prevalence_Post_2000, aes(x=Year, y=Total_People)) +
  geom_point(size = 2) +
  xlab("Year") +
  ylab("Total Number of People Surveyed")

##CreateFigure 5, Roads and Surveys Map, including Roads, Regions, Surveys and Lakes
Road_and_Surveys = tm_shape(Country_Boundary) + tm_fill("grey") +tm_borders(col = "black", lwd = 2) +
  tm_shape(Roads) + tm_lines(col = "red", lwd = 3) +
  tm_shape(Lakes) + tm_fill("skyblue") +
  tm_shape(Malaria_Prevalence_Post_2000) + tm_squares(col = "green", alpha = 0.4, size = 0.1) +
    tm_scale_bar(breaks = c(0, 50, 100, 200), 
               text.size = 1, 
               position = c("left", "bottom"))

##Plot Figure 5 
Road_and_Surveys

##Function to filter Malaria_Prevalence Survey Data by specific Year
Year_Filter_Function = function(Year_Subset, Malaria_Data, Filter_Year) {
  Year_Subset = Malaria_Data%>%
    filter(Year == Filter_Year)
  return(Year_Subset)
}

##Run Year_Filter_Function to get datasets containing only specified Year
Malaria_2000 = Year_Filter_Function(Malaria_2000, Malaria_Prevalence, 2000)
Malaria_2005 = Year_Filter_Function(Malaria_2005, Malaria_Prevalence, 2005)
Malaria_2009 = Year_Filter_Function(Malaria_2009, Malaria_Prevalence, 2009)
Malaria_2010 = Year_Filter_Function(Malaria_2010, Malaria_Prevalence, 2010)
Malaria_2011 = Year_Filter_Function(Malaria_2011, Malaria_Prevalence, 2011)
Malaria_2014 = Year_Filter_Function(Malaria_2014, Malaria_Prevalence, 2014)

##Function to count number of surveys per regions and,
##create Map of number of malaria surveys in per region, 
##and fill representing number of surveys
Count_Surveys_Function = function(Regions_Data, Survey_Data, YEAR) {
  Regions_Surveys = st_intersects(Regions_Data, Survey_Data, sparse=TRUE)
  Regions_Data$n_surveys = sapply(Regions_Surveys, length)
  Surveys_Map = tm_shape(Regions_Data) + tm_fill("n_surveys", 
                                                 breaks = c(0, 5, 10, 20, 30, 40, 50), 
                                                 title = "Surveys") + 
    tm_borders() +
    tm_legend(title = YEAR, 
              legend.position = c("right", "top"))
return(Surveys_Map)
}

##Run Count_Surveys_Function to give number of surveys per Region per year
Surveys_2000 = Count_Surveys_Function(Regions, Malaria_2000, 2000)
Surveys_2005 = Count_Surveys_Function(Regions, Malaria_2005, 2005)
Surveys_2009 = Count_Surveys_Function(Regions, Malaria_2009, 2009)
Surveys_2010 = Count_Surveys_Function(Regions, Malaria_2010, 2010)
Surveys_2011 = Count_Surveys_Function(Regions, Malaria_2011, 2011)
Surveys_2014 = Count_Surveys_Function(Regions, Malaria_2014, 2014)


##Count total number of Surveys per Region (2000-2014)
Intersect_R_S = st_intersects(Regions, Malaria_Prevalence_Post_2000, sparse=TRUE)
Regions$Total_Surveys = sapply(Intersect_R_S, length)

###Figure 6a: Total Number of Surveys Conducted Per Regional District in Malawi (with Region)
Figure6a = plot(st_geometry(Regions)) +
choroLayer(
  x = Regions,
  var = "Total_Surveys",
  lwd = 2,
  border = "white",
  breaks = c(0, 20, 40, 60, 80, 100, 150, 200)
) +
labelLayer(
  x = Regions,
  txt = "NAME_1",
  col =  "black",
  overlap = FALSE,
  halo = TRUE,
  )

##Figure 6b, Map of Number of Surveys per Region
Grouped_Survey_Map = tmap_arrange(Surveys_2000, Surveys_2005, 
                                  Surveys_2009, Surveys_2010,
                                  Surveys_2011 ,Surveys_2014, 
                                  ncol = 3, nrow = 2)
##Plot Figure 6b
Grouped_Survey_Map

##Function to Calculate and Map Pf% + Standard Deviation in each Region.
##Creates a map per year with fill as Prevalence, dots as SD of prevalence
Average_Malaria_Region_Function = function(Regions_Data, Years_Data, YEAR) {
  
  Years_Data$Regions_Data = unlist(st_intersects(Years_Data, Regions_Data, sparse=TRUE))
  Malaria_Percentage = aggregate(Years_Data$Pf_pct, list(Regions_Data = Years_Data$Regions_Data), mean)
  Regions_Data$Average_Malaria = NA
  Regions_Data$Average_Malaria[Malaria_Percentage$Regions_Data] = Malaria_Percentage$x
  Malaria_Var = aggregate(Years_Data$Pf_pct, list(Regions_Data = Years_Data$Regions_Data), sd)
  Regions_Data$SD_Malaria = NA
  Regions_Data$SD_Malaria[Malaria_Var$Regions_Data] = Malaria_Var$x
  
  Map_District_Prevalence = tm_shape(Regions_Data) + 
    tm_fill("Average_Malaria", title = "Malaria %", 
            breaks = c(0, 20, 40, 60, 80, 100)) + 
    tm_dots(size = "SD_Malaria", 
            breaks = c(0, 20, 40, 60), 
            alpha = 0.5, 
            legend.size.is.portrait = TRUE)+
    tm_borders() + 
    tm_legend(title = YEAR)
  
    return(Map_District_Prevalence)
}

##Run Average_Malaria_Region_Function
Region_Prev_2000 = Average_Malaria_Region_Function(Regions, Malaria_2000, 2000)
Region_Prev_2005 = Average_Malaria_Region_Function(Regions, Malaria_2005, 2005)
Region_Prev_2009 = Average_Malaria_Region_Function(Regions, Malaria_2009, 2009)
Region_Prev_2010 = Average_Malaria_Region_Function(Regions, Malaria_2010, 2010)
Region_Prev_2011 = Average_Malaria_Region_Function(Regions, Malaria_2011, 2011)
Region_Prev_2014 = Average_Malaria_Region_Function(Regions, Malaria_2014, 2014)

##Create Map of Region Prevalences with SD: Figure 7
Region_Prevalences = tmap_arrange(Region_Prev_2000, Region_Prev_2005, 
                                  Region_Prev_2009, Region_Prev_2010, 
                                  Region_Prev_2011, Region_Prev_2014, 
                                  ncol = 3, nrow = 2)
##Plot Figure 7
Region_Prevalences

##Impact of altitude on malaria:
##Calculate Average Altitude in a region, by averaging raster for each region,
##and add Altitude column to Regions dataset
Altitude = extract(Malawi_Elevation, Regions, mean, na.rm=TRUE)
Regions$Altitude = Altitude[,1]

##Extract individual altitude of each Malaria prevalence survey (2000 to 2014)
Malaria_Prevalence_Post_2000$Survey_Altitude = extract(Malawi_Elevation, Malaria_Prevalence_Post_2000)

##Calculate Average Malaria % (2000 - 2014) for each Region
##Add Average Malaria % to Regions dataset
Malaria_Prevalence_Post_2000$Regions = unlist(st_intersects(Malaria_Prevalence_Post_2000, 
                                                            Regions, sparse=TRUE))
Malaria_Pct = aggregate(Malaria_Prevalence_Post_2000$Pf_pct, 
                        list(Regions = Malaria_Prevalence_Post_2000$Regions), mean)
Regions$Malaria_Pct = NA
Regions$Malaria_Pct[Malaria_Pct$Regions] = Malaria_Pct$x



##Figure 9a, Region Altitude with Malaria Percentage.
##Fill is altitude, bubbles are malaria prevalence
Prevalence_Altitude_Map = tm_shape(Regions) + 
  tm_fill("Altitude") +
  tm_bubbles(size = "Malaria_Pct", col = "red") +
  tm_borders(col = "black") +
  tm_legend(title = "a")

##Plot Figure 9a
Prevalence_Altitude_Map

##Figure 9b, Malaria Prevalence and Altitude
ggplot(Regions, aes(x= Altitude, y = Malaria_Pct)) +
  geom_point(colour = "red") +
  ylab("Malaria Prevalence (%)") +
  xlab("Altitude (m)") +
  ggtitle("b")

##Figure 10a: Map, altitude and number of surveys
##Fill is altitude, bubbles are number of suveys
Surveys_Altitude_Map = tm_shape(Regions) + 
  tm_fill("Altitude") +
  tm_bubbles(size = "Total_Surveys", col = "blue") +
  tm_borders(col = "black") +
  tm_legend(title = "a")
Surveys_Altitude_Map

##Figure 10b
ggplot(Regions, aes(x= Altitude, y = Total_Surveys)) +
  geom_point(colour = "blue") +
  ylab("Total Surveys") +
  xlab("Altitude (m)") +
  ggtitle("b")

##Figure 10c: Altitude and Surveys Histogram:
ggplot(Malaria_Prevalence_Post_2000, aes(x=Survey_Altitude)) +
  geom_histogram(binwidth = 10, fill = "blue", colour = "white") +
  xlab("Altitude (m)")+
  ggtitle("c")

##Figure 8: Malaria % and number of surveys
ggplot(Regions, aes(x=Malaria_Pct, y = Total_Surveys)) + 
  geom_point() +
  xlab("Average Malaria Prevalence per Region") +
  ylab("Total Number of Surveys per Region")

##Calculate Minimum distance from survey points to the nearest road
##Add min distance to Malaria_Prevalence (2000-2014) data
dist = st_distance(Malaria_Prevalence_Post_2000, Roads)
Malaria_Prevalence_Post_2000$dist_min = apply(dist, 1, min)

##Figure 11a: Histogram of Malaria Prevalence and Distance
ggplot(Malaria_Prevalence_Post_2000, aes(x=dist_min/1000)) +
  geom_histogram(binwidth = 1, fill = "red", colour = "white")+
  xlab("Distance (km)")+
  ggtitle("a")

##Figure 11b: Malaria Prevalence and Distance from Road
ggplot(Malaria_Prevalence_Post_2000, aes(x=dist_min/1000, y = Pf_pct, colour = factor(Year))) +
  geom_point(alpha = 0.3) +
  ylab(" Malaria Prevalence") +
  xlab("Distance (km)")+
  labs(colour = "Year")+
  ggtitle("b")
  
##Summary statistics of Road Distances
summary(Malaria_Prevalence_Post_2000$dist_min)
sd(Malaria_Prevalence_Post_2000$dist_min)
range(Malaria_Prevalence_Post_2000)


 
