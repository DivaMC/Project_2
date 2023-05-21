#Objective 1 - Global Map

#Create map using leaflet for the most recent date. 

#For this map, sum the confirmations and deaths of provinces into one value 
#to depict the total number for the country they belong to. 

#When creating a marker for each country in the map, calculate lat and long as 
#the mean values for the provinces that make up each country.

#Customize the map to reflect the differences in magnitude for confirmations and 
#deaths. 

#In the example map below, circle markers that are blue represent low values, 
#gray represents neutral values, and red represents high values. 

#Low, middle, and high values were categorized to aesthetically map the markers based on their 
#probabilistic distribution using the quartile function. 

#You may use any method you like so that it is logical and allows visualization of value intensity. 

#customize the map to include hover labels that indicate country names 
#and popup labels to show the value of confirmations and deaths for that country. 


library(dplyr)
library(leaflet)

#extract data of global confirmed COVID cases
confirmed_COVID_global <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors=FALSE)

#extract data of global COVID deaths
deaths_COVID_global <- 
  read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", stringsAsFactors=FALSE)

#variable holding province names and latest day numbers
lastday_confirmed <- 
  select(confirmed_COVID_global, Province.State, Country.Region, Lat, Long, "Latest_Day"=tail(names(confirmed_COVID_global),1))

#variable holding province names and latest day numbers
lastday_deaths <-
  select(deaths_COVID_global, Province.State, Country.Region, Lat, Long, "Latest_Day"=tail(names(deaths_COVID_global),1))

#Create dataframe containing sum of deaths and confirmations by province
COVID_dataSum <- 
  lastday_confirmed %>% inner_join(lastday_deaths, by=c("Province.State", "Country.Region", "Lat", "Long")) %>%
  mutate(All_Latest = rowSums(across(c(Latest_Day.x, Latest_Day.y)))) %>%
  rename("Last_Confirm"="Latest_Day.x", "Last_Deaths"="Latest_Day.y") 

#Group by Country. Average latitudes and longitudes. Sum data from countries together.
COVID_dataSum_country <- 
  group_by(COVID_dataSum, Country.Region) %>%
  summarize(mean(Lat), mean(Long), sum(Last_Confirm), sum(Last_Deaths), sum(All_Latest)) %>%
  rename("lat"="mean(Lat)", "long"="mean(Long)", "Confirmations"="sum(Last_Confirm)", "Deaths"="sum(Last_Deaths)", "All"="sum(All_Latest)")

#Map
leaflet(COVID_dataSum_country) %>%
  addTiles() %>%
  setView(lng=0, lat=0, zoom=1) %>%
  addCircleMarkers(~Long, ~Lat, color="red")




 