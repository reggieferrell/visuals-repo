
packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", "readxl", "tidyr","reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes","sf","tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap",
              "readxl","openxlsx","fuzzyjoin","tidygeocoder","leaflet","reshape2",
              "tidytuesdayR","treemap","rnaturalearth")
# invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, library, character.only = TRUE))

#Reggie's Visualization Warehouse 
#references: FLowingdata, Data Science with R, 50 visaulizations in R


kaggle_laptops <- read.csv("C:/Users/rferrell/visuals_repo/laptop_price - dataset.csv") %>% clean_names() %>%
  group_by(company,type_name) %>% summarise(avg_price=mean(price_euro))
kaggle_billionaires <- read.csv("C:/Users/rferrell/visuals_repo/Top_1000_wealthiest_people.csv") %>% clean_names()%>% 
  mutate(count=1) %>% group_by(industry,country) %>% 
  summarise(billionaire_count =sum(count)) %>% ungroup() %>% group_by(country)%>%
  mutate(total_by_industry = sum(billionaire_count),
         billionaire_pct = round(billionaire_count/total_by_industry,2))
kaggle_cities <- read.csv("C:/Users/rferrell/visuals_repo/worldcities.csv") %>% clean_names()
kaggle_climate <- read.csv("C:/Users/rferrell/visuals_repo/climate_change_impact_on_agriculture_2024.csv") %>% clean_names() %>% 
  filter(country=="USA") %>% dplyr::select(1:3,5,7,extreme_weather_events,economic_impact_million_usd) %>%
  group_by(year,region) %>% summarise(average_temperature_c = mean(average_temperature_c),
                              economic_impact_million_usd = mean(economic_impact_million_usd),
                              co2_emissions_mt = mean(co2_emissions_mt),
                              extreme_weather_events = mean(extreme_weather_events),
                              economic_impact_million_usd = mean(economic_impact_million_usd)) %>%
  melt(id=c("year","region"))
kaggle_familyheight <- read.csv("C:/Users/rferrell/visuals_repo/GaltonFamilies.csv")%>% clean_names()


#Bar Charts
kaggle_billionaires %>% filter(country=="USA") %>% 
ggplot(aes(x=industry, y=billionaire_count)) + 
  geom_bar(stat = "identity",fill="green")+
  theme_minimal()

#Stacked Bar Chart
kaggle_billionaires %>%
  ggplot(aes(x=country, y=billionaire_pct, fill=industry)) + 
  geom_bar(stat = "identity",position = "fill")+
  theme_minimal()

#Histogram
kaggle_climate %>% 
  filter(region=="Midwest",variable=="average_temperature_c") %>% 
  ggplot(aes(x=value)) +
  geom_histogram(binwidth=3,fill="green",color="white") +
  theme_minimal()

# Line Charts
kaggle_climate %>% 
  filter(variable=="extreme_weather_events") %>% 
  ggplot(aes(x=year, y=value, fill=region,color=region)) +
  geom_line(size=1)+
  geom_point(size=3)+
  theme_minimal()

#Scatter Plot
kaggle_familyheight %>% 
ggplot(aes(x=child_height, y=father)) + 
  geom_point(size=2) +
  theme_minimal()+
  geom_smooth(method=lm) +
facet_wrap(~gender)

#Stacked Area Chart
kaggle_climate %>% 
  filter(variable=="extreme_weather_events") %>%
  ggplot(aes(x=year, y=value, fill=region)) + 
  geom_area()+
theme_minimal()

#Box Plot 
kaggle_familyheight %>% 
ggplot(aes(x=gender, y=child_height, fill=gender)) + 
  geom_boxplot() +
  theme_minimal()

# Density Charts
kaggle_laptops %>% 
  ggplot(aes(x=avg_price, color=type_name, fill=type_name)) +
  geom_density(alpha=0.6) +
  theme_minimal()


# Bubble CHart
kaggle_familyheight %>% 
  ggplot(aes(x=father, y=mother, size = child_height)) +
  geom_point(alpha=0.5) +
  theme_minimal()


#Pyramid Charts
#Timeline
#Treemap
#Word Cloud
#table
#Unit Chart
#Small Multiples
#Pictogram
#Marimekko plot
#Heatmap
#Slopegraphs
#Beeswarm

#### Maps 

#Map
#Dot Density Map
#Choropleth Map
#R Shiny Map
leaflet() %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles()

