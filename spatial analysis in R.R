packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", "readxl", "tidyr","reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes","sf","tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap",
              "readxl","openxlsx","fuzzyjoin","tidygeocoder","leaflet","reshape2",
              "tidytuesdayR","treemap","rnaturalearth","wordcloud","sfheaders")
# invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, library, character.only = TRUE))

#GIS specific packages 
library("tmap")
library("GISTools")
library("sf")


### Test data 
data(newhaven) #embedded dataset from GISTools
data(georgia) # embedded dataset from GISTools

## Mapping using GISTools 
plot(blocks)
ls() #List objects in current environment
# This can be plotted easily because this is an spatialPOlygonsDataFrame. class() is a way to check. 
plot(roads,col="red")
plot(breach,add=T)

## Mapping with sf data format 
data(georgia)
georgia_sf = st_as_sf(georgia)
class(georgia_sf)
plot(georgia)
plot(georgia_sf)
plot(georgia_sf[,c(4,5)])


# Mapping with quick tmap (qmap)
data(georgia)
georgia_sf<- st_as_sf(georgia)
qtm(georgia_sf, fill="green",style = "cobalt_v3")
qtm(georgia_sf,fill="MedInc",text="Name", text.size=0.5, format="World_wide",style="cobalt_v3",
    text.root=5,fill.title="Median Income")

# Mapping with tmap
