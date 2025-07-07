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


g <- st_union(georgia_sf) #Spatial union. Essentially combined all overlapping geometries into a single polygon/shape. Would only keep the outer boundary. In this case we are taking all county boundaries of Georgia and combining into one polygon representing the state of Georgia.


# These maps can takine sp and sf objects
tm_shape(georgia_sf) + tm_fill("blue") +
  tm_borders(lty = "dashed", col="black") +
tm_style("natural", bg.color="grey90")+
  tm_borders(lwd=2)+
  tm_text("Name",size = .5)+
  tm_layout(title="The State of Georgia",
            title.size=1,
            title.position=c(0.55,"top"))

#plot1
plot1 <- tm_shape(georgia_sf) + tm_fill("coral") + 
  tm_borders()+
  tm_layout(bg.color = "grey85")

plot2 <- tm_shape(georgia_sf) + tm_fill("orange") + 
  tm_borders()+
  tm_layout(bg.color = "grey85")

#Set up plot in the same plot window
library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
print(plot1, vp=viewport(layout.pos.col = 1, height = 5))
print(plot2, vp=viewport(layout.pos.col = 2, height = 5))

#Can subset tmaps by creating a vector extracted from the original dataframe
index <- c(81,82,83,150,62,53,21,16,124,121,17)

georgia_sf.sub <- georgia_sf[index,]
tm_shape(georgia_sf.sub) +
  tm_fill("gold1")+
  tm_borders("grey") +
  tm_text("Name",size=.5)+
  tm_borders(lwd=2)+
  tm_layout(title = "Subsetted Map",
            title.size = 1.5, title.position = c(0.,"bottom"))

##3.4.4 Adding Context to a map
tm_shape(georgia_sf) + 
  tm_fill("white")+
  tm_borders("grey",lwd=.5) +
tm_shape(g)+
  tm_borders(lwd=2)+
tm_shape(georgia_sf.sub)+
    tm_fill("lightblue") + tm_borders()+
tm_layout(title="Georgia with subset", frame = T, 
          title.size = 1, title.position = c(0.02,"bottom"))


### Mapping with OpenStreetMap
library("OpenStreetMap")
??OpenStreetMap
georgia.sub <- georgia[index,] #subset data based on previously created vector
ul <- as.vector(cbind(bbox(georgia.sub)[2,2],bbox(georgia.sub)[1,1])) #essentially createes the smallest rectangle to fit the georgaphic area of the shapefile provided i.e. latitude and longitude for this area
lr <- as.vector(cbind(bbox(georgia.sub)[2,1],bbox(georgia.sub)[1,2])) #Ul and LR are upper left and lower right corners of the bounding box, or level of zoom for a visible area...tells openstreetmap where to specifically download data
#bbox extracts the boundaries of the data in the coordinate zone
map <- openmap(ul,lr)
par(mar=c(0,0,0,0))
plot(map,removeMargin=FALSE)
plot(spTransform(georgia.sub,osm()),add=TRUE,lwd=2)

#Mapping with Google Maps
library("RgoogleMaps")


#Mapping with Leaflet
tmap_mode("view") #Sets tmap to interactive viewing
tm_shape(georgia_sf.sub) + tm_polygons(col = "#C6DBEF80")
tmap_mode("plot")
































