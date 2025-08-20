packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", "readxl", "tidyr","reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes","sf","tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap",
              "readxl","openxlsx","fuzzyjoin","tidygeocoder","leaflet","reshape2",
              "tidytuesdayR","treemap","rnaturalearth","wordcloud","sfheaders","RgoogleMaps")
# invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, library, character.only = TRUE))

############## Working from "An Introduction to R for Spatial Analysis and Mapping" book ###########

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


# These maps can take in sp and sf objects
tm_shape(georgia_sf) + tm_fill("blue") +
  tm_borders(lty = "dashed", col="black") +
tm_style("natural", bg.color="grey90")+
  tm_borders(lwd=2)+
  tm_text("Name",size = .5)+
tm_title("The State of Georgia",position = tm_pos_out("center", "top"), size = 1)

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
  tm_title("Subsetted Map", position = tm_pos_out("center", "top"), size = 1.5)

##3.4.4 Adding Context to a map
tm_shape(georgia_sf) + 
  tm_fill("white")+
  tm_borders("grey",lwd=.5) +
tm_shape(g)+
  tm_borders(lwd=2)+
tm_shape(georgia_sf.sub)+
    tm_fill("lightblue") + tm_borders()+
  tm_title("Georgia with Subset", position = tm_pos_out("center", "top"), size = 1)



### Mapping with OpenStreetMap
library("rJava")
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
#Option 1 -- Will only work on personal machine
georgia_sf.sub <- st_as_sf(georgia_sf.sub) #Need to convert to an sf object -- currently a SPatialPolygonsDataFrame (sp)
tmap_mode("view") #Sets tmap to interactive viewing
map <- tm_shape(georgia_sf.sub) + tm_polygons(col = "#C6DBEF80")
tmap_leaflet(map, viewer = "viewer")

#Option 2 -- Issues with firewall workaround
library("htmlwidgets")

tmap_mode("view")
map <- tm_shape(georgia_sf.sub) + tm_polygons(col = "#C6DBEF80")
saveWidget(tmap_leaflet(map), "georgia_map.html", selfcontained = TRUE) # Save to file

browseURL("georgia_map.html") # Open in your browser


#### Mapping Spatial Data Attributes
#Note: The attributes associated with individual features (lines, points, areas in vector data and cell values in raster data) provide the basis for spatial analyses and geographical investigation
data(newhaven)
ls()
#convert to sf objects
blocks_sf <- st_as_sf(blocks)
breach_sf <- st_as_sf(breach)
tracts_sf <- st_as_sf(tracts)

summary(blocks_sf)
class(blocks_sf)
summary(breach_sf)
class(breach_sf)
summary(tracts_sf)
class(tracts_sf)

breach.dens = st_as_sf(kde.points(breach,lims = tracts))
summary(breach.dens)


#Mapping Polygons and Attributes
tmap_mode('plot')
tm_shape(blocks_sf) + tm_polygons("P_VACANT",breaks=seq(0,50,by=10),
                                  title="Percent Vacant", palette="Oranges")+
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1,0.1))

tm_shape(blocks_sf) + tm_fill("P_VACANT", title="Percent Vacant", palette="Oranges") +
  tm_layout(legend.title.size = 1)

tm_shape(blocks_sf) + tm_polygons("P_VACANT",breaks=seq(0,50,by=10)) #Breaks (Option 1)
tm_shape(blocks_sf) + tm_polygons("P_VACANT",breaks=c(10,15,35,50)) #Breaks (Option 2)

display.brewer.all() #View all color pallettes


#Plotting a map with different intervals
# 1. Equal intervals (default)
p1 <- tm_shape(blocks_sf) + 
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Blues") + 
  tm_layout(legend.title.size = 0.7)

# 2. K-means style classification
p2 <- tm_shape(blocks_sf) + 
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Oranges", style = "kmeans") + 
  tm_layout(legend.title.size = 0.7)

# 3. Quantile classification
p3 <- tm_shape(blocks_sf) + 
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Purples", style = "quantile") + 
  tm_layout(legend.title.size = 0.7)

tm_polygons("P_OWNEROCC", 
            title = "Owner Occ", 
            palette = "Greens", 
            breaks = c(0, round(quantileCuts(blocks$P_OWNEROCC, 6), 1))) + 
  tm_layout(legend.title.size = 0.7)

library(grid)

grid.newpage()

# Set up layout (1 row, 3 columns)
pushViewport(viewport(layout = grid.layout(1, 3)))

# Print maps side by side
print(p1, vp = viewport(layout.pos.col = 1, height = 5))
print(p2, vp = viewport(layout.pos.col = 2, height = 5))
print(p3, vp = viewport(layout.pos.col = 3, height = 5))


#Plotting a map with a histogram
library(tmap)

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title="Owner OCC",palette="-GnBu",
              breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1)),legend.hist=T) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 0.7) +  # valid version 4 call
  tm_compass(position = c("right", "bottom")) +
  tm_layout(
    frame = FALSE,
    title = "New Haven",
    title.size = 2,
    title.position = c("center", "top"),
    legend.hist.size = 0.5)

# Interactive - OpenStreetMap
georgia.sub <- georgia[index,]
ul <- as.vector(cbind(bbox(georgia.sub)[2,2], bbox(georgia.sub)[1,1]))
lr <- as.vector(cbind(bbox(georgia.sub)[2,1], bbox(georgia.sub)[1,2]))
mymap <- openmap(ul,lr)
par(mar=c(0,0,0,0))
plot(mymap, removeMargin=F)
plot(spTransform(georgia.sub,osm()),add=T,lwd=2)



#Choropleth mapping with tmap 
# tmap_mode('plot')
tm_shape(blocks_sf) +
    tm_polygons("P_OWNEROCC", title="Owner Occ",
                palette="-GnBu",
                breaks=c(0,round(quantileCuts(blocks$P_OWNEROCC,6),1)),legend.hist=T) + #census block of proportion of vacant properties 
# tm_scale_bar(width = 0.22) + 
  tm_compass(position = c(0.8,0.07)) +
  tm_layout(frame=F) 

tm_shape(blocks_sf) +
  tm_polygons("white")+
  tm_shape(breach_sf) + 
  tm_dots(size=0.5, shape=19, col="red",alpha=.5)


data(quakes)
head(quakes)
coords.tmp <- cbind(quakes$long, quakes$lat)
quakes.sp <- SpatialPointsDataFrame(coords.tmp, data = data.frame(quakes),
                                    proj4string = CRS("+proj=longlat"))
quakes_sf <- st_as_sf(quakes.sp)

tm_shape(quakes_sf)+
  tm_dots(size = 0.5,alpha=0.3)
p1 <- tm_shape(quakes_sf)+
  tm_bubbles("depth",scale=1,shape=19,alpha=0.3,title.size="Quake Depths")
p2 <- tm_shape(quakes_sf)+
  tm_dots("depth",shape=19,alpha=0.5,size=0.6,palette="PuBuGn",title="Quake Depths")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
  print(p1, vp=viewport(layout.pos.col = 1,height = 5))
  print(p2, vp=viewport(layout.pos.col = 2,height = 5))
  
index <- quakes_sf$mag > 5.5
summary(index)
tmp <- quakes_sf[index,]
tm_shape(tmp) +
  tm_dots(col=brewer.pal(5,"Reds")[4],shape=19,alpha=0.5,size=1)+
  tm_layout(title="Quakes > 5.5",title.position = c("center","top"))

lat <- as.vector(quakes$lat)
long <- as.vector(quakes$long)
MyMap <- MapBackground(lat=lat,lon=long)
tmp <- 1+(quakes$mag - min(quakes$mag))/max(quakes$mag)
PlotOnStaticMap(MyMap,lat,long,cex=tmp,pch=1,col='#FB6A4A30')

MyMap <- MapBackground(lat=lat,lon=long,zoom = 10,maptype="satellite")
PlotPnStaticMap(MyMap,lat,long,cex=tmp,pch=1,col='#FB6A4A50')


### Pulling from https://study.sagepub.com/Brunsdon2e
library(GISTools)  # for the mapping tools
library(sf)  # for the mapping tools
library(rgdal)    # this has the spatial reference tools
library(tmap)
library(OpenStreetMap)
data(newhaven)
# Define a new projection
newProj <- CRS("+proj=longlat +ellps=WGS84")
# Transform blocks and breach
# 1. using spTransform
breach2 <- spTransform(breach, newProj)
blocks2 <- spTransform(blocks, newProj)
# 2. using st_transform
breach_sf <- st_as_sf(breach)
blocks_sf <- st_as_sf(blocks)
breach_sf <- st_transform(breach_sf, "+proj=longlat +ellps=WGS84")
blocks_sf <- st_transform(blocks_sf, "+proj=longlat +ellps=WGS84")

# set the mode
tmap_mode('view')
# plot the blocks
tm_shape(blocks_sf) +
  tm_borders() +
  # and then plot the breaches
  tm_shape(breach_sf) +
  tm_dots(shape = 1, size = 0.1, border.col = NULL, col = "red", alpha = 0.5)

ul <- as.vector(cbind(bbox(blocks2)[2,2], 
                      bbox(blocks2)[1,1]))
lr <- as.vector(cbind(bbox(blocks2)[2,1], 
                      bbox(blocks2)[1,2]))
# download the map tile
MyMap <- openmap(ul,lr)
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE)
# notice how the data need to be transformed 
# to the internal osm projection  
plot(spTransform(blocks2, osm()), add = TRUE, lwd = 1)
plot(spTransform(breach2, osm()), add = T, pch = 19, col = "#DE2D2650")


