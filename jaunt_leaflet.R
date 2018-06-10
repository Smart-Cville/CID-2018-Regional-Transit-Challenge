#' ----
#' title: JAUNT kml
#' author: CID
#' description: unpack shape polygons
#' ----

library(tidyverse)
library(magrittr)
library(maptools)
library(sf)
library(leaflet)
library(mapview)
library(openxlsx)
library(htmltools)

# set working directory to your local repo in 'Console'
setwd("~/your/path/git_repo/")

## CAT sample case ------------------------------------------------------------
geoCAT <- read.table(file="data/CAT_2017_08_GTFS/stops.txt",
                     sep=",", header = T)

# select lon/lat coordinates for building SP object
dfCAT <- data.frame(longitude = geoCAT$stop_lon, 
                    latitude = geoCAT$stop_lat)

# cast into an SP object for leaflet()
coordinates(dfCAT) <- ~longitude+latitude

# plot all of the stops
leaflet(dfCAT) %>%
    addTiles() %>%
    addMarkers()

## Jaunt service polygons ----------------------------------------------------

# unzip .kmz file first
unzip("data/JAUNT_ParaService_Polygons.kmz", exdir = "data/")

# the hard way
kml_coords <- getKMLcoordinates("data/doc.kml")

kml_coords[[1]] # extra 3rd column?

# extract polgyons from each element in 'kml_coords'
pg <- map(kml_coords, ~ list(.[,1:2]) %>% # drop problem column
              st_polygon) %>% # convert to simple feature POLYGON
    st_sfc(crs = 4326) # combine into a collection

class(pg) # "sfc_POLYGON" "sfc"

plot(pg) # base method

# convert into 'sf'
jaunt_sf <- st_sf(shape_id = 1:34, geometry = pg)
class(jaunt_sf)

# the easy way
jaunt_sf <- st_read("data/doc.kml") %>%
    select(geometry) %>%
    mutate(shape_id = 1:26)




ggplot(jaunt_sf) + # geom_sf avaiable in 'ggplot2_2.2.1.9000'
    geom_sf()

mapview(jaunt_sf) # plotting in row order

# arrange polygons by decreasing area
jaunt_sf %<>%
    mutate(aread = st_area(.) %>%
               unclass) %>%
    arrange(desc(aread))

# now plots largest first, so smallest shapes are on top
mapview(jaunt_sf, zcol = "shape_id")

# get associated meta data for polygons
meta <- read.xlsx("data/JAUNT_ParaServiceAreaRules.xlsx")

meta %<>% 
    rename(shape_id = ParaServiceId) %>%
    group_by(shape_id) %>%
    slice(1)

## Combine CAT stops and JAUNT polygons

# play with colors

pal <- colorFactor(c("red", "green", "blue"), 1:34)

factpal <- colorNumeric("viridis", jaunt_sf$aread)

leaflet() %>%
    addTiles() %>%
    addPolygons(data = jaunt_sf, options = NULL, label = ~shape_id, fillOpacity = 0.5,
                color = ~factpal(aread)) %>%
    addCircleMarkers(data = dfCAT, radius = 5, color = "#2b6bb4",
                     fillColor = "#c8da33", fillOpacity = 0.5)
    
class(mapview(dfCAT))

(mapview(jaunt_sf, zcol = "aread") +
    mapview(dfCAT, col.region = "green"))@map

mapview(jaunt_sf, zcol = "aread")@map %>%
    addCircleMarkers(data = dfCAT,radius = 5, color = "#2b6bb4",
                     fillColor = "#c8da33", fillOpacity = 0.5)

### blogpost code -----------------------------------------------------------

# CAT stop coordinates
cat_sf <- read.table(file="data/CAT_2017_08_GTFS/stops.txt", 
                     sep=",", header = T) %>%
    select(stop_name, stop_lon, stop_lat) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"))

# JAUNT polygons arrange largest >>> smallest
jaunt_sf <- st_read("data/doc.kml") %>%
    select(-Description) %>%
    mutate(aread = st_area(.) %>% unclass) %>%
    arrange(desc(aread))

widgetframe::frameWidget( # web overhead
    (mapview(jaunt_sf, label = jaunt_sf$Name, col.region = "#4a9dff", color = "#ec101f") +
        mapview(cat_sf, label = cat_sf$stop_name, color = "greenyellow", col.region = "#2b6bb4"))@map
)

