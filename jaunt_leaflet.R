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

kml_coords <- getKMLcoordinates("data/doc.kml")

k2 <- readOGR("data/doc.kml")
jaunt_sf <- st_read("data/doc.kml") %>%
    

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

factpal <- colorFactor(topo.colors(34), jaunt_sf$shape_id)

leaflet() %>%
    addTiles() %>%
    addPolygons(data = jaunt_sf, label = ~shape_id, fillOpacity = 0.5,
                color = ~factpal(shape_id)) %>%
    addMarkers(data = dfCAT)
    
mapview(dfCAT)

mapview(jaunt_sf, zcol = "aread") +
    mapview(dfCAT, col.region = "green")

