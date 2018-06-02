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

## CAT sample case ------------------------------------------------------------
geoCAT <- read.table(file="/Users/samanthatoet/Desktop/CID/CID-2018-Regional-Transit-Challenge/data/CAT_2017_08_GTFS/stops.txt", 
                     sep=",", header = T)

dfCAT <- data.frame(longitude = geoCAT$stop_lon, 
                    latitude = geoCAT$stop_lat)

coordinates(dfCAT) <- ~longitude+latitude
leaflet(dfCAT) %>% addMarkers() %>% addTiles()


## Jaunt exploration ----------------------------------------------------------

kml_coords <- getKMLcoordinates("data/doc.kml")

pg <- map(kml_coords, ~ list(.[,1:2]) %>%  st_polygon) %>% st_sfc(crs = 4326)

class(pg)

plot(pg)

jaunt_sf <- st_sf(shape_id = 1:34, geometry = pg)
class(jaunt_sf)

ggplot(jaunt_sf) +
    geom_sf()

mapview(jaunt_sf)

# arrange polygons by decreasing area
jaunt_sf %<>%
    mutate(aread = st_area(.) %>%
               unclass) %>%
    arrange(desc(aread))

# flipped map in the right order
mapview(jaunt_sf, zcol = "shape_id")


# play with colors

pal <- colorFactor(c("red", "green", "blue"), 1:34)

factpal <- colorFactor(topo.colors(34), jaunt_sf$shape_id)

leaflet() %>% addTiles() %>%
    addPolygons(data = jaunt_sf, label = ~shape_id, fillOpacity = 0.5,
                color = ~factpal(shape_id)) %>%
    addMarkers(data = dfCAT)
    
    
# bring in accessory data
meta <- read.xlsx("data/JAUNT_ParaServiceAreaRules.xlsx")
meta %<>% 
    rename(shape_id = ParaServiceId) %>%
    group_by(shape_id) %>%
    slice(1)
