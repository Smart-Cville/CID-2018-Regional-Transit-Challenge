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

kml_coords <- getKMLcoordinates("data/doc.kml")

head(kml_coords)

k1 <- kml_coords[[1]]

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

# add color 

jaunt_sf$color <- rep(ggsci::pal_d3()(10), length.out = nrow(jaunt_sf))
leaflet() %>%
    addPolygons(data = jaunt_sf, color = ~color, labels = ~shape_id)
    
# bring in accessory data
meta <- read.xlsx("data/JAUNT_ParaServiceAreaRules.xlsx")
meta %<>% 
    rename(shape_id = ParaServiceId) %>%
    group_by(shape_id) %>%
    slice(1)
