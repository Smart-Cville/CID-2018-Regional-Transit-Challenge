library(sp)
library(leaflet)

geoCAT <- read.table(file="/Users/samanthatoet/Desktop/CID/CID-2018-Regional-Transit-Challenge/data/CAT_2017_08_GTFS/stops.txt", 
                     sep=",", header = T)

dfCAT <- data.frame(longitude = geoCAT$stop_lon, 
                 latitude = geoCAT$stop_lat)

coordinates(dfCAT) <- ~longitude+latitude
leaflet(dfCAT) %>% addMarkers() %>% addTiles()

