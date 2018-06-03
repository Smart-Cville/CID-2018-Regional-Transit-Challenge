#' ---
#' title: Explore R's GTFS Tools
#' description: Fuctions and patterns for importing and plotting the CAT's GTFS data
#' author: nathancday@gmail.com
#' ---

library(magrittr)
library(tidyverse)
library(rgdal)
library(gtfsr)
library(sf)

## GTFSR -----------------------------------------------------------

# https://ropensci.github.io/gtfsr/articles/gtfsr-vignette.html
# visualize gtfs data via leaflet

cat <- import_gtfs("data/CAT_2017_08_GTFS.zip", TRUE)

# pre-prend all colors with '#' for leaflet ease
cat$routes_df$route_color %<>% paste0("#", .)

cat %>% map_gtfs(., route_ids = .$routes_df$route_id,
                 route_colors = .$routes_df$route_color)
# very high-level; powerful by restrictive


## Raw ---------------------------------------------------------------
# reproduce with just leaflet and sf; for customization/data integration
library(leaflet)
library(sf)

# cat$trips_df has pairings between route_id and shape_id
cat$trips_df

cat$trips_df %>% with(table(route_id, shape_id))
# multiple shape_ids to single route_id, because of direction variable

# not sure what to do with direction yet so keep all
shape_key <- cat$trips_df %>%
    select(route_id, shape_id) %>%
    unique()

# route keys for route_color info
route_key <- cat$routes_df %>%
    select(route_id, route_short_name, route_color) %>%
    mutate(route_color = route_color,
           route_short_name = paste("Route", route_short_name))

route_key %<>% inner_join(shape_key)

# exctract line string for each "shape_id"
sfc <- cat$shapes_df %>%
    split(.$shape_id) %>%
    map(~ select(., shape_pt_lon, shape_pt_lat) %>%
            as.matrix %>%
            st_linestring) %>%
    st_sfc(crs = 4326) # leaflet uses 4326

routes <- unique(cat$shapes_df$shape_id) %>% 
    sort() %>% # order to match with names(sfc)
    st_sf(shape_id = ., geometry = sfc) %>%
    inner_join(route_key)

## * 3 options for plots now -----------------------

# base
st_geometry(routes) %>%
    plot(col = routes$route_color)

# gg
ggplot(routes) +
    geom_sf(aes(color = route_id)) +
    scale_color_manual(values = route_key$route_color %>% set_names(route_key$route_id))

# leaf
leaflet(routes) %>%
    addProviderTiles("Stamen.TonerHybrid") %>%
    addPolylines(color = ~route_color,
                 label = ~htmlEscape(route_short_name) )

# now do it for stops

# new key on 'trip_id' to 
trip_key <- cat$trips_df %>%
    select(trip_id, route_id, shape_id) %>%
    unique()

stop_key <- cat$stop_times_df %>%
    select(trip_id, stop_id) %>%
    unique() %>%
    inner_join(trip_key)

# pair down to only allow one row per stop location (even though there is stop overlap)
one_stop <- stop_key %>% arrange(route_id) %>%
    group_by(stop_id) %>%
    slice(1)

# similar sf pattern
stops <- cat$stops_df %>%
    inner_join(one_stop)

stop_points <- cat$stops_df %>%
    split(.$stop_id) %>%
    map(~select(., stop_lon, stop_lat) %>%
            unlist() %>%
            st_point() ) %>%
    st_sfc()

stops <-unique(cat$stops_df$stop_id) %>%
    sort() %>% # order to match with names(sfc)
    st_sf(stop_id = ., geometry = stop_points) %>%
    inner_join(stops)

# add route_color var back in
dc <- cat$routes_df$route_color %>%
    set_names(cat$routes_df$route_id)
stops$route_color <- dc[stops$route_id]


leaflet(routes) %>%
    addProviderTiles("Stamen.TonerHybrid") %>%
    addPolylines(color = ~route_color,
                 label = ~htmlEscape(route_short_name) ) %>%
    addCircleMarkers(data = stops,
                     color = "black",
                     radius = 6,
                     fillColor = ~route_color,
                     popup = ~htmlEscape(stop_name)) %>%
    addLegend(colors = cat$routes_df$route_color, labels = cat$routes_df$route_short_name)
# close enough :)
# now we can visualize trends about ridership volume, daily usage, etc if we merge in new data

