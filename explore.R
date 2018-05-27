#' ---
#' title: Explore Tools
#' description: Build methods for importing and plotting the data
#' author: nathancday@gmail.com
#' ---

library(gtfsr)
library(tidyverse)

# https://ropensci.github.io/gtfsr/articles/gtfsr-vignette.html

cat <- import_gtfs("data/2017_08_CharlottesvilleAreaTransit.zip", TRUE)

cat %>% map_gtfs(., route_ids = .$routes_df$route_id,
                 route_colors = paste0("#", .$routes_df$route_color))

# reproduce with just leaflet
library(leaflet)
library(sf)


sfc <- cat$shapes_df %>%
    split(.$shape_id) %>%
    map(~ select(., matches("lat|lon")) %>%
            as.matrix %>%
            st_linestring) %>%
    st_sfc(crs = 3857)

routes <- cat$shapes_df %>% 
    with(unique(shape_id)) %>%
    st_sf(a = ., geometry = sfc)

st_geometry(routes) %>%
    plot()
 
ggplot(routes) +
    geom_sf(aes(color = a))

