
library(tidyverse)

cv_all <- read_sf(dsn = here::here("data"),
                  layer = "cv") %>% 
  st_transform(crs = 4326) %>% 
  clean_names()



sgma_basins_all <- read_sf(dsn = here::here("data"),
                           layer = "sgma_basins") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() 

sgma_basins <- sgma_basins_all %>% 
  separate(basin_su_1, c("basin", "sub_basin"), sep = " - ") %>% 
  mutate(sub_basin_final = ifelse(is.na(sub_basin), basin, sub_basin)) %>% 
  mutate(sub_basin_final = to_upper_camel_case(sub_basin_final, sep_out = " ")) %>% 
  arrange(sub_basin_final)
 


madera <- sgma_basins %>% 
  filter(sub_basin_final == "Madera")



wgs84 = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs" # Just have this ready to copy/paste

max_score_raster <- raster::raster(here::here("data", "Max_final_score_LU.tif"))

max_score_reproj = projectRaster(max_score_raster, crs = wgs84, method = "bilinear")



max_score_filter <- mask(max_score_reproj, madera)

drywells <- read_sf(here("data",
                         "drywells_cv.shp")) %>% 
  st_transform(crs = 4326)


drywells_mad <- st_intersection(drywells, madera)


#leaflet() %>% 
 # addCircleMarkers(data = drywells) %>% 

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addRasterImage(max_score_filter) %>% 
  addPolygons(data = madera)


if(input$wells_check == TRUE){addCircleMarkers(data = drywells
