library(sf)
library(dplyr)

lu_shp <- "G:/CS6/full_workflow_cs6/Data/for_farmr_input/Land_crops_CS6.shp"
land_shp <- "G:/CS6/full_workflow_cs6/Data/for_buildr/CS6_LUSE_Final2.shp"
lu_sf <- read_sf(lu_shp) %>% 
  st_point_on_surface() 

land_sf <- read_sf(land_shp) %>% 
  select() %>% 
  st_join(lu_sf, join = st_intersects) %>% 
  select(-id) %>% 
  mutate(id = row_number()) %>% 
  mutate(type = ifelse(grepl("^agrl", type), paste0("agrl", id), type)) %>% 
  mutate(type = case_when(
    id == 8 ~ "frst",
    id == "4568" ~ "frst",
    id == "6358" ~ "urmd",
    id == "5" ~ "urmd",
    TRUE ~ type
  ))

land <- land_sf  %>% 
  select(id, type)

lu_sf_w <- land_sf %>% 
  select(-id) %>% 
  rename(lu = type)

write_sf(land, "G:/CS6/full_workflow_cs6/Data/for_buildr/land.shp")
write_sf(lu_sf_w, "G:/CS6/full_workflow_cs6/Data/for_farmr_input/lu_crops.shp")
