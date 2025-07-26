library(sf)
library(dplyr)

lu_shp <- "G:/CS6/full_workflow_cs6/Data/for_farmr_input/Land_crops_CS6.shp"
lu_sf <- read_sf(lu_shp) %>% 
  mutate(id = row_number()) %>% 
  mutate(type = ifelse(grepl("^agrl", type), paste0("agrl", id), type))
lu_sf_w <- lu_sf %>% 
  select(-id) %>% 
  rename(lu = type)

land <- lu_sf %>% 
  mutate(id = as.integer(id)) %>% 
  select(id, type)

write_sf(land, "G:/CS6/full_workflow_cs6/Data/for_buildr/land.shp")
write_sf(lu_sf_w, "G:/CS6/full_workflow_cs6/Data/for_farmr_input/lu_crops.shp")