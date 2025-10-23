library(dplyr)
library(sf)
library(tidyr)
library(SWATprepR)

met_lst <- readRDS("Data/for_prepr/meteo_data.rds")
d_pth <- "Data/for_prepr/additional_meteo/"

## The function to read the meteorological data files
read_meteo_file <- function(txt_file){
  y <- read.csv(txt_file)
  start_date <- as.Date(substr(names(y[1]), 2, 9), format = "%Y%m%d")
  date_seq <- seq(from = start_date, by = "day", length.out = dim(y)[1])
  yy <- y %>%
    rename(value = 1) %>%
    mutate(DATE = date_seq)
  if(dim(y)[2] == 1){
    yy <- select(yy, DATE, value)
  } else if (dim(y)[2] == 2){
    yy <- rename(yy, TMP_MAX = 1, TMP_MIN = 2) %>%
      select(DATE, TMP_MIN, TMP_MAX)
  } else {
    stop("Something wrong with the data format")
  }
  return(yy)
}

# Add new stations to the existing meteo list
stations_df <- data.frame(
  ID = paste0("ID", c(4, 5, 6, 7, 11, 12, 21, 22, 23)),
  Name = c("Lenti-Mahomfa", "Redics", "Szilvágy", "rad_7", "rad_11", 
           "rad_12", "rad_21", "rad_22", "rad_23"),
  Elevation = c(150, 150, 150, 150, 150, 150, 150, 150, 150),
  Source = "online",
  Lat = c(46.593132, 46.679370, 46.730593, 46.8, 46.7, 46.8, 46.6, 46.7, 46.8),
  Long = c(16.570974, 16.404680, 16.625895, 16.3, 16.4, 16.4, 16.5, 16.5, 16.5)
) %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE)

# Combine with existing stations
met_lst$stations <- rbind(met_lst$stations, stations_df)
mapview::mapview(met_lst$stations)

# Add temperature data
for(st in c('7', '11', '12', '21', '22', '23')){
  tmp <- read_meteo_file(paste0(d_pth, 'Kebele_tmp_', st, ".txt")) |> 
    separate(value, into = c("TMP_MIN", "TMP_MAX"), sep = "\t") |> 
    mutate(TMP_MIN = as.numeric(TMP_MIN), TMP_MAX = as.numeric(TMP_MAX)) |> 
    mutate(TMP_MIN = ifelse(TMP_MIN < -40 | TMP_MIN > 50, NA, TMP_MIN),
           TMP_MAX = ifelse(TMP_MAX < -40 | TMP_MAX > 50, NA, TMP_MAX)) |> 
    mutate(TMP_MIN = ifelse(TMP_MIN > TMP_MAX, NA, TMP_MIN),
           TMP_MAX = ifelse(TMP_MAX < TMP_MIN, NA, TMP_MAX)) |> 
    mutate(TMP_MIN = ifelse(is.na(TMP_MIN), -99, TMP_MIN),
           TMP_MAX = ifelse(is.na(TMP_MAX), -99, TMP_MAX))
  met_lst[["data"]][[paste0("ID", st)]][["TMP_MIN"]] <- tmp[, c("DATE", "TMP_MIN")]
  met_lst[["data"]][[paste0("ID", st)]][["TMP_MAX"]] <- tmp[, c("DATE", "TMP_MAX")]
}

plot_weather(met_lst, "TMP_MAX")
plot_weather(met_lst, "TMP_MIN")

# Add relative humidity data
for(st in c('7', '11', '12', '21', '22', '23')){
  tmp <- read_meteo_file(paste0(d_pth, 'Kebele_hum_', st, ".txt")) |> 
    rename(RELHUM = value) |> 
    mutate(RELHUM = ifelse(RELHUM > 1, RELHUM/100, RELHUM)) |> 
    mutate(RELHUM = ifelse(RELHUM < 0 | RELHUM > 1, -99, RELHUM)) |> 
    mutate(RELHUM = ifelse(is.na(RELHUM), -99, RELHUM))
  met_lst[["data"]][[paste0("ID", st)]][["RELHUM"]] <- tmp
}

plot_weather(met_lst, "RELHUM")

# Add precipitation data
for(st in c('7', '11', '12', '22', '23')){
  tmp <- read_meteo_file(paste0(d_pth, 'Kebele_prec_', st, ".txt")) |> 
    rename(PCP = value) |> 
    mutate(PCP = as.numeric(PCP)) |> 
    mutate(PCP = ifelse(PCP < 0 | PCP > 200, -99, PCP)) |>
    mutate(PCP = ifelse(is.na(PCP), -99, PCP))
  met_lst[["data"]][[paste0("ID", st)]][["PCP"]] <- tmp
}

tmp <- read_meteo_file(paste0(d_pth, 'Kebele_Lenti-Mahomfa_P.txt')) |> 
  rename(PCP = value) |> 
  mutate(PCP = as.numeric(PCP)) |> 
  mutate(PCP = ifelse(PCP < 0 | PCP > 200, -99, PCP)) |>
  mutate(PCP = ifelse(is.na(PCP), -99, PCP))
met_lst[["data"]][[paste0("ID4")]][["PCP"]] <- tmp

tmp <- read_meteo_file(paste0(d_pth, 'Kebele_Redics_P.txt')) |> 
  rename(PCP = value) |> 
  mutate(PCP = as.numeric(PCP)) |> 
  mutate(PCP = ifelse(PCP < 0 | PCP > 200, -99, PCP)) |>
  mutate(PCP = ifelse(is.na(PCP), -99, PCP))
met_lst[["data"]][[paste0("ID5")]][["PCP"]] <- tmp

tmp <- read_meteo_file(paste0(d_pth, 'Kebele_Szilvágy_P.txt')) |> 
  rename(PCP = value) |> 
  mutate(PCP = as.numeric(PCP)) |> 
  mutate(PCP = ifelse(PCP < 0 | PCP > 200, -99, PCP)) |>
  mutate(PCP = ifelse(is.na(PCP), -99, PCP))
met_lst[["data"]][[paste0("ID6")]][["PCP"]] <- tmp

plot_weather(met_lst, "PCP")
plot_weather(met_lst, "PCP", "month", "sum")

# Add wind speed data
for(st in c('7', '11', '12', '21', '22', '23')){
  tmp <- read_meteo_file(paste0(d_pth, 'Kebele_wind_', st, ".txt")) |> 
    rename(WNDSPD = value) |> 
    mutate(WNDSPD = as.numeric(WNDSPD)) |>
    mutate(WNDSPD = ifelse(WNDSPD < 0 | WNDSPD > 30, -99, WNDSPD)) |>
    mutate(WNDSPD = ifelse(is.na(WNDSPD), -99, WNDSPD))
  met_lst[["data"]][[paste0("ID", st)]][["WNDSPD"]] <- tmp
}

met_lst[["data"]][["ID1"]][["WNDSPD"]] <- NULL

plot_weather(met_lst, "WNDSPD")
plot_weather(met_lst, "WNDSPD", "month", "mean")

# Add solar radiation data
for(st in c('7', '11', '12', '21', '22', '23')){
  tmp <- read_meteo_file(paste0(d_pth, 'Kebele_rad_', st, ".txt")) |> 
    rename(SLR = value) |> 
    mutate(SLR = as.numeric(SLR)) |> 
    mutate(SLR = ifelse(SLR < 0 | SLR > 50, -99, SLR)) |>
    mutate(SLR = ifelse(is.na(SLR), -99, SLR))
  met_lst[["data"]][[paste0("ID", st)]][["SLR"]] <- tmp
} 

plot_weather(met_lst, "SLR")
plot_weather(met_lst, "SLR", "month", "mean")

saveRDS(met_lst, "Data/for_prepr/meteo_data_plus.rds")

##------------------------------------------------------------------------------
## For interpolation
##------------------------------------------------------------------------------
library(mapview)

## Load the latest meteorological data
met_lst <- readRDS("Data/for_prepr/meteo_data_plus.rds")

## Set the paths
data_path <- "Data/for_buildr/"
basin_path <- paste0(data_path, "CS6_WatBoundary.shp")
DEM_path <- paste0(data_path, "dem_copernicus.tif")
basin <- st_read(basin_path) |> st_transform(4326)

## Visual check of the existing stations and the basin
mapview(met_lst$stations) + mapview(basin)

## Transform stations to projected CRS for interpolation (should be in meters)
met_lst$stations <- met_lst$stations %>% 
  st_transform(3794) %>% 
  mutate(Long = st_coordinates(.)[, 1], Lat  = st_coordinates(.)[, 2])
st_crs(met_lst$stations)

# Interpolate meteorological data to selected grid
print(paste0("SWATprepR version should be not earlier than 1.0.12, currectly installed version is ", 
             as.character(packageVersion("SWATprepR"))))
met_lst_int <- SWATprepR::interpolate(met_lst, basin_path, DEM_path, 2000) 

# Visual check of the interpolated stations and the basin
stations <- st_transform(met_lst_int$stations, 4326)
mapview(stations) + mapview(basin)

# Plot some of the interpolated weather variables
plot_weather(met_lst_int, "PCP", "year", "sum")
plot_weather(met_lst_int, "PCP")
plot_weather(met_lst_int, "TMP_MAX")
plot_weather(met_lst_int, "TMP_MIN")
plot_weather(met_lst_int, "RELHUM")
plot_weather(met_lst_int, "WNDSPD")
plot_weather(met_lst_int, "SLR")

# Converting back coordinates
met_lst_int$stations <- met_lst_int$stations %>% 
  st_transform(4326) %>% 
  mutate(Long = st_coordinates(.)[, 1], Lat  = st_coordinates(.)[, 2])
st_crs(met_lst_int$stations)
# Save the interpolated meteorological data
saveRDS(met_lst_int, paste0(data_path, "meteo_data_int.rds"), compress = "xz")


