met_lst <- readRDS("Data/for_prepr/meteo_data.rds")

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


stations_df <- data.frame(
  ID = paste0("ID", c(4, 5, 6, 7, 11, 12, 21, 22, 23)),
  Name = c("Lenti-Mahomfa", "Redics", "Szilvágy", "rad_7", "rad_11", 
           "rad_12", "rad_21", "rad_22", "rad_23"),
  Elevation = c(150, 150, 150, 150, 150, 150, 150, 150, 150),
  Source = "online",
  Lat = c(46,593132, 46.68, 46.65),
  Long = c(16.47, 16.39, 16.19)
) %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE)