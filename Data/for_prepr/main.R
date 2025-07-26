library(tidyverse)
library(SWATrunR)
library(sf)

my_path <- "meteorological_K/"
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
  ID = paste0("ID", c(1, 2, 3)),
  Name = c("Len", "Kob", "Rak"),
  Elevation = c(190, 185, 187),
  Source = "online",
  Lat = c(46.56, 46.68, 46.65),
  Long = c(16.47, 16.39, 16.19)
) %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = FALSE)

mapview::mapview(stations_df)

# Convert to sf object and store in list
met_lst <- list()
met_lst[["stations"]] <- stations_df


hmd <- read_meteo_file(paste0(my_path, "Len_hmd.txt")) %>%
  rename(RELHUM = value) %>%
  mutate(RELHUM = ifelse(RELHUM > 1, RELHUM/100, RELHUM))

ggplot(hmd, aes(x = DATE, y = RELHUM)) +
  geom_line() +
  labs(title = "Relative Humidity", x = "Date", y = "Relative Humidity (%)") +
  theme_minimal()

met_lst[["data"]][["ID1"]][["RELHUM"]] <- hmd

pcp2 <- read_meteo_file(paste0(my_path, "Kob_pcp.txt")) %>%
  rename(PCP = value)

pcp1 <- read_meteo_file(paste0(my_path, "Len_pcp.txt")) %>%
  rename(PCP = value)

ggplot(pcp1, aes(x = DATE, y = PCP)) +
  geom_line() +
  labs(title = "Prec", x = "Date", y = "Precipitaiton mm/d") +
  theme_minimal()

met_lst[["data"]][["ID1"]][["PCP"]] <- pcp1
met_lst[["data"]][["ID2"]][["PCP"]] <- pcp2

tmp1 <- read_meteo_file(paste0(my_path, "Len_tmp.txt"))
ggplot(tmp1) +
  geom_line(aes(x = DATE, y = TMP_MIN), color = "blue") +
  geom_line(aes(x = DATE, y = TMP_MAX), color = "red") +
  labs(title = "TMP", x = "Date", y = "TMP") +
  theme_minimal()

met_lst[["data"]][["ID1"]][["TMP_MIN"]] <- tmp1[c("DATE", "TMP_MIN")]
met_lst[["data"]][["ID1"]][["TMP_MAX"]] <- tmp1[c("DATE", "TMP_MAX")]

wnd <- read_meteo_file(paste0(my_path, "Len_wnd.txt")) %>%
  rename(WNDSPD = value)

ggplot(wnd, aes(x = DATE, y = WNDSPD)) +
  geom_line() +
  labs(title = "Wind Speed", x = "Date", y = "Wind Speed (m/s)") +
  theme_minimal()

met_lst[["data"]][["ID1"]][["WNDSPD"]] <- wnd

slr <-  read_meteo_file(paste0(my_path, "Rak_slr.txt")) %>%
  rename(SLR = value)

ggplot(slr %>% filter(SLR>0), aes(x = DATE, y = SLR)) +
  geom_line() +
  labs(title = "Solar Radiation", x = "Date", y = "Solar Radiation (MJ/m^2)") +
  theme_minimal()
met_lst[["data"]][["ID3"]][["SLR"]] <- slr

saveRDS(met_lst, "meteo_data.rds")
