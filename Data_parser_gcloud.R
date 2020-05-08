library(tidyverse)
library(lubridate)
library(stinepack)
library(RPostgreSQL)
library(rpostgis)
library(rgdal)

basedatadir  <- "./data"
get_path <- function(filename) return(paste(basedatadir, "/", filename, sep=""))
add_path <- function(path, filename) return(paste(path, "/", filename, sep=""))

parse_and_serialize <- function(filename="data_parsed/sensor_data_2018.csv") {
  dat <- parse_data()
  write_data_csv(dat, filename)
}

# Read all data.
parse_data <- function() {

  # Read road weather sensor data
  dat_rws <- parse_gcloud_fmi() %>%
    mutate(mobile = FALSE)   # stationary sensors

  # read teconer mobile sensor data
  dat_teconer <- parse_gcloud_teconer() %>%
    mutate(mobile = TRUE)  # mobile sensors

  dat_rws %>%
    bind_rows(dat_teconer) %>%
    arrange(tstamp) ->
    dat

  return(dat)
}


# Teconer data ------------------------------------------------------------


# Calibrate as per Lovén et al, 2019: "Mobile road weather sensor calibration with linear mixed models"
calibrate <- function(tsurf, road_state) {
  b0 <- list(
    "1" = -0.03,
    "2" = -0.79,
    "3" = -0.96,
    "4" = -1.01,
    "5" = -0.97,
    "6" = -1.00
  )
  b1 <- list(
    "1" = 1.13,
    "2" = 1.02,
    "3" = 1.04,
    "4" = 0.96,
    "5" = 0.97,
    "6" = 0.91
  )
  #print(paste(tsurf, road_state, sep =" "))
  b0[[road_state]] + b1[[road_state]] * tsurf
}

# Read teconer data
parse_gcloud_teconer <- function(filename = "data/teconer-2018-gcloud.tsv") {
  dat_teconer <- read_tsv(filename,
                          na = "NULL",
                          col_names = c("time","date",
                                        "sensorid",
                                        "tsurf", "road_state",
                                        "lon", "lat"),
                          col_types = cols(.default = col_double(),
                                           time = col_character(), date = col_character(),
                                           sensorid = col_character(),
                                           tsurf = col_double(),
                                           road_state = col_character(),
                                           lon = col_double(), lat = col_double()),
                          n_max = Inf) %>%
    drop_na() %>%  # filter out all rows with NA's
    filter(lon > 24.607508) %>%
    filter(lon < 25.223729) %>%
    filter(lat > 60.104564) %>%
    filter(lat < 60.36975) %>%
    mutate(tstamp = parse_datetime(paste(date, time),
                                 format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(sensorid=stringr::str_sub(sensorid, start=1, end=3)) %>%
    select(-date, -time) %>%
    mutate(tsurf =  map2_dbl(.x = tsurf, .y = road_state, ~calibrate(.x, .y))) %>%   #calibrate
    select(-road_state) %>%
    select(tstamp, sensorid, tsurf, lat, lon) %>%
    arrange(tstamp)

  return(dat_teconer)
}


# RWS data ----------------------------------------------------------------


# Read data from FMI RWSs, stored on gcloud.
parse_gcloud_fmi <- function(filename = "data/fmi-2018-gcloud.tsv") {

  dat <-
    read_tsv(
      filename,
      na = "NULL",
      col_names = c("tstamp",
                    "lat", "lon",
                    "tsurf", "drop1", "drop2"),
      col_types = cols(
        .default = col_double(),
        tstamp = col_character(),
        tsurf = col_double(),
        lat = col_double(), lon = col_double()
      ),
      n_max = Inf
    ) %>%
    select(-drop1, -drop2) %>%
    drop_na() %>%  # filter out all rows with NA's
    filter(lon > 24.607508) %>%
    filter(lon < 25.223729) %>%
    filter(lat > 60.104564) %>%
    filter(lat < 60.36975) %>%
    mutate(tstamp = parse_datetime(tstamp,
                                   format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(sensorid = paste(lon,lat)) %>%
    select(tstamp, sensorid, tsurf, lat, lon) %>%
    arrange(tstamp)

  # At times, a sensor/rws might give multiple observations for a time stamp.
  # If the standard deviation of these observations is above 3, we discard
  # the observations.
  dat %>%
    mutate(row_num = row_number()) %>%
    group_by(sensorid, tstamp) %>%
    mutate(
      n = n(),
      sd = sd(tsurf),
      row_num = c(row_num)
    ) %>%
    ungroup() %>%
    select(n, sd, row_num) %>%
    drop_na() %>%
    filter(sd > 3) %>%
    pull(row_num) ->
    problem_rows

  return(dat %>% slice(-problem_rows))
}

# Serialization ------------------------------------------------------------

save_data <- function(data, out_file = get_path("sensor_data_full.dat")) {
  saveRDS(data, out_file)
}

read_data <- function(in_file = get_path("sensor_data_full.dat")) {
  return(readRDS(in_file))
}

write_data_csv <- function(dat, filename) {
  write_csv(dat, filename)
}

