require(searchConsoleR)
require(httr)
require(tidyverse)
require(data.table)
require(glue)
require(here)
require(janitor)
require(curl)
require(jsonlite)
require(lubridate)
require(googleway)
require(ggmap)

here <- here()

google_key <- Sys.getenv('google_key')
google_cx <- Sys.getenv('google_cx')

`%notin%` <- Negate(`%in%`)

# Get all schools thing
all_schools <- fread(glue("{here}/data/school_websites/university_info/top_100_schools_clean.csv"))[,school := str_remove(site, "\\.edu")]

# Get the files_dates4 that I'm using in the Rmd file. Unfortunately simplest way is to repeat all that code =(.
# This is pulled straight from the Rmd file, because all this code was only written after the report was half done and knitted 100 times. 

files_dates <- fread(glue("{here}/out/school_websites/files_dates.csv"))
files_dates[,dates := str_remove_all(dates, "\\n")]
files_dates[,school := str_extract(short_name, "^.*?(?=_)")]
files_dates[,dei_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][2])]
files_dates[,stem_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][3])]
files_dates[,good_date := parse_date(dates)]
files_dates1 <- files_dates[is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = mdy(dates))]
files_dates2 <- files_dates[!is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = as_date(good_date, "ymd"))]
files_dates3 <- rbind(files_dates1, files_dates2)
files_dates3 <- files_dates3[good_date > "1990-01-01" & good_date < "2021-10-01"]
files_dates3[, month := lubridate::floor_date(good_date, "month")]
one_date <- files_dates3[, .(count = .N), by = "short_name"][count == 1]
files_dates4 <- files_dates3[short_name %in% one_date$short_name]

ivy_c <- c("harvard", "yale", "princeton", "columbia", "upenn", "brown", "dartmouth", "cornell")
files_dates4[,ivy :=  ifelse(school %in% ivy_c, "ivy", "non-ivy")]
files_dates4[dei_term != "anti-racism",dei_term := str_replace_all(dei_term, "-", " ")]


files_dates_places <- files_dates4[all_schools, on = "school"]

register_google(key = google_key)


geolocations <- files_dates_places %>% 
  distinct(school, city, state)

lon_lat <- geolocations %>%  mutate(geocoded = map2_dfr(city, state, ~geocode(location = paste(.x, .y), 
                                             output = 'latlon', 
                                             source = "google", 
                                             force = TRUE, 
                                             language ="en",
                                             messaging=TRUE, 
                                             override_limit=TRUE)))





lon_lat$lon <- lon_lat$geocoded[1]
lon_lat$lat <- lon_lat$geocoded[2]

lon_lat %>% write_rds("./out/school_websites/geolocation_data.Rds")



