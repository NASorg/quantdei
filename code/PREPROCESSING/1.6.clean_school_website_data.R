################################################
# Inputs: files_dates.csv, which is a list of all dates extracted from individual school web pages.
# Outputs: ggplot graphs visualizing this data.
# What it does: More of a test run. 
################################################

here <- rprojroot::find_rstudio_root_file()
library(glue)
library(tictoc)
library(ggplot2)
library(lubridate)
source(glue("{here}/code/setup.R"))

files_dates <- fread(glue("{here}/out/school_websites/files_dates.csv"))
files_dates[,dates := str_remove_all(dates, "\\n")]
files_dates[,school := str_extract(short_name, "^.*?(?=/)")][]
files_dates[, dei_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][2])]
files_dates[, stem_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][3])]
files_dates[, good_date := parse_date(dates)]

files_dates1 <- files_dates[is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = mdy(dates))]
files_dates2 <- files_dates[!is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = as_date(good_date, "ymd"))]
files_dates3 <- rbind(files_dates1, files_dates2)

# Just make sure these are good. Run this a bunch of times and eyeball it. 
# sample_n(files_dates3[,.(dates, good_date)], 100)
# files_dates4 <- as_tibble(files_dates3)

files_dates3 <- files_dates3[good_date > "1990-01-01" & good_date < "2021-10-01"]
files_dates3[, month := lubridate::floor_date(good_date, "month")]

setorder(files_dates3[, .(count = .N), by = "short_name"], -count)[]

one_date <- files_dates3[, .(count = .N), by = "short_name"][count == 1]

files_dates4 <- files_dates3[short_name %in% one_date$short_name]

files_dates4[month == "2021-09-01"] %>% count(short_name) %>% arrange(desc(n))

files_dates4[good_date > "2010-01-01"] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  # arrange(desc(count))
  ggplot(aes(x = month, y = count)) +
  geom_line() + 
  theme_classic() +
  labs(title = "Mentions of both STEM and DEI terms on university websites over time",
       x = "time", y = "count")
