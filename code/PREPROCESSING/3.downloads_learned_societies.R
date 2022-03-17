here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

dl_dir <- glue("{here}/data/learned_societies")

# We'll do physics, chemistry, biology, mathematics

# physics: American Physical Society ------------------------------------------------
# url structure like: https://flux.aps.org/meetings/YR07/APR07/all_APR07.pdf

years <- str_pad(seq(7, 21), 2, pad = 0)
months <- c("MAR", "APR")
yearmonth <- cross2(years, months) %>% rbindlist 
names(yearmonth) <- c("year", "month")

make_url <- function(year_f, month_f){
  url <- glue("https://flux.aps.org/meetings/YR{year_f}/{month_f}{year_f}/all_{month_f}{year_f}.pdf")
}

yearmonth[,url := map2_chr(yearmonth$year, yearmonth$month, ~make_url(.x, .y))]

get_pdf <- function(url_f){
  folder <- "apa"
  # year <- yearmonth[url = url_f]$year
  # month <- yearmonth[url = url_f]$month
  # url_f <- yearmonth$url[1]
  timenow <- strftime(Sys.time(), format="%Y%m%d--%H-%M-%S")
  
  file_name <- str_extract(url_f, "(?<=_).*$")
  file_name_w_dir <- glue("{dl_dir}/{folder}/{file_name}")
  print(glue("{file_name} | writing  | {timenow}"))
  
  if(!file.exists(file_name_w_dir)){
    file.create(file_name_w_dir)
    try(curl_download(url_f, file_name_w_dir, mode = "wb"))
    print(glue("{file_name} | wrote    | {timenow}"))
    return()
  }else{
    print(glue("{file_name} | exists   | {timenow}"))
    return()
  }
  
  if(file.size(file_name_w_dir) < 2){
    print(paste0(file_name_w_dir, " bad file"))
  }
}

map(yearmonth$url, ~get_pdf(.x))

# chemistry ---------------------------------------------------------------


