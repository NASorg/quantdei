require(httr)
require(tidyverse)
require(data.table)
require(glue)
require(here)
require(janitor)
require(curl)
require(jsonlite)
require(lubridate)

here <- here()

search_terms <- c("racism", "systemic+racism", "white+supremacy" , "anti-racism", "antiracism", "social+justice", "privilege", "critical+race+theory", "intersectionality", "sexism", "diversity+AND+all:equity+AND+all:inclusion")

dl_dir <- glue("{here}/data/scholarship/arxiv/")

get_results <- function(term){
  search_url <- glue('http://export.arxiv.org/api/query?search_query=all:"{term}"&start=0&max_results=1000')  
  
  out_file <- glue("{dl_dir}{term}.xml")
  
  if(!exists(out_file)){
    curl_fetch_disk(search_url, out_file)
  }
}
map(search_terms, ~get_results(.x))


# xml_files <- list.files(dl_dir, full.names = T)
# 
# xml2::as_xml_document(xml_files[6])

# NOTE: The DEI search term did not work very well from  here -- i.e. parsing colons etc. So that was done manually in the browser, at the following link: http://export.arxiv.org/api/query?search_query=all:diversity+AND+all:equity+AND+all:inclusion&start=0&max_results=1000

# What is the simplest way to parse out this data? Just grep out the publication dates!

pub_dates <- as.data.table(system(glue("rg 'published>' {dl_dir}"), intern = T))
pub_dates <- pub_dates |> separate(V1, into = c("file", "pub_date"), sep = ":", extra = "drop")
pub_dates[,file := str_extract(file, "(?<=arxiv/).*?(?=\\.xml)")][]
pub_dates[,pub_date := str_remove_all(pub_date, "<published>|</published>")][]
pub_dates[,pub_date := str_extract(pub_date, "[0-9]{4}-[0-9]{2}-[0-9]{2}")][]
pub_dates[,pub_date := ymd(pub_date)]
pub_dates[,file := str_replace_all(file, "\\+", " ")][] |> fwrite("./out/scholarship/arxiv.csv")























