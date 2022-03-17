require(searchConsoleR)
require(httr)
require(tidyverse)
require(data.table)
require(glue)
require(here)
require(janitor)
require(curl)
require(jsonlite)

here <- here()

google_key <- Sys.getenv('google_key')
google_cx <- Sys.getenv('google_cx')

`%notin%` <- Negate(`%in%`)

##################################################################################################
# Prepare the data --------------------------------------------------------
crt_terms <- c("diversity", "equity", "inclusion", "systemic racism", "white supremacy" , "anti-racism", "justice", "privilege", "critical race theory")
stem_terms <- c("astronomy", "biology", "chemistry", "engineering", "geology", "mathematics", "meteorology", "oceanography", "physics", "science")

# crt_terms <- c("diversity statement")

school_names <- fread(glue("{here}/data/school_websites/university_info/100_top_universities_urls.csv")) %>% clean_names()
school_names_to_remove <- read_lines(glue("{here}/data/school_websites/university_info/universities_to_remove.csv"))
school_names <- school_names[school_names$university %notin% school_names_to_remove]
school_names_to_add <- fread(glue("{here}/data/school_websites/university_info/universities_to_add.csv"))
school_names <- rbind(school_names, school_names_to_add)
school_names[,site:= str_remove(site, "www\\.")]
school_names[is.na(rank)]$rank <- 99
school_names <- data.table(school_names, key = "rank")
schools_to_search <- school_names[,.SD, by = rank]

search_combo <- rbindlist(cross3(schools_to_search$site, crt_terms, stem_terms))
names(search_combo) <- c("site", "crt_term", "stem_term")
search_combo <- search_combo[school_names, university := university, on = "site"]
search_combo[, id := .I]
setcolorder(search_combo, "id")

# Explanation
# Therefore the output file format is going to just be a combination of these terms, like:
# harvard_diversity_astronomy_1, olin_critical-race-theory_science_3, etc. (the number is for the page; hyphens to break up words)

##################################################################################################
# Make the function that does the actual search ---------------------------

GetSearches <- function(id_f, google_key = google_key, google_cx = google_cx, country = "us"){
  stem_term <- search_combo[id == id_f]$stem_term
  crt_term <- search_combo[id == id_f]$crt_term
  site <- search_combo[id == id_f]$site
  
  print(paste(site, crt_term, stem_term, sep = " | "))
  keyword <- glue('site:{site} "{crt_term}" "{stem_term}"')
  keyword <- str_replace_all(keyword, " ", "%20")
  
  file_name <- paste0(str_remove(site, "\\..*$"), "_", crt_term, "_", stem_term)
  file_name <- str_replace_all(file_name, " ", "-")
  
  for(page in 1){ # 1 was seq(1, 91, 10) because we wanted to traverse 10 pages; but now we're just going to do first page.
    # round_page <- round(page / 10) + 1
    json_out <- paste0(json_dir, "/", file_name, "_", page, "_update.json" ) # page was page
    
    if(file.exists(json_out)){
      print(paste0(json_out, " exists"))
      next
    }
  
    url1 <- glue("https://www.googleapis.com/customsearch/v1?key={google_key}&q={keyword}&gl={country}&hl=en&cx={google_cx}&start={page}&filter=0")
    print(url1)
    
    
    try(d2 <- read_json(url1))
    if(!exists("d2")){
      print("didnt' download properly")
      next
    }
    d3 <- map_df(d2$items, ~as.data.table(.x))
    d3_row <- nrow(d3)
    if(d3_row == 0){
      print(paste("page", page , "is empty"))
      break
    }
    jsonlite::write_json(d2, json_out)
    rm(d2)
    print(glue("searching on {site} and {crt_term} and {stem_term} now | page {page}"))
    return()
  }
}
# Do the search -----------------------------------------------------------

map_df(search_combo$id, ~GetSearches(.x, google_key, google_cx))