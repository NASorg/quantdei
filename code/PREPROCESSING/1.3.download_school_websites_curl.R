################################################
# Inputs: json_links.csv, the file created after cleaning up the raw json files from the google custom search API. 
# Outputs: a (very) large number of html files; the names of which are created by the unique URL identifier. These are then passed to 1.4 etc. 
################################################


here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))
# Make the folders --------------------------------------------------------

school_folders <- map_chr(schools_to_search$site, ~str_remove(.x, "\\..*$"))

make_folder <- function(school_name){
  if(!dir.exists(glue("{here}/data/school_websites/html_files/{school_name}"))){
    dir.create(glue("{here}/data/school_websites/html_files/{school_name}"))
    print(glue("creating {school_name} folder"))
  }else{
    print(glue("{school_name} folder exists")) 
  }
}

# It's done so no need to run it every time; but the actual school_folders var is useful so leave the above.
# map(school_folders, ~make_folder(.x))

# Get links ---------------------------------------------------------------

# hash them to solve the name problem
json_school_links <- fread(glue("{here}/data/school_websites/json_links.csv"))
json_school_links <- json_school_links[link %notlike% "pdf"]
json_school_links[, hash := map_chr(link, ~rlang::hash(.x))]
json1 <- unique(json_school_links, by = "hash") 

dl_dir <- glue("{here}/data/school_websites/html_files")

# Main dl func ------------------------------------------------------------

filetypes <- c("\\.pdf", "\\.doc", "\\.docx", "\\.xls", "\\.xlsx", "\\.ppt", "\\.html$", "\\.htm$")

got_hashes <- list.files(dl_dir, recursive = T, full.names = T)
got_hashes <- str_extract(got_hashes, "[0-9a-z]{32}")
pages_to_get <- json1[hash %notin% got_hashes]

get_html <- function(hash_f){
  # url <- "http://www.stanford.edu/~bgirod/pdfs/LiangVirginIsllands2002.pdf"
  # title <- "C:\\DOC\\Macro with diversity, Inflation Modelling and Beliefs\\Basic"
  # hash_f <- "b6a70aafc170420e1318b432bb4a3462"
  timenow <- strftime(Sys.time(), format="%Y%m%d--%H-%M-%S")
  
  file_name <- paste0(str_remove(json1[hash == hash_f]$json_file, "[0-9]\\.json|[0-9]_update\\.json"), hash_f, ".html")
  folder <- str_extract(json1[hash == hash_f]$json_file, "^.*?(?=_)")
  url <- json1[hash == hash_f]$link
  file_name_w_dir <- glue("{dl_dir}/{folder}/{file_name}")

  if(!file.exists(file_name_w_dir)){
    file.create(file_name_w_dir)
    try(curl_download(url, file_name_w_dir, mode = "wb"))
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


map(pages_to_get$hash, ~get_html(.x))

