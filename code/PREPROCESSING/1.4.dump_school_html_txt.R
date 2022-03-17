here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

print("getting file list...................")
all_html_files <- list.files(glue("{here}/data/school_websites/html_files"), recursive = T, full.names = T, pattern = "html")

# all_html_files <- list.files(glue("{here}/data/learned_societies"), recursive = T, full.names = T, pattern = "html")

print(paste0("got ", length(all_html_files), " html files"))

all_txt_files <- list.files(glue("{here}/data/school_websites/html_files"), recursive = T, full.names = T, pattern = "txt")

print(paste0("got ", length(all_txt_files), " txt files"))

files_to_get <- all_html_files[all_html_files %notin% map_chr(all_txt_files, ~str_replace(.x, "\\.txt", "\\.html"))]

# Changing that because so many were poorly converted and I have to redo them. 
files_to_get <- all_html_files

print(paste0("left to get ", length(files_to_get), " html files"))

convert_to_text <- function(file){
  # file <- all_html_files[1]
  txt_out <- str_replace(file, "\\.html", "\\.txt")
  if(!file.exists(txt_out) | file.size(txt_out) == 0){
    # file.create(txt_out)
    # system(glue("html2text '{file}' > '{txt_out}'"))
    system(glue("lynx -dump '{file}' > '{txt_out}'"))
    print(paste0("creating ", txt_out))
    return()
  }else{
    print(paste0(txt_out, " exists"))
    return()
  }
  
}

print("starting to convert...............")
map(files_to_get, ~convert_to_text(.x))