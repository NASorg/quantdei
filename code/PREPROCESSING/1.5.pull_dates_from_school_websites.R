################################################
# Inputs: txt files of school websites (converted from html in 1.4) downloaded after the JSON search files are returned (1.1-1.3). 
# Outputs: files_dates.csv, which is a very long list of all the txt files created from the HTML files and the dates that were extracted from it. 
# 
# Explanation: 
# Pulls dates off all of the downloaded pages and adds them to a csv file; 
# Further development: this needs a way of preventing duplicates additions to the CSV file. Wasn't strictly necessary for current usage, because it's straightforward enough to only apply it to a subset of HTML files. The first session in ~September-October 2021 is separate from the updated in January 2022. This ensures no double-up in the files parsed and added to the csv. 
################################################


here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))
library(tictoc)
library(furrr)

all_txt_files_full <- list.files(glue("{here}/data/school_websites/html_files"), recursive = T, full.names = T, pattern = "txt")
all_txt_files_short <- basename(all_txt_files_full)
all_txt_files_both <- as.data.table(cbind(all_txt_files_full, all_txt_files_short))

pull_dates <- function(file){
  # file <- all_txt_files_both$all_txt_files_full[1]
  # file <- glue("{here}/data/school_websites/html_files/yale/yale_white-supremacy_chemistry_e2bb1be8acf2e0cfd2d5bd5a76730ceb.txt")
  
  short_name <- basename(file)
  print(short_name)
  
  if(file.size(file) > 50000000 | file.size(file) < 100){
    return()
  }
  fulltext <- read_file(file)
  dates1 <- str_extract_all(fulltext, "\\b(3[01]|[12][0-9]|0?[1-9])/(1[0-2]|0?[1-9])/(?:[0-9]{2})?[0-9]{2}\\b")
  # dates2 <- str_extract_all(fulltext, "^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]|(?:Jan|Mar|May|Jul|Aug|Oct|Dec)))\1|(?:(?:29|30)(\/|-|\.)(?:0?[1,3-9]|1[0-2]|(?:Jan|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec))\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)(?:0?2|(?:Feb))\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9]|(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep))|(?:1[0-2]|(?:Oct|Nov|Dec)))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$")
  dates2 <- str_extract_all(fulltext, "\\b(((Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2},\\s+\\d{4}))\\b")
  
  if(length(dates1) == 0 && length(dates2) == 0){
    if(identical(dates1[[1]], character(0)) & identical(dates2[[1]], character(0))){
      return()
    }
  }
  
  res <- rbind(as.data.table(dates1[[1]]), as.data.table(dates2[[1]]))
  res <- as.data.table(cbind(res, short_name))
  names(res) <- c("dates", "short_name")
  fwrite(res, glue("{here}/data/school_websites/files_dates.csv"), append = T)
  return(res)

}
tic()
map(all_txt_files_both$all_txt_files_full, ~pull_dates(.x))
toc()
