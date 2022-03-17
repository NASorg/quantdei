source("./code/setup.R")

pubmed_files <- list.files(glue("{here}/data/scholarship/pubmed"), pattern = "csv", full.names = T)
pubmed_files <- pubmed_files[pubmed_files %like% "set"]

ParsePubMedFile <- function(file){
  # file <- pubmed_files[2]
  topic <- str_remove(basename(file), ".csv")
  topic <- str_replace_all(str_extract(topic, "(?<=csv-).*?(?=-set)"), "_", " ")
  r <- fread(file)
  r$topic <- topic
  return(r)
}

pubmed <- map_df(pubmed_files, ~(ParsePubMedFile(.x)))
pubmed <- pubmed %>% clean_names()
pubmed %>% write_fst(., glue("{here}/out/scholarship/pubmed.fst"), 100)

