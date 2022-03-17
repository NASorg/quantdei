source("./code/setup.R")

gs_data <- list.files(glue("{here}/data/scholarship/google_scholar"), pattern = "csv", full.names = T)
gs_data <- gs_data[gs_data %notlike% "bak"]

ReadGsData <- function(file){
  # file <- gs_data[1]
  topic <- str_remove(basename(file), ".csv")
  topic <- str_replace_all(str_replace_all(topic, "_", " | "), "-", " ")
  r1 <- fread(file) %>% clean_names()
  r1 <- r1[!is.na(year)]
  r1$topic <- topic
  return(r1)
}

gs <- map_df(gs_data, ~ReadGsData(.x))

gs %>% write_fst(., glue("{here}/out/scholarship/google_scholar.fst"), 100)
