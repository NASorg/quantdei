library(xml2)
source("./code/setup.R")
arxiv_files <- list.files(glue("{here}/data/scholarship/arxiv/"), pattern = "xml", full.names = T)

MakeArxivFrame <- function(file){
  # file <- arxiv_files[3]
  # print(file)
  topic <- str_remove(basename(file), ".xml")
  topic <- str_replace_all(str_replace_all(topic, "\\+", " "), "AND", "")
  r1 <- as_list(read_xml(file))
  r2 <- as.data.table(as_tibble(r1) %>% unnest_longer(feed))
  if(nrow(r2) < 8) return()
  r3 <- r2[feed_id == "published", .(date = ymd(as.Date(str_extract(unlist(feed), ".*?(?=Z)"))))]
  r3$topic <- topic
  return(r3)
}

arxiv <- map_df(arxiv_files, ~MakeArxivFrame(.x))

arxiv %>% fwrite(., glue("{here}/out/scholarship/arxiv.csv"))
