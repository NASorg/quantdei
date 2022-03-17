source("./code/setup.R")

 
json_files_w_path <- list.files(json_dir, full.names = T)
json_files_w_path <- json_files_w_path[json_files_w_path %like% "update"]  # Just for updates in Jan 2022. 
json_files_no_path <- basename(json_files_w_path)
json_files_both <- as.data.table(cbind(json_files_no_path, json_files_w_path))

funwrap <- function(dt, cols) {
  
  dt <- j3
  cols <- c("kind","title","htmlTitle","link","displayLink","snippet","htmlSnippet","cacheId","formattedUrl","htmlFormattedUrl","fileFormat","mime","json_file", "pagemap")
  
  un <- dt[, cols, with = FALSE][, .ID := .I][]
  un <- un[,
           append(
             list(.ID = rep(.ID, lengths(get(cols[[1]])))),
             stats::setNames(lapply(cols, function(vv) unlist(get(vv))), cols)
           )]
  
  # safeguard against empty values
  if (!all(cols %in% names(un))) un[, c(cols) := NA]
  
  rr <- un[dt[, !cols, with = FALSE][, .ID := .I], on = ".ID"][, .ID := NULL]
  setcolorder(rr, names(dt))
  rr[]
} # thanks https://stackoverflow.com/questions/44336733/how-to-unlist-a-column-in-a-data-table David but srsly


parse_json <- function(json_file){
  # json_file <- "yale_diversity_astronomy_9.json"
  # json_file <- "harvard_diversity_astronomy_1_update.json"
  # json_file <- json_files_no_path[1]
  
  
  
  print(json_file)
  json_file_name <- json_files_both[json_files_no_path == json_file]$json_files_w_path
  j1 <- read_json(json_file_name)
  try(j2 <- map_df(j1$items, ~as.data.table(.x)))
  if(nrow(j2) == 0){
    try(j2 <- map_df(j1, ~as.data.table(.x)))
  }
  # j3 <- j2 %>% distinct(link, .keep_all = T)
  # j3 <- unique(j2, by = "link")
  j3 <- j2
  if(length(j3$fileFormat) == 0){
    j3$fileFormat <- NA
  }
  if(length(j3$mime) == 0){
    j3$mime <- NA
  }
  if(length(j3$cacheId) == 0){
    j3$cacheId <- NA
  }
  j3 <- cbind(j3, json_file)
  
  
  # Obviated because I skip funwrap above and just do it in tidyverse.
  
  res3 <- j3 |> distinct(link, .keep_all = T)
  setDT(res3)
  res3 <- res3[,.(title,link,snippet,cacheId,json_file)]
  res3 <- res3 |> unnest(cols = c(title,link,snippet,cacheId,json_file))
  setDT(res3)
  res3[is.na(cacheId), cacheId := "NA"]
  
  res3 %>% fwrite("./data/school_websites/json_links.csv", append = T)
  # print(res3)
  return(res3)
}


# json_files_both[json_files == "yale_diversity_astronomy_8.json"]

res <- map_df(json_files_no_path, ~parse_json(.x))

str(res)
