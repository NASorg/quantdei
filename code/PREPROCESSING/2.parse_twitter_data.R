here <- rprojroot::find_rstudio_root_file()
library(glue)
library(tictoc)
library(ggplot2)
source(glue("{here}/code/setup.R"))

rds_files <- list.files(glue("{here}/data/twitter/twitter_account_dei_term"), full.names =T)

parse_feed <- function(file){
  # file <- rds_files[2]
  if(file.size(file) < 100){
    return()
  }
  
  r <- readRDS(file)
  
  if(!nrow(r) > 1){
    return()
  }
  
  r <- r[,c("text", "created_at", "id")]
  r <- cbind(r, file)
  return(r)
}
tic()
tweets_clean <- future_map_dfr(rds_files, ~parse_feed(.x))
toc()

# tweets_clean %>% fwrite("./data/twitter/tweets_clean.csv")