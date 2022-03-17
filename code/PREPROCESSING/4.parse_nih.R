
source("./code/setup.R")
#Parsing the files right here... Done and code is outputted.

nih_files <- list.files(glue("{here}/data/grants/nih/"), full.names = T)
dt_tempout <- glue("{here}/out/grants/nih_parsed_all1.csv")
read_nih <- function(file){
  # file <- nih_files[11]
  print(file)
  file_name <- str_remove_all(file, glue("{here}/data/grants/nih/|/"))
  res <- fread(file, skip = 4, colClasses = "character")
  search_term <- str_remove_all(str_extract(file_name, "--.*?\\."), "--|\\.")
  res1 <- cbind(res, search_term, file_name)
  res1 <- res1 %>% clean_names()
  
  fwrite(res1, dt_tempout, append = T)
  return(res1)
}
map_df(nih_files, ~read_nih(.x))

nih_rewrite <- fread(dt_tempout)

# Using fst because it's much smaller and will fit on github. 
nih_rewrite %>% write_fst(., glue("{here}/out/grants/nih_parsed_all.fst"), 100) 

unlink(dt_tempout)