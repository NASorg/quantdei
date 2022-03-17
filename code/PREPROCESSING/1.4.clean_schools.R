################################################
# Inputs: 100_top_universities_url.csv, universities_to_remove.csv, universities_to_add.csv 
# Outputs: top_100_schools_clean.csv 
# This is a basic operation to clean up and combine the input files to create the final list of school websites/names. 
################################################


here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

school_names <- fread(glue("{here}/data/school_websites/university_info/100_top_universities_urls.csv")) %>% clean_names()
school_names_to_remove <- read_lines(glue("{here}/data/school_websites/university_info/universities_to_remove.csv"))
school_names <- school_names[school_names$university %notin% school_names_to_remove]
school_names_to_add <- fread(glue("{here}/data/school_websites/university_info/universities_to_add.csv"))
school_names <- rbind(school_names, school_names_to_add)
school_names[,site:= str_remove(site, "www\\.")]
school_names[is.na(rank)]$rank <- 99
school_names <- data.table(school_names, key = "rank")
schools_to_search <- school_names[,.SD, by = rank]
fwrite(schools_to_search, "./data/school_websites/university_info/top_100_schools_clean.csv")