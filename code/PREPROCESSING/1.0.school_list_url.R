#  ALREADY RUN. NO NEED TO RUN AGAIN. 

source("./code/setup.R")

top100 <- fread("./data/school_websites/university_info/rank_university.txt") |> 
  distinct(university, .keep_all = T)

all_schools <- fread("./data/school_websites/university_info/university_college_websites.csv") %>% 
  rename("university" = `School Name`) |> 
  distinct(university, .keep_all = T)

linked_data_set <- pair_blocking(top100, all_schools) %>%
  compare_pairs(by = c("university"),default_comparator = jaro_winkler(0.9)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()


linked_data_set <- linked_data_set %>% 
  filter(!is.na(university.x)) %>% 
  rename(university = university.x) %>% 
  select(-university.y) %>% 
  arrange(rank)

linked_data_set <- linked_data_set %>% arrange(rank)

linked_data_set %>% fwrite("./data/100_top_universities_urls.csv")