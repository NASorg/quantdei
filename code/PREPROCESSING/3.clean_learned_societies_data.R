here <- rprojroot::find_rstudio_root_file()
library(glue)
library(tictoc)
library(ggplot2)
source(glue("{here}/code/setup.R"))

dl_dir <- glue("{here}/data/professional_associations")

txt_files <- list.files(glue("{here}/data/learned_societies"), recursive = T, full.names = T, pattern = "txt")
dei_terms <- fread(glue("{here}/data/dei_terms.txt"), header = F, sep = NULL)
names(dei_terms) <- "term"
dei_terms[, term := str_to_lower(term)]


get_count <- function(file_in){
  # file_in <- txt_files[1]
  
  if(file.size(file_in) == 0){
    print(paste0(file_in, " empty"))
    return()
  }
  
  fulltext <- str_to_lower(read_file(file_in))
  total_chr <- nchar(fulltext)
  dei_count <- dei_terms[, count :=  map_int(term, ~stri_count_regex(fulltext, paste0("\\b",.x,"[\\b\\s+]")))]

  flip_dei_count <-  transpose(dei_count, make.names = TRUE)
  
  res <- cbind(file_in, flip_dei_count, total_chr)
  
  return(res)
  
}
tic()
dei_counts <- future_map_dfr(txt_files, ~get_count(.x))
toc()

dei_counts1 <- dei_counts[, `:=`(year = str_extract(file_in, "[0-9]{4}"), 
                  society = str_extract(file_in, "(?<=learned_societies/).*?(?=/)"))]

dei_counts1[, term_sum := rowSums(.SD), .SDcols = 2:32]
dei_counts1[, dei_sum := rowSums(.SD), .SDcols = c("diversity", "equity", "inclusion")]
dei_counts1[, race_sum := rowSums(.SD), .SDcols = c("anti-racism", "antiracism", "racism")]
dei_counts1[, gender_sum := rowSums(.SD), .SDcols = c("gender")]

society_full_names <- c("acs" = "Am. Chemical Society", "ams" = "Am. Mathematical Society", "apa" = "Am. Physical Association", "asbmb" = "Am. Society for \nBiochemistry and Molecular Biology ")
society_full_names <- cbind(as.data.table(names(society_full_names)), as.data.table(society_full_names))
names(society_full_names) <- c("society", "society_full_name")
dei_counts2 <- dei_counts1[society_full_names, on = "society"]


# Export it out. But I make further manipulations on the IMPORTED  --------

# dei_counts2 %>% fwrite("./data/learned_societies/dei_term_counts.csv")

dei_counts2 <- fread("./data/learned_societies/dei_term_counts.csv")
dei_counts2[society == "apa", bias := 0]
dei_counts2[society == "apa", inclusion := 0]

# dei_counts1 <- dei_counts[, `:=`(year = str_extract(file_in, "[0-9]{4}"), 
                                 # society = str_extract(file_in, "(?<=learned_societies/).*?(?=/)"))]

dei_counts2[, term_sum := rowSums(.SD), .SDcols = 2:32]
dei_counts2[, dei_sum := rowSums(.SD), .SDcols = c("diversity", "equity", "inclusion")]
dei_counts2[, race_sum := rowSums(.SD), .SDcols = c("anti-racism", "antiracism", "racism")]
dei_counts2[, gender_sum := rowSums(.SD), .SDcols = c("gender")]




dei_counts2 %>% fwrite("./out/learned_societies/dei_term_counts.csv")
