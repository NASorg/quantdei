library(glue)
here <- rprojroot::find_rstudio_root_file()

source(glue("{here}/code/setup.R"))

file_dir <- glue("{here}/data/grants/nsf")

# Explanation -------------------------------------------------------------------------
# Awards.csv was downloaded from the NSF grants website ADDRESS on DATE. This is a simple analysis of the results in there that relate to DEI issues.

#################

library(xml2)

all_xmls <- list.files(file_dir, full.names = T, pattern = "xml")

read_file

xml2::read_xml(all_xmls[1])

parse_xml <- function(xml){
  # xml <- all_xmls[129]
  try(xml_as_list <- xml2::as_list(read_xml(xml)))
  if(!exists("xml_as_list")){
    return(NA)
  }
  
  title <- xml_as_list$rootTag$Award$AwardTitle[[1]][1]
  funder <- xml_as_list$rootTag$Award$AGENCY[[1]][1]
  fund_date <- xml_as_list$rootTag$Award$AwardEffectiveDate[[1]][1]
  amount <- xml_as_list$rootTag$Award$AwardAmount[[1]][1]
  try(narrative <- xml_as_list$rootTag$Award$AbstractNarration[[1]][1])
  
  if(!exists("narrative")){
    narrative <- NA
  }
  
  res <- as.data.table(cbind(title, funder, fund_date, amount, narrative))
  
  fwrite(res, paste0(file_dir, "/nsf/nsf_all_grants_clean.csv"), append = TRUE)
  
  return(res)
}

res <- future_map_dfr(all_xmls, ~parse_xml(.x))
# res %>% fwrite(paste0(file_dir, "/all_grants_clean1.csv"))
nsf <- fread(paste0(file_dir, "/nsf_all_grants_clean.csv"))
test2 <- test1[1:10]



# Basic boolean values ----------------------------------------------------
# This will allow storing the fully array of DEI terms per narrative
dei_terms <- read_lines(glue("{here}/data/dei_terms.txt"))
exclude_terms <- c("inclusion")
dei_terms <- dei_terms[dei_terms %notin% exclude_terms]

for(term in dei_terms){
  nsf[,paste0(term) := ifelse(str_detect(narrative, paste0("\\b", term, "\\b")), 1, 0)]
}

nsf[,-c("narrative")] %>% fwrite(., paste0(here, "/data/grants/nsf/nsf_all_grants_summary_data.csv"))


# DEI term variable -------------------------------------------------------
# this is not as comprehensive but will allow faceting the data 
# test <- sample_n(nsf, 1000)

pull_dei_term <- function(narrative){
  # narrative <- nsf[1]$narrative
  res <- map_chr(dei_terms, ~str_extract(narrative, paste0("\\b", .x, "\\b")))
  res <- res[!is.na(res)][1]
  res <- as.data.table(res)
  return(res)
}

nsf[, dei_term := map_df(narrative, ~pull_dei_term(.))]

nsf[,-c("narrative")] %>% fwrite("./data/grants/nsf/nsf_all_grants_summary_data_w_dei_terms.csv")



# test1[!is.na(narrative)] %>% nrow


# nsf <- fread("./data/nsf/Awards.csv")
# 
# nsf <- nsf %>% mutate(StartDate = mdy(StartDate))
# options(datatable.prettyprint.char=20L)
# nsf %>% 
#   count(StartDate) %>% 
#   mutate(year = year(StartDate)) %>% 
#   filter(year > 1980 & year < 2021) %>% 
#   ggplot() +
#   theme_classic() +
#   geom_col(aes(year, n)) +
#   labs(title = "Number of NSF grants in the mathematical and physical sciences that mention 'race'")
# 
# 
# 
# nsf %>% 
#   mutate(year = year(StartDate)) %>% 
#   filter(year > 1990 & year < 2021) %>% 
#   mutate(amount_awarded = parse_number(AwardedAmountToDate)) %>% 
#   group_by(year) %>% 
#   summarise(dollars_per_year = sum(amount_awarded)) %>% 
#   ggplot() +
#   theme_classic() +
#   geom_col(aes(year, dollars_per_year)) +
#   labs(title = "Size of all NSF grants in the mathematical and physical sciences that mention 'race'", y = "Total grant size") +
#   scale_y_continuous(labels=dollar_format())
#   
# 
# nsf %>% 
#   mutate(year = year(StartDate)) %>% 
#   filter(year > 1990 & year < 2021) %>% 
#   mutate(amount_awarded = parse_number(AwardedAmountToDate)) %>% 
#   # group_by(year) %>% 
#   # summarise(dollars_per_year = sum(amount_awarded)) %>% 
#   ggplot() +
#   theme_classic() +
#   geom_jitter(aes(year, amount_awarded)) +
#   labs(title = "Size of individual NSF grants in the mathematical and physical sciences that mention 'race'", y = "Individual grant size") +
#   scale_y_continuous(labels=dollar_format())
# 
