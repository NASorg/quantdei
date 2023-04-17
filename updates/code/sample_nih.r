require(dplyr)
require(tidyverse)
require(glue)
library(janitor)
require(here)
library(fst)

here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))

# Load NIH Data & Graphs -------------------------------------------------------

nih <- read_fst(glue("{here}/out/grants/nih_parsed_all.fst"))
nih <- as.data.table(nih)

nih[search_term != "discrimination"] %>% 
  count(search_term, year = fiscal_year) %>% 
  mutate(search_term = ifelse(search_term == "anti-racism", "anti-racism", str_replace_all(search_term, "-", " "))) %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~search_term, ncol = 2, scales = "free") +
  geom_line(size = 1) + 
  geom_jitter() +
  #theme_classic() +
  labs(title = "Fig 27. All NIH grants with DEI-related terms by year",
       x = "year", y = "count")


# Find NIH Duplicates ----------------------------------------------------------
# Filter by Project Number, Serial Number, and DUNS Number ---------------------

dupes_nih_all_edited <- nih %>%
  get_dupes(project_number, serial_number, duns_number) %>% 
  mutate(agency="nih") %>% 
  relocate(agency,.before=1)

nih[!search_term %like% "discrimination"] %>%
  distinct(project_number, serial_number, duns_number, .keep_all = T) %>%
  #distinct(project_title, program_official_information, project_start_date, .keep_all = T) %>%
  count(search_term, year = fiscal_year) %>%
  mutate(search_term = ifelse(search_term == "anti-racism", "anti-racism", str_replace_all(search_term, "-", " "))) %>%
  arrange(year, n) %>%
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~search_term, ncol = 2, scales = "free") +
  geom_line(size = .75) +
  geom_jitter() +
  # theme_classic() +
  labs(
    title = "Fig 27. All NIH grants with DEI-related terms by year (Revised Using NAS's Filter)",
    x = "year", y = "count"
  )


# Find NIH Duplicates ----------------------------------------------------------
# Use Bruna's Original Filter --------------------------------------------------

# Bruna's Original Duplication Search
dupes_nih_all_original <- nih %>%
  get_dupes(project_title, program_official_information, project_start_date) %>% 
  mutate(agency="nih") %>% 
  relocate(agency,.before=1)

nih[!search_term %like% "discrimination"] %>%
  #distinct(project_number, serial_number, duns_number, .keep_all = T) %>%
  distinct(project_title, program_official_information, project_start_date, .keep_all = T) %>%
  count(search_term, year = fiscal_year) %>%
  mutate(search_term = ifelse(search_term == "anti-racism", "anti-racism", str_replace_all(search_term, "-", " "))) %>%
  arrange(year, n) %>%
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~search_term, ncol = 2, scales = "free") +
  geom_line(size = .75) +
  geom_jitter() +
  # theme_classic() +
  labs(
    title = "Fig 27. All NIH grants with DEI-related terms by year (Revised Using Bruna's Filter)",
    x = "year", y = "count"
  )
