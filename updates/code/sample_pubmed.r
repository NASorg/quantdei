# Much of the filtering used here was first created by Dr. Emilio Bruna at the University of Florida.
# Mason Goad and Bruce R. Chartwell thank Dr. Bruna for these contributions. 
# Dr. Bruna's original code can be found at: https://github.com/BrunaLab/quantdei_nas

require(tidyverse)
require(glue)
require(here)
require(janitor)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))


# Load PubMed Data Set ---------------------------------------------------------

pm <- read_fst(glue("{here}/out/scholarship/pubmed.fst")) %>%
  as_tibble() %>%
  mutate_all(tolower) %>%
  rename(year = publication_year, source = journal_book)


# Find Duplicates --------------------------------------------------------------

pm_dupes <- pm %>% get_dupes(title, source, year)


# Consolidate Duplicates -------------------------------------------------------

pm_deduped <- pm %>% distinct(title, source, year, .keep_all = T)


# Find False Positives ---------------------------------------------------------

pm_nondei <- pm_deduped %>%
  filter(
    (
      str_detect(title, "breastfeeding") |
        str_detect(title, "stroke") |
        str_detect(title, " liver ") |
        str_detect(title, " lungs ") |
        str_detect(title, "gene therapy") |
        str_detect(title, "cancer") |
        str_detect(title, "hepatitis") |
        str_detect(title, "hiv") |
        str_detect(title, "covid") |
        str_detect(title, "systemic complications") |
        str_detect(title, "research bias") |
        str_detect(title, "tuberculosis") |
        str_detect(title, "blood pressure") |
        str_detect(title, "nutrition")
    ) == TRUE
  ) %>%
  filter(str_detect(title, "1619 project") == FALSE) %>%
  filter(str_detect(title, "advocacy") == FALSE) %>%
  filter(str_detect(title, "ally") == FALSE) %>%
  filter(str_detect(title, "anti-racism") == FALSE) %>%
  filter(str_detect(title, "antiracism") == FALSE) %>%
  filter(str_detect(title, "systemic bias") == FALSE) %>%
  filter(str_detect(title, "racial bias") == FALSE) %>%
  filter(str_detect(title, "black lives") == FALSE) %>%
  filter(str_detect(title, "black lives matter") == FALSE) %>%
  filter(str_detect(title, "blm") == FALSE) %>%
  filter(str_detect(title, "BLM") == FALSE) %>%
  filter(str_detect(title, "civil rights") == FALSE) %>%
  filter(str_detect(title, "critical race theory") == FALSE) %>%
  filter(str_detect(title, "culturally sensitive") == FALSE) %>%
  filter(str_detect(title, "discrimination") == FALSE) %>%
  filter(str_detect(title, "diversity") == FALSE) %>%
  filter(str_detect(title, "diverse") == FALSE) %>%
  filter(str_detect(title, "equity") == FALSE) %>%
  filter(str_detect(title, "equality") == FALSE) %>%
  # filter(str_detect(title, "gender") == FALSE) %>%
  filter(str_detect(title, "george floyd") == FALSE) %>%
  filter(str_detect(title, "inequality") == FALSE) %>%
  filter(str_detect(title, "implicit bias") == FALSE) %>%
  filter(str_detect(title, "indigenous") == FALSE) %>%
  filter(str_detect(title, "inclusion") == FALSE) %>%
  filter(str_detect(title, "inclusive") == FALSE) %>%
  filter(str_detect(title, "intersectional") == FALSE) %>%
  filter(str_detect(title, "justice") == FALSE) %>%
  filter(str_detect(title, "kendi") == FALSE) %>%
  filter(str_detect(title, "microaggression") == FALSE) %>%
  filter(str_detect(title, "multicultural") == FALSE) %>%
  filter(str_detect(title, "oppression") == FALSE) %>%
  filter(str_detect(title, "privilege") == FALSE) %>%
  # filter(str_detect(title, "race") == FALSE) %>%
  filter(str_detect(title, "racism") == FALSE) %>%
  # filter(str_detect(title, "racial") == FALSE) %>%
  filter(str_detect(title, "racist") == FALSE) %>%
  filter(str_detect(title, "reform") == FALSE) %>%
  filter(str_detect(title, "social justice") == FALSE) %>%
  filter(str_detect(title, "social change") == FALSE) %>%
  filter(str_detect(title, "systemic racism") == FALSE) %>%
  filter(str_detect(title, "transgender") == FALSE) %>%
  filter(str_detect(title, "transgenderism") == FALSE) %>%
  filter(str_detect(title, "underrepresented") == FALSE) %>%
  filter(str_detect(title, "white fragility") == FALSE) %>%
  filter(str_detect(title, "white supremacy") == FALSE) %>%
  filter(str_detect(title, "white privilege") == FALSE) %>%
  mutate(database = "PubMed")


# Remove False Positives From Data Set -----------------------------------------

pm_deduped_2 <- anti_join(pm_deduped, pm_nondei)


# Create Original Graphs -------------------------------------------------------

pm %>%
  mutate(
    create_date = ymd(create_date),
    year = floor_date(create_date, "year")
  ) %>%
  count(topic, year) %>%
  filter(year > "2010-01-01" & year < "2022-01-01") %>%
  ggplot(., aes(x = year, y = n)) +
  geom_line(size = 1) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(
    title = "Original PubMed Results For DEI Terms (Recent)",
    x = "year", y = "count"
  ) +
  # theme_classic()  +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_date(
    breaks = function(x) {
      seq.Date(
        from = as.Date("2000-01-01"),
        to = as.Date("2021-01-01"),
        by = "2 years"
      )
    }, date_labels = "%y",
    expand = c(0, 0)
  )

pm %>%
  mutate(
    create_date = ymd(create_date),
    year = floor_date(create_date, "year")
  ) %>%
  count(topic, year) %>%
  ggplot(., aes(x = year, y = n)) +
  geom_line(size = 1) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(
    title = "Original PubMed results for DEI terms (Longitudinal)",
    x = "year", y = "count"
  ) #+
# theme_classic()


# Create Comparison Graphs -----------------------------------------------------

pm_deduped_2 %>%
  mutate(
    create_date = ymd(create_date),
    year = floor_date(create_date, "year")
  ) %>%
  count(topic, year) %>%
  filter(year > "2010-01-01" & year < "2022-01-01") %>%
  ggplot(., aes(x = year, y = n)) +
  geom_line(size = 1) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(
    title = "Cleaned PubMed Results For DEI Terms (Recent)",
    x = "year", y = "count"
  ) +
  # theme_classic()  +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_date(
    breaks = function(x) {
      seq.Date(
        from = as.Date("2000-01-01"),
        to = as.Date("2021-01-01"),
        by = "2 years"
      )
    }, date_labels = "%y",
    expand = c(0, 0)
  )

pm_deduped_2 %>%
  mutate(
    create_date = ymd(create_date),
    year = floor_date(create_date, "year")
  ) %>%
  count(topic, year) %>%
  ggplot(., aes(x = year, y = n)) +
  geom_line(size = 1) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(
    title = "Cleaned PubMed results for DEI terms (Longitudinal)",
    x = "year", y = "count"
  ) #+
# theme_classic()





# Extra Code ===================================================================
# ==============================================================================
# ==============================================================================

pubmed_control <- fread(glue("{here}/out/scholarship/PubMed_total_records_by_publication_year.csv")) %>% clean_names()

pubmed_control %>%
  mutate(
    year = paste0(citation_year, "-01-01"),
    year = ymd(year),
    year = floor_date(year, "year")
  ) %>%
  filter(year > "1950-01-01" & year < "2022-01-01") %>%
  rename(count = x2022) %>%
  ggplot(., aes(x = year, y = count)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Fig 34. Growth of PubMed database since 1950",
    x = "year", y = "count"
  ) #+
# theme_classic()



# Seeing that the entire PubMed database is related to bio medical science,
# nothing we pulled can be said to be unrelated to STEM.

# We have left this section of Dr. Bruna's code up for those who are curious
# as to the 2,100 objects (out of 73,000+) that it filtered from the data set.

# pm_sources_not_stem <- pm_deduped %>%
# filter(
# (
# str_detect(source, "law") |
# str_detect(source, "race") |
# str_detect(source, "class") |
# str_detect(source, "education") |
# str_detect(source, "teacher") |
# str_detect(source, "urban") |
# str_detect(source, "sociology") |
# str_detect(source, "cultural anthropology") |
# str_detect(source, "cultural studies") |
# str_detect(source, "social") |
# str_detect(source, "gender")
# ) == TRUE
# ) %>%
# filter(str_detect(source, "psychology") == FALSE) %>%
# mutate(source = gsub("the ", "", source))
# pm_sources_not_stem

# top_pm_sources_not_stem <- pm_sources_not_stem %>%
# group_by(source) %>%
# tally() %>%
# arrange(desc(n))
# top_pm_sources_not_stem
