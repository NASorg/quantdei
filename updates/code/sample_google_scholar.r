# Much of the filtering used here was first created by Dr. Emilio Bruna at the University of Florida.
# Mason Goad and Bruce R. Chartwell thank Dr. Bruna for these contributions. 
# Dr. Bruna's original code can be found at: https://github.com/BrunaLab/quantdei_nas

require(tidyverse)
require(glue)
require(here)
require(janitor)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))


# Load Google Scholar Data -----------------------------------------------------

gs <- read_fst(glue("{here}/out/scholarship/google_scholar.fst")) %>%
  as_tibble() %>%
  mutate_all(tolower)


# Make Original Graphs ---------------------------------------------------------

gs %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  count(topic, year) %>% 
  filter(year < "2022-01-01" & year > "2000-01-01" & topic != "science") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) + 
  geom_line(size = .75) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 31. Google Scholar results for DEI and STEM terms",
       x = "year", y = "count") +
  #theme_classic() +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))


# Find Duplicates --------------------------------------------------------------

gs_dupes <- gs %>% get_dupes(title, source, year)


# Remove Duplicates From Data Set ----------------------------------------------

gs_deduped <- anti_join(gs, gs_dupes)


# Check for Non-STEM Sources With Key Word Filter ------------------------------

gs_sources_not_stem <- gs_deduped %>%
  filter(
    (
      str_detect(source, "law") |
        str_detect(source, "race") |
        str_detect(source, "class") |
        str_detect(source, "education") |
        str_detect(source, "teacher") |
        str_detect(source, "urban") |
        str_detect(source, "sociology") |
        str_detect(source, "cultural anthropology") |
        str_detect(source, "cultural studies") |
        str_detect(source, "social") |
        str_detect(source, "gender")
    ) == TRUE
  ) %>%
  filter(str_detect(source, "psychology") == FALSE) %>%
  filter(str_detect(source, "science education") == FALSE) %>%
  filter(str_detect(source, "physics education") == FALSE) %>%
  filter(str_detect(source, "equity in stem") == FALSE) %>%
  filter(str_detect(source, "diversity in stem") == FALSE) %>%
  filter(str_detect(source, "cultural studies of science") == FALSE) %>%
  filter(str_detect(source, "physics teacher") == FALSE) %>%
  filter(str_detect(source, "mathematics") == FALSE) %>%
  filter(str_detect(source, "law & medicine") == FALSE) %>%
  filter(str_detect(source, "law and social science") == FALSE) %>%
  filter(str_detect(source, "medical education") == FALSE) %>%
  filter(str_detect(source, "mathematics and science teachers") == FALSE) %>%
  filter(str_detect(source, "microbiology") == FALSE) %>%
  filter(str_detect(source, "biology education") == FALSE) %>%
  filter(str_detect(source, "biology teacher") == FALSE) %>%
  filter(str_detect(source, "chemistry teacher") == FALSE) %>%
  filter(str_detect(source, "chemistry education") == FALSE) %>%
  filter(str_detect(source, "chemical education") == FALSE) %>%
  filter(str_detect(source, "science & education") == FALSE) %>%
  filter(str_detect(source, "science teacher") == FALSE) %>%
  filter(str_detect(source, "cultural geography") == FALSE) %>%
  filter(str_detect(source, "geography education") == FALSE) %>%
  filter(str_detect(source, "geography teacher") == FALSE) %>%
  filter(str_detect(source, "journal of gender, science, and") == FALSE) %>%
  filter(str_detect(source, "social psychological") == FALSE) %>%
  filter(str_detect(source, "health education") == FALSE) %>%
  filter(str_detect(source, "social science & medicine") == FALSE) %>%
  filter(str_detect(source, "philosophy of social science") == FALSE) %>%
  filter(str_detect(source, "urban sciences") == FALSE) %>%
  filter(str_detect(source, "stem gender equality") == FALSE) %>%
  filter(str_detect(source, "undergraduate stem education") == FALSE) %>%
  filter(str_detect(source, "journal of science teacher education") == FALSE) %>%
  filter(str_detect(source, "social function of science") == FALSE) %>%
  mutate(source = gsub("the ", "", source))


# Remove Non-STEM Sources From Data Set ----------------------------------------

gs_deduped_2 <- anti_join(gs_deduped, gs_sources_not_stem)


# Find Articles With No Source -------------------------------------------------

gs_no_source <- gs_deduped_2 %>%
  filter(source == "" | source == "��") %>%
  group_by(source)


# Remove Articles With No Source From Data Set ---------------------------------

gs_deduped_3 <- anti_join(gs_deduped_2, gs_no_source)


# Create Comparison Graphs -----------------------------------------------------

gs_deduped_3 %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  count(topic, year) %>% 
  filter(year < "2022-01-01" & year > "2000-01-01" & topic != "science") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) + 
  geom_line(size = .75) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 31. Google Scholar results for DEI and STEM terms (filtered)",
       x = "year", y = "count") +
  #theme_classic() +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))

