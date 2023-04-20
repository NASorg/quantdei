# Much of the filtering used here was first created by Dr. Emilio Bruna at the University of Florida.
# Mason Goad and Bruce R. Chartwell thank Dr. Bruna for these contributions. 
# Dr. Bruna's original code can be found at: https://github.com/BrunaLab/quantdei_nas

require(dplyr)
require(tidyverse)
require(glue)
library(janitor)
require(here)
library(fst)

here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))


# Load NSF Grants --------------------------------------------------------------

n <- fread(paste0(here, "/out/grants/nsf_all_grants_summary_data.csv"))
n[, term_sum := rowSums(.SD), .SDcols = 5:34]
n[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
n[, day := lubridate::floor_date(good_date, "day")]
n[, month := lubridate::floor_date(good_date, "month")]
n[, year := lubridate::floor_date(good_date, "year")]

#n <- n %>%
# mutate_all(tolower) %>%
# as_tibble()

#names(n)


# Find Duplicate Files ---------------------------------------------------------

nsf_dupes_all <- n %>%
  get_dupes(title) %>%
  filter(str_detect(title, "postdoctoral research fellowship") == FALSE) %>%
  filter(str_detect(title, "postdoctoral fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate research fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate reserach fellowship program") == FALSE) %>%
  filter(str_detect(title, "waterman") == FALSE) %>%
  filter(str_detect(title, "summer institute") == FALSE) %>%
  as_tibble() %>% 
  mutate(agency="nsf") %>% 
  relocate(agency,.before=1)
nsf_dupes_all[, term_sum := rowSums(.SD), .SDcols = 5:34]
nsf_dupes_all[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
nsf_dupes_all[, day := lubridate::floor_date(good_date, "day")]
nsf_dupes_all[, month := lubridate::floor_date(good_date, "month")]
nsf_dupes_all[, year := lubridate::floor_date(good_date, "year")]


# Remove Duplicated Files ------------------------------------------------------

nsf_nodupe <- anti_join(n, nsf_dupes_all) #%>% as_tibble()


# Make Comparison Graphs -----------------------------------------------------

# Compare Fig. 18
n[month > "2010-01-01" & month < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(term_sum)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 18. All DEI terms in all NSF grants by year",
       x = "time", y = "count")

nsf_nodupe[month > "2010-01-01" & month < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(term_sum)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 18. All DEI terms in all NSF grants by year (duplicates removed)",
       x = "time", y = "count")




# Compare Fig. 19
n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(`anti-racism`, antiracism, `Black Lives Matter`, BLM, `Critical Race Theory`, `George Floyd`, racism, racist, `white fragility`, `white supremacy`)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 19. All NSF grants with antiracist terms by year",
       x = "year", y = "count")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(`anti-racism`, antiracism, `Black Lives Matter`, BLM, `Critical Race Theory`, `George Floyd`, racism, racist, `white fragility`, `white supremacy`)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 19. All NSF grants with antiracist terms by year (duplicates removed)",
       x = "year", y = "count")




# Compare Fig. 20
n[year > "2010-01-01" & year < "2021-10-01" & term_sum > 2] %>% 
  group_by(year) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  ggplot(aes(x = year, y = amount_funded)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 20. NSF grant funding for all DEI-related projects",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01" & term_sum > 2] %>% 
  group_by(year) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  ggplot(aes(x = year, y = amount_funded)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 20. NSF grant funding for all DEI-related projects (duplicates removed)",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")




# Compare Fig. 21
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(status = ifelse(term_sum > 2, "DEI-related", "All other grants")) %>% 
  group_by(year, status) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  mutate(amount_funded = log(amount_funded)) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = status)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  # scale_y_continuous(labels = dollar) +
  labs(title = "Fig 21. NSF grant funding for all DEI-related projects compared to other projects",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(status = ifelse(term_sum > 2, "DEI-related", "All other grants")) %>% 
  group_by(year, status) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  mutate(amount_funded = log(amount_funded)) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = status)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  # scale_y_continuous(labels = dollar) +
  labs(title = "Fig 21. NSF grant funding for all DEI-related projects compared to other projects (duplicates removed)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")




# Compare Fig. 22
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  filter(antiracism_bool == 1) %>% 
  ggplot(aes(x = year, y = amount_funded)) +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 22. NSF grant funding for all antiracism-related projects (absolute)",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  filter(antiracism_bool == 1) %>% 
  ggplot(aes(x = year, y = amount_funded)) +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 22. NSF grant funding for all antiracist-related projects (absolute, duplicates removed)",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")




# Compare Fig. 23
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(antiracism_bool, "Grants with \nantiracist language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 23. NSF grants for antiracist and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(antiracism_bool, "Grants with \nantiracist language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 23. NSF grants for antiracist and all other topics by grant ammount (logged, duplicates removed)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")




# Compare Fig. 24
n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(gender, transgender)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 24. All NSF grants with terms 'gender' and 'transgender' by year",
       x = "year", y = "count")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(gender, transgender)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) + 
  #theme_classic() +
  labs(title = "Fig 24. All NSF grants with terms 'gender' and 'transgender' by year (duplicates removed)",
       x = "year", y = "count")




# Compare Fig. 25
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(gender_trans_bool = ifelse(gender == TRUE | transgender == TRUE, 1, 0)) %>% 
  group_by(year, gender_trans_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(gender_trans_bool, "Grants with gender-related language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 25. NSF grants for gender-related and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

nsf_nodupe[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(gender_trans_bool = ifelse(gender == TRUE | transgender == TRUE, 1, 0)) %>% 
  group_by(year, gender_trans_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(gender_trans_bool, "Grants with gender-related language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line(size = 1) +
  #theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 25. NSF grants for gender-related and all other topics by grant ammount (logged, duplicates removed)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")


# Load and Graph Fig. 26 -------------------------------------------------------

n1 <- fread(glue("{here}/out/grants/nsf_all_grants_summary_data_w_dei_terms.csv"))
n1[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
n1[, day := lubridate::floor_date(good_date, "day")]
n1[, month := lubridate::floor_date(good_date, "month")]
n1[, year := lubridate::floor_date(good_date, "year")]

n2 <- n1[year > "2010-01-01" & year < "2021-10-01"] %>% 
  count(year, dei_term)

n2[dei_term != "" & n > 1 & dei_term %notin% c("racism", "transgender", "George Floyd", "Critical Race Theory", "oppression", "antiracism", "BLM", "ally")] %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  geom_jitter() +
  #theme_classic() +
  labs(title = "Fig 26. All NSF grants with DEI-related terms by year",
       x = "year", y = "count")


# Load and Graph Fig. 26 Comparison --------------------------------------------

nsf_dupes_all_1 <- n1 %>%
  get_dupes(title) %>%
  filter(str_detect(title, "postdoctoral research fellowship") == FALSE) %>%
  filter(str_detect(title, "postdoctoral fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate research fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate reserach fellowship program") == FALSE) %>%
  filter(str_detect(title, "waterman") == FALSE) %>%
  filter(str_detect(title, "summer institute") == FALSE) %>%
  as_tibble() %>% 
  mutate(agency="nsf") %>% 
  relocate(agency,.before=1)
nsf_dupes_all_1[, term_sum := rowSums(.SD), .SDcols = 5:34]
nsf_dupes_all_1[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
nsf_dupes_all_1[, day := lubridate::floor_date(good_date, "day")]
nsf_dupes_all_1[, month := lubridate::floor_date(good_date, "month")]
nsf_dupes_all_1[, year := lubridate::floor_date(good_date, "year")]

nsf_nodupe_1 <- anti_join(n1, nsf_dupes_all_1) #%>% as_tibble()

n2_deduped <- nsf_nodupe_1[year > "2010-01-01" & year < "2021-10-01"] %>% 
  count(year, dei_term)

n2_deduped[dei_term != "" & n > 1 & dei_term %notin% c("racism", "transgender", "George Floyd", "Critical Race Theory", "oppression", "antiracism", "BLM", "ally")] %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  geom_jitter() +
  #theme_classic() +
  labs(title = "Fig 26. All NSF grants with DEI-related terms by year (duplicates removed)",
       x = "year", y = "count")



# NSF Error Analysis -----------------------------------------------------------

# 'Diversity" Error Analysis
diversity_nsf <- nsf_nodupe %>%
  filter(diversity == 1) %>%
  select(-diversity) %>%
  filter(equity == 0 &
           bias == 0 &
           transgender == 0 &
           systemic == 0 &
           racism == 0 &
           discrimination == 0 &
           `social justice` == 0 &
           justice == 0 &
           racist == 0 &
           multicultural == 0 &
           privilege == 0 &
           Kendi == 0 &
           ally == 0 &
           race == 0 &
           intersectional == 0 &
           `implicit bias` == 0 &
           gender == 0) #%>%
  #select(title, fund_date, amount)


 diversity_nsf_fail <- diversity_nsf %>%
   filter(
     (
       str_detect(title, "genetic") |
         str_detect(title, "species") |
         str_detect(title, "dna") |
         str_detect(title, "rna ") | # note space
         str_detect(title, "ecology") |
         str_detect(title, "food web") |
         str_detect(title, "herbivor") |
         str_detect(title, "mammal") |
         str_detect(title, "insect") |
         str_detect(title, "mutualis") |
         str_detect(title, "habitat") |
         str_detect(title, "avian") |
         str_detect(title, "taxonom") |
         str_detect(title, "wildlife") |
         str_detect(title, "evolution") |
         str_detect(title, "microb") |
         str_detect(title, "phylogen") |
         str_detect(title, "biogeograph") |
         str_detect(title, "biogeochemist") |
         str_detect(title, "tropical") |
         str_detect(title, "genom") |
         str_detect(title, "biotic") |
         str_detect(title, "host") |
         str_detect(title, "arctic") |
         str_detect(title, "networks") |
         str_detect(title, "parasite") |
         str_detect(title, "biodiversity") |
         str_detect(title, "species") |
         str_detect(title, "insect") |
         str_detect(title, "mammal") |
         str_detect(title, "ecology") |
         str_detect(title, "evolution") |
         str_detect(title, "evolv") |
         str_detect(title, "bird") |
         str_detect(title, "snake") |
         str_detect(title, "frog") |
         str_detect(title, "lizard") |
         str_detect(title, "earth") |
         str_detect(title, "genetic") |
         str_detect(title, "botan") |
         str_detect(title, "biodiversity") |
         str_detect(title, "plant")
     ) == TRUE
   ) %>%
   filter(str_detect(title, "1619 project") == FALSE) %>%
   filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
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
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 

 diversity_nsf[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Diversity' (Duplicates Removed, n = 6,901)",
        x = "time", y = "count")
 
 diversity_nsf_fail[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "False Positives Identified In All NSF Grants By Tern 'Diversity' (Duplicates Removed, n = 928)",
        x = "time", y = "count")
 
 diversity_nsf_actual <- anti_join(diversity_nsf, diversity_nsf_fail)

 diversity_nsf_actual[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Diversity' After Filtering (Duplicates and False Positives Removed, n = 5,973)",
        x = "time", y = "count")
 
 
 
 
 # 'Systemic' Error Analysis
 systemic_nsf <- nsf_nodupe %>%
   filter(systemic == 1) %>%
   select(-systemic) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            #systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            justice == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 systemic_nsf_fail <- systemic_nsf %>%
   filter(
     (
       str_detect(title,"systemic complications")|
         str_detect(title,"systemic complications from")
     )==TRUE) %>% 
   filter(str_detect(title, "1619 project") == FALSE) %>%
   filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   #filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
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
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 
 

 
 
 # 'Equity' Error Analysis
 equity_nsf <- nsf_nodupe %>%
   filter(equity == 1) %>%
   select(-equity) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            justice == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 equity_nsf_fail <- equity_nsf %>%
   filter(
     (
         str_detect(title,"private equity")|
           str_detect(title,"finance")|
           str_detect(title,"financial")|
           str_detect(title,"investment")|
           str_detect(title,"home")|
           str_detect(title,"bank")|
           str_detect(title,"theater")|
           str_detect(title,"firm")|
           str_detect(title,"capital")|
           str_detect(title,"wall street")
       )==TRUE) %>% 
   filter(str_detect(title, "1619 project") == FALSE) %>%
   filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   #filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
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
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 
 
 equity_nsf[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Equity' (Duplicates Removed, n = 362)",
        x = "time", y = "count")
 
 equity_nsf_fail[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "False Positives Identified In All NSF Grants By Tern 'Equity' (Duplicates Removed, n = 2)",
        x = "time", y = "count")
 
 
 equity_nsf_actual <- anti_join(equity_nsf, equity_nsf_fail)
 
 equity_nsf_actual[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Equity' After Filtering (n = 360)",
        x = "time", y = "count")
 
 
 
 
 # 'Justice' Error Analysis
 justice_nsf <- nsf_nodupe %>%
   filter(justice == 1) %>%
   select(-justice) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 justice_nsf_fail <- justice_nsf %>%
   filter(
     (
       str_detect(title,"supreme court")|
         str_detect(title,"sandra day o  connor")|
         str_detect(title,"conner")|
         str_detect(title,"online")|
         str_detect(title,"commencement")|
         str_detect(title,"civil rights")|
         str_detect(title,"criminal justice")
     )==TRUE) %>% 
   filter(str_detect(title, "1619 project") == FALSE) %>%
   filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
   filter(str_detect(title, "george floyd") == FALSE) %>%
   filter(str_detect(title, "inequality") == FALSE) %>%
   filter(str_detect(title, "implicit bias") == FALSE) %>%
   filter(str_detect(title, "indigenous") == FALSE) %>%
   filter(str_detect(title, "inclusion") == FALSE) %>%
   filter(str_detect(title, "inclusive") == FALSE) %>%
   filter(str_detect(title, "intersectional") == FALSE) %>%
   #filter(str_detect(title, "justice") == FALSE) %>%
   filter(str_detect(title, "kendi") == FALSE) %>%
   filter(str_detect(title, "microaggression") == FALSE) %>%
   filter(str_detect(title, "multicultural") == FALSE) %>%
   filter(str_detect(title, "oppression") == FALSE) %>%
   filter(str_detect(title, "privilege") == FALSE) %>%
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 
 
 justice_nsf[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Justice' (Duplicates Removed, n = 292)",
        x = "time", y = "count")
 
 justice_nsf_fail[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "False Positives Identified In All NSF Grants By Tern 'Justice' (Duplicates Removed, n = 0)",
        x = "time", y = "count")
 
 
 
 
 # 'Advocacy' Error Analysis
 advocacy_nsf <- nsf_nodupe %>%
   filter(advocacy == 1) %>%
   select(-advocacy) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 advocacy_nsf_fail <- advocacy_nsf %>%
   filter(
     (
       str_detect(title,"patient advocacy")|
         str_detect(title,"legal")|
         str_detect(title,"law school")|
         str_detect(title,"appellate")|
         str_detect(title,"college of law")|
         str_detect(title,"truman")|
         str_detect(title,"trial")|
         str_detect(title,"veterans")|
         str_detect(title,"legislature")|
         str_detect(title,"civil rights")|
         str_detect(title,"legislative")
     )==TRUE) %>% 
   filter(str_detect(title, "1619 project") == FALSE) %>%
   #filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
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
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 
 
 advocacy_nsf[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "All NSF Grants By Term 'Advocacy' (Duplicates Removed, n = 144)",
        x = "time", y = "count")
 
 advocacy_nsf_fail[month > "2010-01-01" & month < "2021-10-01"] %>%  
   group_by(year) %>% 
   summarise(n = sum(term_sum)) %>% 
   ggplot(aes(x = year, y = n)) +
   # facet_wrap(~dei_term, ncol = 3, scales = "free") +
   geom_line(size = 1) + 
   #scale_y_continuous(limits = c(.5,700)) +
   #theme_classic() +
   labs(title = "False Positives Identified In All NSF Grants By Tern 'Advocacy' (Duplicates Removed, n = 2)",
        x = "time", y = "count")
 
 
 
 # Analyze Sample Sizes -------------------------------------------------------
 # ----------------------------------------------------------------------------
 # ----------------------------------------------------------------------------
 
 

 # 1619 Project // n = 0
 nhj_project_nsf <- nsf_nodupe %>%
   filter(`1619 project` == 1) %>%
   select(-`1619 project`) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 # Bias // n = 666
 bias_nsf <- nsf_nodupe %>%
   filter(bias == 1) %>%
   select(-bias) %>%
   filter(diversity == 0 &
            #bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 bias_nsf_fail <- bias_nsf %>%
   filter(
     (
       str_detect(title,"medical bias")|
         str_detect(title,"research bias")|
         str_detect(title,"publication bias")|
         str_detect(title,"bias the results")|
         str_detect(title,"researcher bias")
     )==TRUE) %>% 
   filter(str_detect(title, "1619 project") == FALSE) %>%
   #filter(str_detect(title, "advocacy") == FALSE) %>%
   filter(str_detect(title, "ally") == FALSE) %>%
   filter(str_detect(title, "anti-racism") == FALSE) %>%
   filter(str_detect(title, "antiracism") == FALSE) %>%
   #filter(str_detect(title, "bias") == FALSE) %>%
   filter(str_detect(title, "black lives") == FALSE) %>%
   filter(str_detect(title, "black lives matter") == FALSE) %>%
   filter(str_detect(title, "blm") == FALSE) %>%
   filter(str_detect(title, "civil right") == FALSE) %>%
   filter(str_detect(title, "critical race theory") == FALSE) %>%
   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
   filter(str_detect(title, "discrimination") == FALSE) %>%
   filter(str_detect(title, "pathways") == FALSE) %>%
   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
   filter(str_detect(title, "diverse") == FALSE) %>%
   filter(str_detect(title, "equity") == FALSE) %>%
   filter(str_detect(title, "equality") == FALSE) %>%
   filter(str_detect(title, "gender") == FALSE) %>%
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
   filter(str_detect(title, "race") == FALSE) %>%
   filter(str_detect(title, "racism") == FALSE) %>%
   filter(str_detect(title, "racial") == FALSE) %>%
   filter(str_detect(title, "racist") == FALSE) %>%
   filter(str_detect(title, "reform") == FALSE) %>%
   filter(str_detect(title, "social justice") == FALSE) %>%
   filter(str_detect(title, "social change") == FALSE) %>%
   filter(str_detect(title, "systemic racism") == FALSE) %>%
   filter(str_detect(title, "transgender") == FALSE) %>%
   filter(str_detect(title, "minority") == FALSE) %>%
   filter(str_detect(title, "trans") == FALSE) %>%
   filter(str_detect(title, "underrepresented") == FALSE) %>%
   filter(str_detect(title, "white fragility") == FALSE) %>%
   filter(str_detect(title, "white supremacy") == FALSE) 
 
 
 # Implicit Bias // n = 0
 implicit_bias_nsf <- nsf_nodupe %>%
   filter(`implicit bias` == 1) %>%
   select(-`implicit bias`) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            #`implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 # Systemic // n = 0
 systemic_nsf <- nsf_nodupe %>%
   filter(systemic == 1) %>%
   select(-systemic) %>%
   filter(diversity == 0 &
            bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
 
 # Race // n = 0
 race_nsf <- nsf_nodupe %>%
   filter(race == 1) %>%
   select(-race) %>%
   filter(diversity == 0 &
            #bias == 0 &
            transgender == 0 &
            systemic == 0 &
            racism == 0 &
            discrimination == 0 &
            `social justice` == 0 &
            equity == 0 &
            racist == 0 &
            multicultural == 0 &
            privilege == 0 &
            Kendi == 0 &
            ally == 0 &
            race == 0 &
            intersectional == 0 &
            `implicit bias` == 0 &
            gender == 0) #%>%
 #select(title, fund_date, amount)
 
