# DEI in Scientific Journals
# By Mason Goad, Research Fellow
# The National Association of Scholars


# Set Working Directory to Your Data Location ==================================
getwd()
setwd("C:/Users/THIS IS AN EXAMPLE/OneDrive/Desktop/dei_in_journals/data_clean")

# Load the Required Libraries ==================================================
require(janitor)
require(dplyr)
require(tidyverse)
require(scales)


# Load ScienceDirect Data
sci_direct <- read.csv("science_direct_clean.csv")

# Search For Duplicate Entries
sci_direct_deduped <- sci_direct %>% distinct(title, .keep_all = T)

# Filter ScienceDirect Results By Title:
sci_direct_titles <- sci_direct_deduped %>% 
  filter((
    str_detect(title,"diversity")| str_detect(title,"Diversity")|
      str_detect(title,"equity")| str_detect(title,"Equity")|
      str_detect(title,"health equity")| str_detect(title,"Health Equity")|
      str_detect(title,"inclusion")| str_detect(title,"Inclusion")|
      str_detect(title,"inclusivity")| str_detect(title,"Inclusivity")|
      str_detect(title,"DEI")| str_detect(title,"DEIJ")|
      str_detect(title,"Diverse Workforce")|
      str_detect(title,"anti-racism")| str_detect(title,"antiracism")|
      str_detect(title,"Anti-racism")| str_detect(title,"Anti-Racism")|
      str_detect(title,"Antiracism")|
      str_detect(title,"decolonization")| str_detect(title,"Decolonization")|
      str_detect(title,"decolonize")| str_detect(title,"Decolonize")|
      str_detect(title,"racism")| str_detect(title,"race")|
      str_detect(title,"Racism")| str_detect(title,"Race")|
      str_detect(title,"justice")| str_detect(title,"Justice")|
      str_detect(title,"underrepresented")| str_detect(title,"under-represented")|
      str_detect(title,"Underrepresented")| str_detect(title,"Under-represented")|
      str_detect(title,"minority")| str_detect(title,"indigenous")|
      str_detect(title,"Minority")| str_detect(title,"Indigenous")|
      str_detect(title,"gender")| str_detect(title,"Gender")|
      str_detect(title,"transgender")| str_detect(title,"Transgender")|
      str_detect(title,"trans-gender")| str_detect(title,"Trans-gender")|
      str_detect(title,"Retention of Women")| str_detect(title,"Careers of Women")|
      str_detect(title,"LGBT")| str_detect(title,"LGBTQ")|
      str_detect(title,"LGBTQIA")| str_detect(title,"Queer")|
      str_detect(title,"systemic bias")| str_detect(title,"Systemic Bias")|
      str_detect(title,"implicit bias")| str_detect(title,"Implicit Bias")|
      str_detect(title,"unconcious bias")| str_detect(title,"Unconcious Bias")|
      str_detect(title,"Latinx"))==TRUE)


# Search ScienceDirect Abstracts / Step One:
sci_direct_title_fail <- sci_direct_deduped %>% filter((
  str_detect(title,"diversity")| str_detect(title,"Diversity")|
    str_detect(title,"equity")| str_detect(title,"Equity")|
    str_detect(title,"health equity")| str_detect(title,"Health Equity")|
    str_detect(title,"inclusion")| str_detect(title,"Inclusion")|
    str_detect(title,"inclusivity")| str_detect(title,"Inclusivity")|
    str_detect(title,"DEI")| str_detect(title,"DEIJ")|
    str_detect(title,"Diverse Workforce")|
    str_detect(title,"anti-racism")| str_detect(title,"antiracism")|
    str_detect(title,"Anti-racism")| str_detect(title,"Anti-Racism")|
    str_detect(title,"Antiracism")|
    str_detect(title,"decolonization")| str_detect(title,"Decolonization")|
    str_detect(title,"decolonize")| str_detect(title,"Decolonize")|
    str_detect(title,"racism")| str_detect(title,"race")|
    str_detect(title,"Racism")| str_detect(title,"Race")|
    str_detect(title,"justice")| str_detect(title,"Justice")|
    str_detect(title,"underrepresented")| str_detect(title,"under-represented")|
    str_detect(title,"Underrepresented")| str_detect(title,"Under-represented")|
    str_detect(title,"minority")| str_detect(title,"indigenous")|
    str_detect(title,"Minority")| str_detect(title,"Indigenous")|
    str_detect(title,"gender")| str_detect(title,"Gender")|
    str_detect(title,"transgender")| str_detect(title,"Transgender")|
    str_detect(title,"trans-gender")| str_detect(title,"Trans-gender")|
    str_detect(title,"Retention of Women")| str_detect(title,"Careers of Women")|
    str_detect(title,"LGBT")| str_detect(title,"LGBTQ")|
    str_detect(title,"LGBTQIA")| str_detect(title,"Queer")|
    str_detect(title,"systemic bias")| str_detect(title,"Systemic Bias")|
    str_detect(title,"implicit bias")| str_detect(title,"Implicit Bias")|
    str_detect(title,"unconcious bias")| str_detect(title,"Unconcious Bias")|
    str_detect(title,"Latinx"))==FALSE) 

# Search Science Direct Abstracts / Step Two:
sci_direct_abstracts <- sci_direct_title_fail %>% filter((
  str_detect(abstract,"diversity")| str_detect(abstract,"Diversity")|
    str_detect(abstract,"equity")| str_detect(abstract,"Equity")|
    str_detect(abstract,"health equity")| str_detect(abstract,"Health Equity")|
    str_detect(abstract,"inclusion")| str_detect(abstract,"Inclusion")|
    str_detect(abstract,"inclusivity")| str_detect(abstract,"Inclusivity")|
    str_detect(abstract,"DEI")| str_detect(abstract,"DEIJ")|
    str_detect(abstract,"Diverse Workforce")|
    str_detect(abstract,"anti-racism")| str_detect(abstract,"antiracism")|
    str_detect(abstract,"Anti-racism")| str_detect(abstract,"Anti-Racism")|
    str_detect(abstract,"Antiracism")|
    str_detect(abstract,"decolonization")| str_detect(abstract,"Decolonization")|
    str_detect(abstract,"decolonize")| str_detect(abstract,"Decolonize")|
    str_detect(abstract,"racism")| str_detect(abstract,"race")|
    str_detect(abstract,"Racism")| str_detect(abstract,"Race")|
    str_detect(abstract,"justice")| str_detect(abstract,"Justice")|
    str_detect(abstract,"underrepresented")| str_detect(abstract,"under-represented")|
    str_detect(abstract,"Underrepresented")| str_detect(abstract,"Under-represented")|
    str_detect(abstract,"minority")| str_detect(abstract,"indigenous")|
    str_detect(abstract,"Minority")| str_detect(abstract,"Indigenous")|
    str_detect(abstract,"gender")| str_detect(abstract,"Gender")|
    str_detect(abstract,"transgender")| str_detect(abstract,"Transgender")|
    str_detect(abstract,"trans-gender")| str_detect(abstract,"Trans-gender")|
    str_detect(abstract,"Retention of Women")| str_detect(abstract,"Careers of Women")|
    str_detect(abstract,"LGBT")| str_detect(abstract,"LGBTQ")|
    str_detect(abstract,"LGBTQIA")| str_detect(abstract,"Queer")|
    str_detect(abstract,"systemic bias")| str_detect(abstract,"Systemic Bias")|
    str_detect(abstract,"implicit bias")| str_detect(abstract,"Implicit Bias")|
    str_detect(abstract,"unconcious bias")| str_detect(abstract,"Unconcious Bias")|
    str_detect(abstract,"Latinx"))==TRUE) 

# Bind Science Direct Positive Titles & Positive Abstracts
sci_direct_actual <- rbind(sci_direct_titles, sci_direct_abstracts)

# See Sources of Articles
sci_direct_journal <- sci_direct_actual %>% get_dupes(journal)
sci_direct_other_journals <- anti_join(sci_direct_actual, sci_direct_journal)


# Graph Data
sci_direct_actual %>% 
  group_by(year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=year, y=total, group = 1)) + geom_line(size = 1.5) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1997, 2022)) +
  labs(title = "DEI Related Articles Added To ScienceDirect Database By Year (1997-2022)",
       x = "Time", y = "Count") +
  theme(axis.title = element_text(face ="bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))