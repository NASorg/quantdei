# Decolonization Literature in Google Scholar
# By Mason Goad, Research Fellow
# The National Association of Scholars


# Set Working Directory to Your Data Location ==================================
getwd()
setwd("C:/Users/mason/OneDrive/Desktop/Decolonize GS")

# Load the Required Libraries ==================================================
require(janitor)
require(dplyr)
require(tidyverse)
require(scales)

# Load Google Scholar Data & Bind Sets
Deco_0 <- read.csv("GS_Christonormativity_Results_All_Years.csv")
Deco_1 <- read.csv("GS_Decolonisation_Results_All_Years.csv")
Deco_2 <- read.csv("GS_Decolonization_Results_2010_2023.csv")
Deco_3 <- read.csv("GS_Decolonization_Results_1950_2009.csv")
Deco_4 <- read.csv("GS_Eurocentrism_Results_2003_2023.csv")
Deco_5 <- read.csv("GS_Eurocentrism_Results_All_Years.csv")
Deco_6 <- read.csv("GS_Post_Colonialism_Results_1950_2009.csv")
Deco_7 <- read.csv("GS_Post_Colonialism_Results_2010_2023.csv")
Deco_8 <- read.csv("GS_Post_Colonialism_Results_All_Years.csv")
Deco_9 <- read.csv("GS_Reindiginization_Results_All_Years.csv")
Deco_10 <- read.csv("GS_Settler_Colonialism_Results_1950_2009.csv")
Deco_11 <- read.csv("GS_Settler_Colonialism_Results_2010_2023.csv")
Deco_12 <- read.csv("GS_Whiteness_Results_2003_2023.csv")

Deco_All_PT1 <- rbind(Deco_0, Deco_1, Deco_2, Deco_3, Deco_4,
                      Deco_5, Deco_6, Deco_7, Deco_8, Deco_9,
                      Deco_10, Deco_11, Deco_12)

# Search For Duplicate Entries & Remove
Deco_All_PT2 <- Deco_All_PT1 %>% distinct(Title, .keep_all = T)

# Scan Titles For False Positives
sample_title <- Deco_All_PT2 %>% distinct(Title)
sample_n(sample_title, 21)

# Look For Most Common Sources
common_source <- Deco_All_PT2 %>%get_dupes(Source)
common_pub <- Deco_All_PT2 %>% get_dupes(Publisher)

# Graph Data:
Deco_All_PT2 %>%
  group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.5) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1950, 2020)) +
  labs(title = "Decolonization Literature in Google Scholar by Year of Publication (1950-2020)",
       x = "Time", y = "Count") +
  theme(axis.title = element_text(face ="bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
# Looking at the trends over 70 years, the publication of decolonization
# literature skyrocketed after the year 2000. Granted, the results here may
# be biased due to search limitations via Publish or Perish software.


# The good news is that the publication rates of such literature appear to be
# in a very steady decline since 2020 to the present day.
Deco_All_PT2 %>%
  group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.5) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(2020, 2023)) +
  labs(title = "Decolonization Literature in Google Scholar by Year of Publication (2020-2023)",
       x = "Time", y = "Count") +
  theme(axis.title = element_text(face ="bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Even more good news: present day levels of decolonization literature being
# published are approaching year 2000 levels. This time frame (2003 to 2023)
# was the clearest and most heavily-scanned era with our software.
Deco_All_PT2 %>%
  group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.5) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(2003, 2023)) +
  labs(title = "Decolonization Literature in Google Scholar by Year of Publication (2003-2023)",
       x = "Time", y = "Count") +
  theme(axis.title = element_text(face ="bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

