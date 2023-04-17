
require(tidyverse)
require(glue)
require(here)


here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))


# Load Twitter Data ------------------------------------------------------------

t <- fread(glue("{here}/out/twitter/tweets_clean.csv"))
names(t)

t <- fread(glue("{here}/out/twitter/tweets_clean.csv"))
t[,good_date := as_date(created_at)]
t[,good_date := ymd(good_date)]
t[,short_name := str_remove(file, glue("{here}/data/_twitter/twitter_account_dei_term/"))]
t[,school := str_extract(short_name, "^.*?(?=--)")]
t[,account := map_chr(short_name, ~str_split(.x, "--")[[1]][2])]
t[,dei_term := str_remove(map_chr(short_name, ~str_split(.x, "--")[[1]][3]), "\\.Rds")]
t[,month := lubridate::floor_date(good_date, "month")]


# Find "Race" Errors -----------------------------------------------------------

race_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"race")==TRUE) 

race_fail <- race_tweets %>% 
  filter(
    (
      str_detect(text,"team")|
      str_detect(text,"compete")|
      str_detect(text,"human race")|
      str_detect(text,"road race")|
      str_detect(text,"motorcycle")|
      str_detect(text,"finished")|
      str_detect(text,"car race")|
      str_detect(text,"driver")|
      str_detect(text,"nascar")|
      str_detect(text,"indy")|
      str_detect(text,"daytona")|
      str_detect(text,"mudder")|
      str_detect(text,"food truck")|
      str_detect(text,"athletic")|
      str_detect(text,"bike race")|
      str_detect(text,"swim")|
      str_detect(text,"golf")|
      str_detect(text,"ncaa")|
      str_detect(text,"sailing")|
      str_detect(text,"5k")|
      str_detect(text,"10k")|
      str_detect(text,"championship")|
      str_detect(text,"cycling")|
      str_detect(text,"cross country")|
      str_detect(text,"rowing")|
      str_detect(text,"football")|
      str_detect(text,"triathl")|
      str_detect(text,"baseball")|
      str_detect(text,"olympics")|
      str_detect(text,"basketball")|
      str_detect(text,"espn")|
      str_detect(text,"acc ")|
      str_detect(text,"title")|
      str_detect(text,"space race")|
      str_detect(text,"doeracetozero")|
      str_detect(text,"patsrun")|
      str_detect(text,"endurance")|
      str_detect(text,"robot")|
      str_detect(text,"relay race")|
      str_detect(text,"sweat")|
      str_detect(text,"stadium")|
      str_detect(text,"amazing race")|
      str_detect(text,"boston")
    )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"diversity")==FALSE) %>% 
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"racial justice")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  mutate(fail_category="race")


(nrow(race_fail)/nrow(race_tweets))*100


race_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Race' (n = 25,167)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


race_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,32)) +
  labs(title = "False Positives Identified in Tweets About 'Race' (n = 5,055)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# Find "Justice" Errors --------------------------------------------------------

justice_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"justice")==TRUE) 

justice_fail <- justice_tweets %>% 
  filter(
    (
      str_detect(text,"supreme court")|
      str_detect(text,"sandra day o  connor")|
      str_detect(text,"conner")|
      str_detect(text,"online")|
      str_detect(text,"commencement")|
      str_detect(text,"civil rights")|
      str_detect(text,"criminal justice")
    )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"antiracism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>% 
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>%
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="justice")


justice_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Justice' (n = 22,090)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


justice_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,60)) +
  labs(title = "False Positives Identified in Tweets About 'Justice' (n = 2,466)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# Find "Diversity" Errors ------------------------------------------------------

diversity_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"diversity")==TRUE) 

diversity_fail <- diversity_tweets %>% 
  filter(
    (
      str_detect(text,"species")|
      str_detect(text,"insect")|
      str_detect(text,"mammal")|
      str_detect(text,"bird")|
      str_detect(text,"ecology")|
      str_detect(text,"evolution")|
      str_detect(text,"snake")|
      str_detect(text,"frog")|
      str_detect(text,"lizard")|
      str_detect(text,"genetic")|
      str_detect(text,"reptile")|
      str_detect(text,"botan")|
      str_detect(text,"biodiversity")|
      str_detect(text,"plant") 
    )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"anti-racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"civil rights")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="diversity")


diversity_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Diversity' (n = 31,268)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


diversity_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,37)) +
  labs(title = "False Positives Identified in Tweets About 'Diversity' (n = 325)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# Find "Equity" Errors ---------------------------------------------------------

equity_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"equity")==TRUE) 

equity_fail <- equity_tweets %>% 
  filter(
    (
      str_detect(text,"private equity")|
      str_detect(text,"finance")|
      str_detect(text,"financial")|
      str_detect(text,"investment")|
      str_detect(text,"home")|
      str_detect(text,"bank")|
      str_detect(text,"theater")|
      str_detect(text,"firm")|
      str_detect(text,"capital")|
      str_detect(text,"wall street")
    )==TRUE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"civil rights")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"Kendi")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"privilege")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="equity")


equity_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Equity' (n = 16,374)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


equity_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,30)) +
  labs(title = "False Positives Identified in Tweets About 'Equity' (n = 487)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# Find "Advocacy" Errors -------------------------------------------------------

advocacy_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"advocacy")==TRUE) 

advocacy_fail <- advocacy_tweets %>% 
  filter(
    (
      str_detect(text,"patient advocacy")|
      str_detect(text,"legal")|
      str_detect(text,"law school")|
      str_detect(text,"appellate")|
      str_detect(text,"college of law")|
      str_detect(text,"truman")|
      str_detect(text,"trial")|
      str_detect(text,"veterans")|
      str_detect(text,"legislature")|
      str_detect(text,"civil rights")|
      str_detect(text,"legislative")
    )==TRUE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"racial justice")==FALSE) %>% 
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"Kendi")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"privilege")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="advocacy")


advocacy_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Advocacy' (n = 5,128)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


advocacy_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,28)) +
  labs(title = "False Positives Identified in Tweets About 'Advocacy' (n = 683)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# Find "Ally" Errors -----------------------------------------------------------

ally_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"ally")==TRUE) 

ally_fail <- ally_tweets %>% 
  filter(
    (
      str_detect(text,"softball")|
      str_detect(text,"tennis")|
      str_detect(text,"gymnast")|
      str_detect(text,"lacross")|
      str_detect(text,"hockey")|
      str_detect(text,"golf")|
      str_detect(text,"water polo")|
      str_detect(text,"basketball")|
      str_detect(text,"swim")|
      str_detect(text,"pool")|
      str_detect(text,"champion")|
      str_detect(text,"athlete")|
      str_detect(text,"sport")|
      str_detect(text,"stadium")|
      str_detect(text,"horses")|
      str_detect(text,"equestrian")
    )==TRUE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"racial justice")==FALSE) %>% 
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"Kendi")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"privilege")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="ally")


ally_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Ally' (n = 6,953)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


ally_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,11)) +
  labs(title = "False Positives Identified in Tweets About 'Ally' (n = 274)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


# privilege errors -------------------------------------------------------

privilege_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"privilege")==TRUE) 

privilege_fail <- privilege_tweets %>% 
  filter(
    (
      str_detect(text,"a privilege to")|
      str_detect(text,"my privilege to")|
      str_detect(text,"reporter")|
      str_detect(text,"privilege and honor")|
      str_detect(text,"privilege and an")|
      str_detect(text,"privilege of")|
      str_detect(text,"the privilege of")
    )==TRUE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"white privilege")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"racial justice")==FALSE) %>% 
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"Black Lives Matter")==FALSE) %>%
  filter(str_detect(text,"BLM")==FALSE) %>%
  filter(str_detect(text,"George Floyd")==FALSE) %>%
  filter(str_detect(text,"Kendi")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"power")==FALSE) %>%
  filter(str_detect(text,"antiracism")==FALSE) %>%
  filter(str_detect(text,"social justice")==FALSE) %>% 
  mutate(fail_category="privilege")


privilege_tweets %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Tweets About 'Privilege' (n = 3,498)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


privilege_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(.5,9)) +
  labs(title = "False Positives Identified in Tweets About 'Privilege' (n = 991)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))



# Bind Sample Sets & Graph -----------------------------------------------------

tweet_all<-bind_rows(race_tweets,
                     equity_tweets,
                     diversity_tweets,
                     justice_tweets,
                     advocacy_tweets,
                     ally_tweets,
                     privilege_tweets) 

tweet_fail<-bind_rows(race_fail,
                      equity_fail,
                      diversity_fail,
                      justice_fail,
                      advocacy_fail,
                      ally_fail,
                      privilege_fail) %>% 
  relocate(fail_category,.before=1)


tweet_all %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  labs(title = "All Sampled Tweets (n = 110,478)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))


tweet_fail %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line(size = .75) +
  scale_y_continuous(limits = c(0,68)) +
  labs(title = "All False Postivies Identified in Sampled Tweets (n = 10,281)",
       x = "year", y = "count") +
  #theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))



# Conduct Primary Account Analysis ---------------------------------------------


twitter_handles <-  fread(glue("{here}/out/twitter/schools_twitter_handles.csv"))
# unique_handles <- map_chr(unique(twitter_handles, by = "twitter_handle")$twitter_handle, ~str_remove(.x, "twitter\\.com/"))
school_main_handles <- unique(twitter_handles,by="file_short_name",fromLast=FALSE)
school_main_handles[,twitter_handle := map_chr(twitter_handle, ~str_remove(.x, "twitter\\.com/"))]

school_main_sample <- tweet_all[account %in% school_main_handles$twitter_handle]
school_main_sample[, month := lubridate::floor_date(good_date, "month")]
school_main_sample[, year := lubridate::floor_date(good_date, "year")]

school_main_fail <- tweet_fail[account %in% school_main_handles$twitter_handle]
school_main_fail[, month := lubridate::floor_date(good_date, "month")]
school_main_fail[, year := lubridate::floor_date(good_date, "year")]


school_main_sample[month > "2015-01-01"] %>%
  count(account, month) %>% 
  # group_by(account, month) 
  # summarise(total = n(), .groups = 'drop')
  ggplot(., aes(x=month, y=n)) + geom_col() +
  labs(title = "All Sampled Tweets from University Primary Accounts",
       x = "year", y = "count") +
  #theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))


school_main_fail[month > "2015-01-01"] %>%
  count(account, month) %>% 
  # group_by(account, month) 
  # summarise(total = n(), .groups = 'drop')
  ggplot(., aes(x=month, y=n)) + geom_col() +
  scale_y_continuous(limits = c(0,1600)) +
  labs(title = "All False Positives Identified in Sampled Tweets from University Primary Accounts",
       x = "year", y = "count") +
  #theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))


# Calculate Percentages of False Positives by Category -------------------------

advocacy_fail_perc <- (nrow(advocacy_fail)/nrow(advocacy_tweets))*100
advocacy_fail_perc

ally_fail_perc <-(nrow(ally_fail)/nrow(ally_tweets))*100
ally_fail_perc

diversity_fail_perc <-(nrow(diversity_fail)/nrow(diversity_tweets))*100
diversity_fail_perc

equity_fail_perc <-(nrow(equity_fail)/nrow(equity_tweets))*100
equity_fail_perc

justice_fail_perc <-(nrow(justice_fail)/nrow(justice_tweets))*100
justice_fail_perc

privilege_fail_perc <-(nrow(privilege_fail)/nrow(privilege_tweets))*100
privilege_fail_perc

race_fail_perc <-(nrow(race_fail)/nrow(race_tweets))*100
race_fail_perc

sample_fail_perc<-(nrow(tweet_fail)/nrow(tweet_all))*100
sample_fail_perc
