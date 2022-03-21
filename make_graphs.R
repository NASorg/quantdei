#' ---
#' title: "Quantitative Study of Diversity, Equity and Inclusion in STEM Subjects in US Universities"
#' author: "Scott Turner, Mason Goad, and Bruce Chartwell"
#' date: "Latest version: `r format(Sys.time(), '%d %B, %Y')`"
#' abstract: " \\singlespacing \\noindent Over the last few years institutions of higher education in the United States and around the world have increasingly begun adopting Diversity, Equity, and Inclusion (DEI) ideology and discourse in both administration and teaching. While these ideas first arose in association with the humanities, more recently they have begun to influence the natural sciences. This report attempts to quantitatively measure the prevalence of DEI-associated language in Science, Technology, Engineering, and Mathematics (STEM) fields at US universities, in US-based academic associations in the sciences, and in the grantmaking activities of major science funders. The four new datasets assembled here comprise over 280,000 files and 30 gigabytes, making this the largest quantitative study of the burgeoning DEI influence on the sciences in the US. It is designed to provide the foundation for, but ultimately supplement, a comprehensive study of the phenomenon using qualitative analysis."
#' fontsize: 
#'   12pt
#' urlcolor: blue
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
      echo = FALSE,
      dev = "svglite",
      fig.ext = ".svg", 
      warning = FALSE, 
      error = FALSE)
here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

#' 
#' # Executive summary
#' 
#' This report is the largest quantitative study of the growth of Diversity, Equity, and Inclusion (DEI) language in the sciences published thus far. It gathers and analyzes five original datasets and performs a very simple form of quantitative analysis to measure the growth of DEI language in those datasets: counting the number of DEI-related terms. The results are presented in over 30 graphs, a series of tables, and an animation. These graphics are intended to measure the change in magnitude of the usage of DEI terminology in the sciences over the last decade.
#' 
#' The five datasets this paper gathers and analyzes are:
#' 
#' 1.  The websites of the top 100 universities and colleges in the United States;
#' 2.  Twitter feeds of the top 100 universities and university-related accounts;
#' 3.  The annual programs of academic associations in the four major branches of the natural sciences;
#' 4.  Grants by three major funders of scientific research: the National Science Foundation, the Ford Foundation, and the National Institutes of Health;
#' 5.  Scientific scholarship in four major publishers, aggregators, or preprint repositories: Google Scholar, arXiv, Web of Science, and PubMed. 
#' 
#' Each dataset and an explanation of how it was gathered, and the results of the analysis, is presented in its own section.
#' 
#' We find that DEI language has increased significantly over the last decade, and in particular over the last few years, in all of these areas with the exception of the annual programs of academic associations. Some of the headline findings include: 
#' 
#' * The number of web pages that mention discuss both STEM and DEI on the 100 university websites grew from 110 in 2010 to 2,891 in 2021. This suggests that **DEI is being linked with STEM over 26 times more frequently than it was a decade ago**; 
#' * This growth was more concentrated at Ivy League universities than non-Ivy. **As of 2021, each Ivy university on average had almost double the number of web pages linking DEI and STEM than non-Ivy universities.**;
#' * The use of DEI or social justice-related language on the Twitter accounts of universities has grown since 2015, but is primarily characterized by explosions of interest followed by reversion to the mean; 
#' * The evidence for the growth of DEI language in the annual programs of academic associations is mixed. **The data show a combination of moderate to no growth, and sometimes negative growth, for many of the associations. Use of antiracist language has grown since 2020, however.**
#' * Both the National Science Foundation and the National Institutes of Health have diverted significantly more funding to DEI-related topics over the last few years. **NSF funding to projects with antiracist themes more than tripled from 2020 to 2021.** The number of DEI-related projects receiving NIH funding grew by dozens and sometimes hundreds of times between 2010 and the present.
#' * Between 2010 and 2021, **scientific publications and preprints that incorporate DEI or antiracist language grew from between 3 and 42 times faster than scientific topics in general in the Web of Science.**
#' * Similar patterns are observed in data from Google Scholar and PubMed, also published by NIH. The growth of preprints with DEI or antiracist themes has grown significantly in the last couple of years, but this findings is less reliable due to the small number of papers captured.
#' 
#' Care must be taken with the interpretation of some of these figures. For instance, while it is true that co-occurrence of DEI and STEM terminology on university websites is 26 times more prevalent than it was a decade ago, the use of websites in general is also far more prevalent. Some of our data allowed for appropriate base rate comparison, but other forms of data (such as university websites) did not. The extremely sharp growth in the co-occurrence of the two terms, however, at a time when DEI was gaining rapid salience in the broader culture, suggests that there is a strong signal in the data.
#' 
#' The findings of this report in general are intended to be interpreted in this manner. This is an exploratory and inductive analysis. Most research attention and funding of DEI has so far been interested in adopting a DEI framework to social phenomenon; here we use the framework of quantitative social science to study the rise of DEI itself. The findings are preliminary, though the trends we observe are striking. We look forward to other scholars and researchers building on them in an effort to explore and understand the impact of this new school of thought on the conduct and content of science in the United States.
#' 
#' \newpage
#' 
#' # Introduction
#' 
#' The role of Diversity, Equity and Inclusion (DEI) in higher education in the United States and around the world has increased significantly in recent years. Opinions are divided on the desirability of this trend. While the sciences are almost certainly being influenced by DEI, there have been limited attempts so far to quantify the extent of such influence.
#' 
#' This research note discusses the creation and analysis of five new datasets that attempt to quantitatively measure the salience of DEI language in discussion of STEM content at the top 100 universities in the United States.
#' 
#' We conceive of this report as a preliminary research note as part of the National Association of Scholar's larger investigation of DEI influence in higher education in the United States. It is almost entirely an exercise in quantitative description. Where possible, we have attempted to place each measure -- typically no more than term frequency counts -- in an appropriate scale. For instance, we count the number of tweets using DEI language from the Twitter accounts of the universities in the study -- yet this number alone might be misleading, because the total number of tweets from the accounts has also increased over time. Therefore, in the case of tweets we should instead be interested in the *relative* frequency of DEI language: this would tell us whether DEI ideas have come to occupy a relatively larger number of Tweets over time.
#' 
#' In some cases, however, a relevant baseline is not possible to establish. For instance, the first section of the report downloads HTML files from nearly 60,000 URLs on university websites -- yet we cannot gather data on the total number of URLs on university websites over time, and so these figures are given as raw counts rather than proportions. Raw counts still serve as relevant and useful indicators of the growth of language on this topic.
#' 
#' In the case of grants, where available we used relative dollar amounts, rather than the number of grants given -- because the former is obviously more indicative of institutional priorities.
#' 
#' This report does not discuss the desirability of the increase in prevalance of DEI speech or even assess its meaning -- tasks more suited for qualitative analysis. The findings are meant to be meaning-neutral: advocates of DEI may be either satisfied with their success so far, or see far greater scope for growth; opponents may be alarmed at the incursions of DEI language, or relieved that they are not as severe as anticipated.
#' 
#' The report, then, is a computational project that counts the prevalence of terms associated with both DEI and STEM. It is difficult, if not impossible, to use such an approach to evaluate the extent to which DEI ideas are influencing the conduct and output of scientific research. It would be reasonable to presume that the more these terms come to intermingle in scientific work, the more DEI is influencing scientific knowledge --- yet the precise nature of this influence can still only be assessed through detailed qualitative analysis.
#' 
#' To illustrate this tension, consider two potential models for how DEI ideology could influence STEM training and research. In the first, it serves both a pastoral and bureaucratic-managerial function: the moral formation of young adults is no longer mediated by religion and philosophy, but instead antiracism and other flavors of progressive identitarianism. This matrix of ideas simultaneously enables administrators to accrue resources and exercise power. But in this model, the core of the scientific process is mostly left intact --- straightforward positivist research is simply framed in and guided toward antiracist or equity-based ends. A computer science course thus teaches the usual sequence --- hash tables, shell scripting, object oriented programming --- but adds in a class about the racist origins of the United States. A week of classes where students could have learned more computer science was diverted to a political topic, but the scientific knowledge that was imparted was not itself influenced by DEI thought.
#' 
#' The second model would instead resemble something like Lysenkoism: a field of purportedly scientific study that was in fact ideologically-driven pseudoscience.^[Here setting aside the lack of consensus on the demarcation criterion and a host of other issues in the philosophy and sociology of science.]
#' 
#' As stated above, the data here cannot adjudicate between which of these is going on --- though the former seems much more probable. There is of course also an argument that in practice these two processes cannot be neatly separated, and that any incursion of extra-scientific beliefs into purely scientific enquiry works to the detriment of science. This report does not clear up any of these questions; it simply counts the number of DEI-related words that have begun appearing in scientific contexts.
#' 
#' Our work builds on that of Leif Rasmussen published by the Center for the Study of Partisanship and Ideology in 2021,[^Rasmussen, Leif. Increasing Politicization and Homogeneity in Scientific Funding: An Analysis of NSF Grants, 1990-2020. The Center for the Study of Partisanship and Ideology. Report No. 4 (11/16/2021). https://cspicenter.org/wp-content/uploads/2021/11/CSPI.Report.4.pdf] though it broadens the remit of study to many other data sources, and does not examine the NSF data in nearly as much depth as Rasmussen. The findings, however, are similar. Other work in the same broad vein includes that by Zach Goldberg,[^Goldberg, Zach. Woke Terms & Media Racism Statistics. Tablet Magazine (08/05/2020). https://www.tabletmag.com/sections/news/articles/media-great-racial-awakening.] who studies "The Great Awokening." The present study examines the same set of topics, with a specific focus on the sciences.
#' 
#' A discussion of each of the datasets follows.
#' 
#' # University websites
#' 
#' Most public organizations now use their websites to communicate with internal and external stakeholders. If indeed DEI thought was gaining influence in STEM teaching and research at universities in the US, one would expect to see this on university websites.
#' 
#' To pursue this question, we gathered nearly 60,000 URLs from university websites that included both at least one STEM term from our dictionary and one DEI term. We then extracted dates from those pages, and threw out every URL that included more than one date. We graphed the remainder on frequency plots. The result shows the number of URLs on university websites over time that feature a STEM term, a DEI term, and a single date.
#' 
#' Our STEM dictionary included the following terms:
#' 
#' `"astronomy", "biology", "chemistry", "engineering", "geology", "mathematics", "meteorology", "oceanography", "physics", "science"`
#' 
#' Our DEI dictionary included the following terms:
#' 
#' `"diversity", "equity", "inclusion", "systemic racism", "white supremacy" , "anti-racism", "justice", "privilege", "critical race theory"`
#' 
#' Each combination of these terms was searched along with each university in our dataset, consisting of over 100. The result were thousands of searches of the form `yale astronomy critical race theory`, `harvard metereology antiracism`, etc.
#' 
#' The precise methods are explained in the technical appendix.
#' 
#' ## Results
#' 
## ----school_websites0, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------
files_dates <- fread(glue("{here}/out/school_websites/files_dates.csv"))
files_dates[,dates := str_remove_all(dates, "\\n")]
files_dates[,school := str_extract(short_name, "^.*?(?=_)")]
files_dates[,dei_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][2])]
files_dates[,stem_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][3])]
files_dates[,good_date := parse_date(dates)]
files_dates1 <- files_dates[is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = mdy(dates))]
files_dates2 <- files_dates[!is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = as_date(good_date, "ymd"))]
files_dates3 <- rbind(files_dates1, files_dates2)

# Just make sure these are good. Run this a bunch of times and eyeball it. 
# sample_n(files_dates3[,.(dates, good_date)], 100)

# files_dates4 <- as_tibble(files_dates3)

files_dates3 <- files_dates3[good_date > "1990-01-01" & good_date < "2021-10-01"]
files_dates3[, month := lubridate::floor_date(good_date, "month")]

# setorder(files_dates3[, .(count = .N), by = "short_name"], -count)[]

one_date <- files_dates3[, .(count = .N), by = "short_name"][count == 1]
files_dates4 <- files_dates3[short_name %in% one_date$short_name]

ivy_c <- c("harvard", "yale", "princeton", "columbia", "upenn", "brown", "dartmouth", "cornell")
files_dates4[,ivy :=  ifelse(school %in% ivy_c, "ivy", "non-ivy")]
files_dates4[dei_term != "anti-racism",dei_term := str_replace_all(dei_term, "-", " ")]

# files_dates4[month == "2021-09-01"] %>% count(short_name) %>% arrange(desc(n))

#' 
## ----school_websites1, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------
s <- svgstring()

# AD HOC. USED TO CALCULATE THE GROWTH MENTIONED IN THE EXECUTIVE SUMMARY
# files_dates4[good_date > "2010-01-01" & good_date < "2011-01-01" & !is.na(dei_term)] %>% 
#   arrange(good_date) %>% 
#   group_by(month) %>% 
#   summarize(count = n()) %>% 
#   summarise(sum = sum(count))
# 
# files_dates4[good_date > "2021-01-01" & good_date < "2022-01-01" & !is.na(dei_term)] %>% 
#   arrange(good_date) %>% 
#   group_by(month) %>% 
#   summarize(count = n()) %>% 
#   summarise(sum = sum(count))


files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  # arrange(desc(count))
  ggplot(aes(x = month, y = count)) +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 1. Mentions of both STEM and DEI terms \non all university websites over time",
       x = "time", y = "count")

htmltools::HTML(s())

ggsave(glue("{here}/graphs/Fig 1.svg"))
ggsave(glue("{here}/graphs/Fig 1.png"))

#' 
## ----school_websites2, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------
files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month, dei_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~dei_term, scales = "free", nrow = 6, ncol = 2) +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 2. Mentions of specific DEI terms alongside all STEM terms \non university websites over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 2.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 2.png"), height = 8,  width = 6, dpi = 300)

#' 
## ----school_websites3, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------
files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month, stem_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~stem_term, ncol = 2, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 3. Mentions of all DEI terms alongside specific STEM terms \non university websites over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 3.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 3.png"), height = 8,  width = 6, dpi = 300)

#' 
## ----school_websites4, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------
files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  # mutate(year = floor_date(good_date, "year")) %>%
  group_by(month, ivy) %>%
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~ivy, ncol = 2, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 4. Mentions of all DEI terms alongside all STEM terms on \nIvy League vs non-Ivy League universities over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 4.svg"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 4.png"), height = 4,  width = 6, dpi = 300)

# IVY vs NON-IVY CALCULATIONS
# files_dates4 %>% distinct(school, .keep_all = T) %>% count(ivy)
# ratio:
# 8:92
# 92/8
# 
# in 2010 the ratio was 11:99; that is 11 hits on ivy and 99 hits on ivy
# So that is like: 99/92 (1.07 hits per non ivy) vs 11/8 (1.375 hits per ivy)
# 
# In 2021 the ratio was 409:2490, that is ivy to non-ivy. 
# So that is 2490/92 (27.065) hits per non-ivy and 409/8  (51.125) hits per ivy 
#
# files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
#   mutate(year = floor_date(good_date, "year")) %>% 
#   group_by(year, ivy) %>% 
#   summarize(count = n(), .groups = "drop") %>% 
#   arrange(year)



#' 
#' The data underlying the above graph is tallied on a monthly basis. If calculated on a yearly basis, the data shows that Ivy league universities had 409 web pages featuring both STEM and DEI language in 2021, while non-Ivy universities had 2,490. But because there were only 8 Ivy league universities in the dataset, but 92 non-Ivy, Ivy league universities have almost double the per-university number of such web pages (51 vs 27). 
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
files_dates4[good_date > "2016-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  mutate(year = floor_date(month, unit = "year"), 
         dei_term = str_replace_all(dei_term, " ", "\n")) %>% 
  group_by(year, stem_term, dei_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line()  +
  facet_grid(vars(stem_term), vars(dei_term), "free_y") +
  theme_bw(base_size = 8) +
  labs(title = "Fig 5. Matrix of DEI vs STEM terms at all universities since 2016",
       x = "year", y = "count") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2016-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "2 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 5.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 5.png"), height = 8,  width = 6, dpi = 300)


#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------


geolocation_data <- readRDS(glue("{here}/out/school_websites/geolocation_data.Rds"))
geolocation_data <- as.data.table(geolocation_data[,-c("geocoded")])

files_dates4 %>%
  inner_join(geolocation_data, by = "school") %>% 
  count(month, school, lon, lat) %>% 
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(size = 1, alpha = .15, color = "red") +
  theme_map() +
  coord_map() + 
  labs(title = "Fig 6. Geographic spread of co-occurrence \nof DEI and STEM terms at universities")

ggsave(glue("{here}/graphs/Fig 6.jpg"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 6.svg"), height = 4,  width = 6, dpi = 300)

anim <- files_dates4 %>%
  inner_join(geolocation_data, by = "school") %>% 
  count(month, school, lon, lat) %>% 
  ggplot(aes(lon, lat)) +
  borders("state", colour = "lightgray") +
  geom_point(size = 3, alpha = .15, color = "red") +
  transition_manual(month, cumulative = T) +
  labs(title = "Growth of DEI and STEM terms over time: { current_frame }") +
  theme_map() +
  coord_map()

# animate(
#   anim,
#   nframes = 100,
#   fps = 10, 
#   height = 4, 
#   width = 6, 
#   units = "in",
#   res = 150
# )

# anim_save(glue("{here}/graphs/dei_stem_growth_map.gif"))
# files_dates4 %>% sample_n(10)

#' 
#' \noindent NOTE: We put the graphs on the same page and let the axes float, meaning that their absolute values differ. This brings to focus what we're interested in: the relative trends, rather than the absolute values as such.
#' 
#' ## Examples
#' 
#' The primary limitation of this method is that it does not actually tell us whether DEI ideology is influencing STEM teaching content, which is our main interest. The data only tells us that these two terms are occurring together on the same university web page, with a single specified date.
#' 
#' The simplest means of validating the appropriateness of this data is simply to look at it. Here are some examples of data demonstrating some of the typical dynamics of DEI and STEM language in specific university websites.
#' 
#' In each case, the File column contains the university name, the DEI term, and the STEM term. The files were randomly drawn from the dataset.
#' 
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | File                                                                   | Note                                                                                                                                                                                                                                             | Strength    |
#' +========================================================================+==================================================================================================================================================================================================================================================+=============+
#' | arizona_equity_meteorology_27444b40ad1b73d86f706f334e5fdf0e.txt        | 'Equity' appears in the bibliography of a scholar in environmental science.                                                                                                                                                                      | Weak        |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | duke_critical-race-theory_geology_b0c3bbea8fdf201dc7957a0fe6b0ec3d.txt | A blog by a professor discussing both geology and CRT.                                                                                                                                                                                           | Weak        |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | asu_inclusion_engineering_697a19f2f0ca82489e081d7b15da2a66.txt         | Reference to Apple-funded "Inclusion and Diversity Engineering" scholarships.                                                                                                                                                                    | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | arizona_anti-racism_astronomy_87753bb3997368539b56ddb993344536.txt     | Examines relationship between STEM and the 'JEDI' DEI initiative.                                                                                                                                                                                | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | gatech_justice_biology_4a0e1d1cc422fd17bcbbbf6a0ef20154.txt            | An employer list.                                                                                                                                                                                                                                | None        |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | hmc_inclusion_biology_f59fdb64b82ae57ed841122cac1e001e.txt             | Link to Harvey Mudd College's Women's Inclusion in Science, Technology, Engineering & Mathematics application page.                                                                                                                              | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | ucsb_diversity_engineering_cb2a7b7421bd15ec29428b0f096ddc85.txt        | Advertisement for a seminar on "Science and Diversity" led by a professor of biomedical engineering                                                                                                                                              | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | tufts_justice_chemistry_26490c4c9705ef7d94dad09605ebf5c8.txt           | A list of STEM research projects based on anti-racist or social justice principles, including "Connecting the Dots between Organic Chemistry and Social Justice through Mechanistic Reasoning" and Systemic Racism as a Driver for Math Anxiety" | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | illinois_diversity_meteorology_7ded23554c67592282755fd8e6167937.txt    | A landing page for weather data. DEI appears in a link.                                                                                                                                                                                          | None        |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' | wisc_anti-racism_engineering_2b4f83cad917ae4c4a6d0c2099e4700e.txt      | News of a \$5 million award "to fund an interdisciplinary, multi-year project to advance anti-racist practices and pedagogy in science, technology, engineering, mathematics, and medicine"                                                      | Strong      |
#' +------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+
#' 
#' This survey is a telling indication of the kind of data our large-scale web scraping operation surfaced. Six of the ten cases we randomly sampled showed strong links between DEI initiatives and STEM, while in four the relationship was either weak or non-existent. This indicates that there is still noise in the data. However, we also discarded nearly 90% of the data in an attempt to make the signal as clean as possible. Even if only 3/5 of the data in the remaining 10,000 datapoints are valid indications of strong associations between DEI and STEM, there is no reason to suspect that the number of weak associations increased over time relative to the total number of observations. 
#' 
#' In other words, there is still a strong signal in the data that DEI and STEM topics have been co-occurring with much greater frequency in recent years. This also accords with the rest of the measures we develop below. 
#' 
#' # Twitter feeds
#' 
#' Twitter has become the frontier of the culture war, and is one of the key platforms DEI advocates use for persuasion and mobilization. We're interested in the degree to which the social media presence of higher educational institutions has come to adopt DEI-related language. This analysis of Twitter feeds focuses primarily on DEI language in general, not only the confluence of STEM and DEI. It thus complements our study of STEM and DEI, and shows that the increase of DEI in STEM follows a broader trend of increased usage of DEI language in general. 
#' 
#' ## Methods
#' 
#' We used the Google JSON API to search for the name of the university and "twitter" (i.e. `Harvard University twitter`, etc.) for the 100 schools. We dumped the raw JSON and parsed it for all Twitter accounts identified on the first page of results (a maximum of 10). This allowed us to identify both the *primary* Twitter account ran by the school (for instance, `@Harvard`), but at the same time up to nine other related accounts (for instance, in the case of University of Arizona: arizonaalumni, arizonafball, dailywildcat, etc.)^[These are in the `./data/school_websites/json` folder in the project's replication files.] This allows for more detailed statistics and lets us see both the depth and breadth of DEI-terminology.
#' 
#' Parsing out the Twitter accounts from the JSON search results led us to identifying 895 unique accounts. We then linked them with their respective schools, and combined them with 21 DEI terms. This led to 22,475 individual API calls yielding 151,284 tweets.^[This is more than simply multiplying the DEI terms by the unique accounts, because at that point we had not deduplicated the accounts.] These tweets -- each from a school or school-related account, where the tweet uses a DEI term -- form the dataset.
#' 
#' ## Results
#' 
## ----twitter1, echo=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------------------------------------
t <- fread(glue("{here}/out/twitter/tweets_clean.csv"))
t[,good_date := as_date(created_at)]
t[,good_date := ymd(good_date)]
t[,short_name := str_remove(file, glue("{here}/data/_twitter/twitter_account_dei_term/"))]
t[,school := str_extract(short_name, "^.*?(?=--)")]
t[,account := map_chr(short_name, ~str_split(.x, "--")[[1]][2])]
t[,dei_term := str_remove(map_chr(short_name, ~str_split(.x, "--")[[1]][3]), "\\.Rds")]
t[,month := lubridate::floor_date(good_date, "month")]
# unique(t, by = "account")
# t %>%  distinct(dei_term)

#' 
## ----twitter2, echo=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------------------------------------
t %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line() +
  labs(title = "Fig 7. All DEI-related Tweets from all school-related accounts over time",
       x = "year", y = "count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(glue("{here}/graphs/Fig 7.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 7.svg"), height = 4,  width = 6, dpi = 300)


#' 
## ----twitter3, echo=FALSE, warning=FALSE, error=FALSE, fig.height=18, fig.width=12-----------------------------------------------------------------
t %>%
  group_by(month, dei_term) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  filter(month > "2016-01-01") %>% 
  ggplot(., aes(x=month, y=total, group = 1)) + geom_line() +
  labs(title = "Fig 8. DEI-related Tweets from all school-related accounts \nby DEI term",
       x = "year", y = "count") +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~dei_term, ncol = 3, scales = "free_y") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 8.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 8.png"), height = 8,  width = 6, dpi = 300)


#' 
## ----twitter4, echo=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------------------------------------
twitter_handles <-  fread(glue("{here}/out/twitter/schools_twitter_handles.csv"))
# unique_handles <- map_chr(unique(twitter_handles, by = "twitter_handle")$twitter_handle, ~str_remove(.x, "twitter\\.com/"))
school_main_handles <- unique(twitter_handles,by="file_short_name",fromLast=FALSE)
school_main_handles[,twitter_handle := map_chr(twitter_handle, ~str_remove(.x, "twitter\\.com/"))]
school_main_tweets <- t[account %in% school_main_handles$twitter_handle]
school_main_tweets[, month := lubridate::floor_date(good_date, "month")]
school_main_tweets[, year := lubridate::floor_date(good_date, "year")]

school_main_tweets[month > "2015-01-01"] %>%
  count(account, month) %>% 
  # group_by(account, month) 
  # summarise(total = n(), .groups = 'drop')
  ggplot(., aes(x=month, y=n)) + geom_col() +
  labs(title = "Fig 9. All DEI-related Tweets from university primary accounts",
       x = "year", y = "count") +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 9.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 9.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' Every single primary school account used DEI terms on its twitter timeline at some stage during the period under observation.
#' 
#' ## Discussion
#' 
#' The limitations of this analysis are similar to those in the previous cases --- except in this instance, we're are not looking at co-occurrence with STEM language, but simply the growth of DEI language on the Twitter accounts of universities over time. 
#' 
#' It is unclear what effect this has, if any, on STEM education at universities. The pattern is a modest amount of growth of STEM language over time, along with a big spike in the summer of 2020, almost certainly a response to the killing of George Floyd.
#' 
#' # Academic associations
#' 
#' The motivation for studying the output of academic associations is because we are interested in whether DEI ideology is influencing the scientific output of a field. If it was, we'd expect to see it most directly in at least two places: the pipeline of new research and thinking published by these associations, and scientific output itself (i.e. preprints, dissertations, journal publications, books). In this section, we look at the former. The following section is reserved for the latter.
#' 
#' Most of these associations hold annual conferences where researchers present scientific abstracts of their work. We were able to obtain a collection of annual programs for three of the four major scientific associations; in another case we used 19 years (\~225 pdfs) of their monthly newsletters and conducted text analysis on it.
#' 
#' \noindent The four scientific associations included in this analysis are:
#' 
#' -   American Physical Association (APA)
#' 
#' -   American Chemical Society (ACS)
#' 
#' -   American Mathematical Society (AMS)
#' 
#' -   American Society for Biochemistry and Molecular Biology (ASBMB)
#' 
#' \noindent These were the largest groups we could identify based on membership size and resources they control.
#' 
#' These results of this analysis are the most ambiguous of all studied so far.
#' 
#' On the one hand, all of the societies appear to have a section on their website and a variety of committees devoted to DEI issues --- *e.g.* [Minorities in Physics](https://www.aps.org/programs/minorities/index.cfm),[Women in Physics](https://www.aps.org/programs/women/index.cfm), [Committee on the Status of Women in Physics](https://www.aps.org/about/governance/committees/cswp/index.cfm), an [antiracist education and resource list](https://www.acs.org/content/dam/acsorg/communities/diversity-inclusion-and-respect/anti-racism-resource-education-list.pdf) based on a document compiled by [the artist, organizer, and activist Sarah Sophie Flicker](https://www.the-bleu.com/40s/sarah-sophie-flicker/) --- but their annual programs do not show a strong skew toward DEI-related content.
#' 
#' For opponents of DEI, these results might be encouraging: they show that while professional associations are promoting DEI ideas through organizational and staffing change, this is not leading to a significant impact in the *content* of scientific output that the societies evaluate and discuss annually.
#' 
#' ## Methods
#' 
#' In three of the four cases we downloaded all of the annual programs, in most cases for all of the years available (over 20 years). In some cases this was only for \~5 years. The following table summarizes the data:
#' 
#' +---------------------------------------------------------+---------------------------------------------------------------------------------------------------+
#' | Academic Association                                         | Data obtained                                                                                     |
#' +=========================================================+===================================================================================================+
#' | American Physical Association                           | Program of abstracts from the March and April annual scientific meetings from 2007-2021 (30 pdfs) |
#' +---------------------------------------------------------+---------------------------------------------------------------------------------------------------+
#' | American Chemical Society                               | Program of abstracts from (biennial) annual scientific meetings from 2015-2019 (10 pdfs)          |
#' +---------------------------------------------------------+---------------------------------------------------------------------------------------------------+
#' | American Mathematical Society                           | Annual scientific abstracts from 2008-2021 (14 html files)                                        |
#' +---------------------------------------------------------+---------------------------------------------------------------------------------------------------+
#' | American Society for Biochemistry and Molecular Biology | Monthly internal newsletter *ASBMB Today* from April 2002 to October 2021 (225 pdfs)              |
#' +---------------------------------------------------------+---------------------------------------------------------------------------------------------------+
#' 
#' The DEI-related terms for this analysis were the following: *1619 project, advocacy, ally, anti-racism, antiracism, bias, Black Lives Matter, BLM, Race Theory, discrimination, diverse, diversity, equity, gender, George Floyd, inequality, implicit bias, indigenous, Indigenous, inclusion, intersectional, justice, Kendi, microaggression, multicultural, oppression, privilege, race, racism, racial, racist, social justice, systemic, transgender, trans, white fragility, white supremacy.*
#' 
#' : Data used for academic association analysis
#' 
#' In each case, we did a straightforward dictionary analysis, counting the appearances of each of the DEI terms. This analysis did not seem to call for any greater sophistication, because our quantitative question is very simple: what is the rate of use of DEI terms over time? There is a sense in which raw counts are not as instructive as the change in proportion of DEI words per total words over time --- but it depends on what one is trying to infer. Looking at the change in size of `.txt` files over time (thus excluding graphics and formatting in `pdf` files), it appears that these societies are publishing *far more text* in each of these reports over time. We thus present both the raw tallies as well as the proportion of DEI terms per total volume of text. This allows us to gauge both the absolute and relative frequency of DEI terms.
#' 
#' ## Results
#' 
#' First we present the data as absolute counts. The results here are uneven -- downward trends over time for the AMC and AMS, but generally increasing trends for the APS and ASBMB. 
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
dei_counts1  <-  fread(glue("{here}/out/learned_societies/dei_term_counts.csv"))

dei_counts1 %>%
  group_by(society_full_name, year) %>%
  summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(year1 = floor_date(year1, "year")) %>% 
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 10. DEI-related terms in academic association \npublications (absolute counts)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 10.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 10.svg"), height = 4,  width = 6, dpi = 300)


#' The picture shifts slightly when the term counts are presented as proportional to the total number of characters in each year. In each case the actual proportion is minuscule as a percentage -- though this is unimportant by itself, given our concern is trends over time. When evaluated this way the trend is mostly flat, except for a jump in 2021 at the AMS. 
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
dei_counts1 %>%
  mutate(prop = term_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 11. DEI-related terms in academic association \npublications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 11.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 11.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' The above involves all of the DEI terms, but now we will examine each set of DEI terms related with gender, race, etc. in turn. 
#' 
## ----learned_societies1, echo=FALSE, warning=FALSE, error=FALSE------------------------------------------------------------------------------------
# # Diversity, equity, inclusion only ---------------------------------------
dei_counts1 %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(dei_sum), .groups = 'drop') %>%
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 12. Terms 'diversity', 'equity', and 'inclusion' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") + 
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 12.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 12.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1 %>%
  mutate(prop = dei_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 13. Terms 'diversity', 'equity', and 'inclusion' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 13.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 13.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' We now do the same analysis for race-related terminology often associated with DEI initiatives. In this instance, two of the societies did not mention either 'racism' or 'antiracism' at all in their publications. It is likely that the results are much higher for ASBMB because the only available text data for that organization were its newsletters, which are generally lighter on scientific content and heavier on social topics. The result for the APA is ambiguous given the missing data and wide variance in the estimates; there is no clear pattern. 
#' 
## ----learned_societies2, echo=FALSE, warning=FALSE, error=FALSE------------------------------------------------------------------------------------
# # Racism and antiracism only ----------------------------------------------
dei_counts1 %>%
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(race_sum), .groups = 'drop') %>%
  filter(total > 0)  %>% 
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 14. Terms 'racism' and 'antiracism' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
    scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 14.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 14.svg"), height = 4,  width = 6, dpi = 300)

dei_counts1 %>%
  mutate(prop = race_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  filter(prop > 0) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 15. Terms 'racism' and 'antiracism' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 15.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 15.svg"), height = 4,  width = 6, dpi = 300)

#' 
#' There is also no obvious pattern when analyzing the term 'gender' either, whether as absolute or relative counts.
#' 
## ----learned_societies3, echo=FALSE, warning=FALSE, error=FALSE------------------------------------------------------------------------------------
dei_counts1 %>%
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(gender_sum), .groups = 'drop') %>%
  ggplot(., aes(x=year1, y=total, group = 1)) + 
  geom_col() +
  labs(title = "Fig 16. Term 'gender' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") + 
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 16.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 16.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1 %>%
  mutate(prop = gender_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  filter(prop > 0) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) +
  geom_col() +
  labs(title = "Fig 17.  Term 'gender' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 17.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 17.svg"), height = 4,  width = 6, dpi = 300)

#' 
#' ## Discussion
#' 
#' The data used for this analysis is one of the most closely targeted of all the datasets collected for this project. If DEI language was being heavily incorporated into the abstracts of scientific conferences, this analysis would have shown it (for instance, "feminist glaciology").
#' 
#' There are two obvious limitations to the data: We do not have annual reports of abstracts for ASBMB (where we rely instead on the newsletter) and we only five years of data for the ACS. However, the data we did obtain for each of these cases seems appropriate. If the ASBMB had made a hard turn toward DEI ideology, that should certainly be apparent over 225 issues of their internal newsletter. The ACS data consists of 10 files (two each year) and intersects with the general period of growth of DEI ideas through this period. However, it is missing the last two years, when DEI activity increased significantly in the other datasets.
#' 
#' The quantitative measurements here do not address the extent to which the administrative resources or researcher attention is being diverted to DEI ideology, even if it doesn't impact the actual science itself. That question could be discovered through qualitative analysis of the associations' activities.
#' 
#' # Scientific grants and awards
#' 
#' Yet another way of measuring the relevance and strength of DEI initiatives in the sciences is through funding and grants. Grantmaking enables and drives research programs, and scientists shape their work to conform to funding opportunities. Is there an increasing trend of funding DEI-related projects?
#' 
#' ## Methods
#' 
#' We downloaded the following three datasets:
#' 
#' +-------------------------------+----------------------------------------------------------+
#' | Grantmaking body              | Data obtained                                            |
#' +===============================+==========================================================+
#' | National Science Foundation   | Full award data from 2010-2021 (\~140,000 awards, 1.5gb) |
#' +-------------------------------+----------------------------------------------------------+
#' | National Institutes of Health | Awards for nine targeted searches (described below)      |
#' +-------------------------------+----------------------------------------------------------+
#' | Ford Foundation               | Full historical award data (\~24,000 entries)            |
#' +-------------------------------+----------------------------------------------------------+
#' 
#' The DEI-related dictionary used in this analysis is the same as that used for the analysis of academic association publications above.
#' 
#' In all of the datasets, we created new boolean variables indicating whether or not a grant included one of the DEI terms.
#' 
#' The `R` files associated with this task are:
#' 
#' -   `4.nsf_analysis.R`
#' -   `4.parse_nih.R`
#' 
#' All of the data files associated with the task are in `./out/grants/`.
#' 
#' ## Results
#' 
#' ### National Science Foundation
#' 
#' The NSF data is presented at an annual level, to smooth out the effect of seasonal grant cycles. 
#' 
#' The first graph simply shows the total number of DEI terms in all NSF grants by year.
#' 
## ----nsf1, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------
n <- fread(paste0(here, "/out/grants/nsf_all_grants_summary_data.csv"))
n[, term_sum := rowSums(.SD), .SDcols = 5:34]
n[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
n[, day := lubridate::floor_date(good_date, "day")]
n[, month := lubridate::floor_date(good_date, "month")]
n[, year := lubridate::floor_date(good_date, "year")]

n[month > "2010-01-01" & month < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(term_sum)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 18. All DEI terms in all NSF grants by year",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 18.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 18.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' The following graphs takes a subset of the total DEI terms and looks only at the following antiracism terms: *anti-racism, antiracism, Black Lives Matter, BLM, Critical Race Theory, George Floyd, racism, racist, white fragility, white supremacy.*
#' 
#' The result is a clear upward trend. But this is still a crude measure, and the absolute number of grants with antiracist themes is not large -- only over 100 out of a total of over 144,000.
#' 
## ----nsf3a, echo=FALSE, warning=FALSE, error=FALSE-------------------------------------------------------------------------------------------------
n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(`anti-racism`, antiracism, `Black Lives Matter`, BLM, `Critical Race Theory`, `George Floyd`, racism, racist, `white fragility`, `white supremacy`)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 19. All NSF grants with antiracist terms by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 19.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 19.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' A different measure of the change in salience of DEI-related grants can be given by the change in amount of funding. Following are both the change in total dollars dedicated to DEI-related grants in general, then antiracist grants in particular, with both cases compared to the total funding to non-DEI topics. Logging the funding amounts is necessary to make the trend visible, because it is still a relatively very small portion of total NSF grantmaking devoted to DEI-inflected projects. Our interest is in its rapid recent growth.
#' 
#' This first graph measures all grants that contain at least three DEI-related terms. This filters out incidental inclusions of grants that only happen to use DEI language, and should thus function as a more robust measure of actual DEI-related grants.
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
n[year > "2010-01-01" & year < "2021-10-01" & term_sum > 2] %>% 
  group_by(year) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  ggplot(aes(x = year, y = amount_funded)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 20. NSF grant funding for all DEI-related projects",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 20.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 20.svg"), height = 4,  width = 6, dpi = 300)

#' 
#' The following graph is substantially the same measure as the above, except the y-axis is logged so the total DEI-related spend can be compared to the total grants given by NSF. The grant amount here cannot be interpreted as a dollar amount; this graph simply serves to show that the funding directed towards DEI-related projects has grown over time relative to the total grant dollars awarded. 
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(status = ifelse(term_sum > 2, "DEI-related", "All other grants")) %>% 
  group_by(year, status) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  mutate(amount_funded = log(amount_funded)) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = status)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  # scale_y_continuous(labels = dollar) +
  labs(title = "Fig 21. NSF grant funding for all DEI-related projects compared to other projects",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 21.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 21.svg"), height = 4,  width = 6, dpi = 300)

#' 
#' The following graph illustrates the same trend for projects that incorporate antiracist language. The category of "antiracism-related projects" showed in the following graph refers to any grant that includes any of the following terms in its project narrative: *antiracism, anti-racism, Black Lives Matter, BLM, Critical Race Theory,  George Floyd, racism, racist, white fragility, white supremacy.* 
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  filter(antiracism_bool == 1) %>% 
  ggplot(aes(x = year, y = amount_funded)) +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 22. NSF grant funding for all antiracism-related projects (absolute)",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 22.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 22.svg"), height = 4,  width = 6, dpi = 300)


n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(antiracism_bool, "Grants with \nantiracist language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 23. NSF grants for antiracist and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 23.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 23.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' The following graphs show the same analysis but for the terms 'gender' or 'transgender.' 
#' 
## ----nsf3b, echo=FALSE, warning=FALSE, error=FALSE-------------------------------------------------------------------------------------------------
n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(gender, transgender)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 24. All NSF grants with terms 'gender' and 'transgender' by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 24.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 24.svg"), height = 4,  width = 6, dpi = 300)

n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(gender_trans_bool = ifelse(gender == TRUE | transgender == TRUE, 1, 0)) %>% 
  group_by(year, gender_trans_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(gender_trans_bool, "Grants with gender-related language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 25. NSF grants for gender-related and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 25.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 25.svg"), height = 4,  width = 6, dpi = 300)

#' Following 
#' 
## ----nsf3c, echo=FALSE, warning=FALSE, error=FALSE-------------------------------------------------------------------------------------------------
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
  theme_classic() +
  labs(title = "Fig 26. All NSF grants with DEI-related terms by year",
       x = "year", y = "count")


ggsave(glue("{here}/graphs/Fig 26.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 26.svg"), height = 8,  width = 6, dpi = 300)


#' 
#' ## National Institutes of Health
#' 
#' NIH data is simpler to process than NSF data. Given the size of the database and the apparent inability to query the entire corpus at once, the data below comes from targeted searches for DEI-specific terms. The searches queried the abstract and headline for the following 10 DEI-related terms. We were able to obtain their absolute frequency over time, but not what portion of total grants included DEI terms, or what portion of total grant funding each year involved such terms. 
#' 
## ----nih1, echo=FALSE, warning=FALSE, error=FALSE, fig.height=14-----------------------------------------------------------------------------------
nih <- read_fst(glue("{here}/out/grants/nih_parsed_all.fst"))
nih <- as.data.table(nih)
nih[search_term != "discrimination"] %>% 
  count(search_term, year = fiscal_year) %>% 
  mutate(search_term = ifelse(search_term == "anti-racism", "anti-racism", str_replace_all(search_term, "-", " "))) %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~search_term, ncol = 2, scales = "free") +
  geom_line() + 
  geom_jitter() +
  theme_classic() +
  labs(title = "Fig 27. All NIH grants with DEI-related terms by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 27.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 27.svg"), height = 8,  width = 6, dpi = 300)


#' 
#' ## Ford Foundation
#' 
#' The Ford Foundation publishes their full grant database. We accessed the data between 2010 and 2022, consisting of 16,630 grants. Only grants between 2013 and 2021 were used, because of an absence of 2012 data.
#' 
#' Following is a breakdown of all grants that contained some combination of STEM and DEI terminology in either the title or the grant description.
#' 
## ----ff0a, echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------
stem_terms <- read_lines(glue("{here}/out/stem_terms.txt"))
dei_terms <- read_lines(glue("{here}/out/dei_terms.txt"))
more_dei_terms <- c("women", "minorities", "minority", "marginal", "marginalized")
dei_terms <- c(dei_terms, more_dei_terms)

ff <- fread(glue("{here}/out/grants/ford_foundation_grant_db_20220307.csv"), fill = T) %>% 
  clean_names()
ff <- ff[fiscal_year > "2012" & fiscal_year < "2022"]
ff[, description_lower := str_to_lower(description)]
ff[,dei_bool := ifelse(description_lower %like% str_to_lower(glue_collapse(dei_terms, sep = "|")) | subjects %like% str_to_lower(glue_collapse(dei_terms, sep = "|")), 1, 0)]
ff[,stem_bool := ifelse(description_lower %like% str_to_lower(glue_collapse(stem_terms, sep = "|")) | subjects %like% str_to_lower(glue_collapse(stem_terms, sep = "|")), 1, 0)]
ff[,stem_dei_bool := ifelse(stem_bool == 1 & dei_bool == 1, 1, 0)] 
setnames(ff, "fiscal_year", "year")

ff %>% 
  group_by(year, stem_bool, dei_bool) %>% 
  mutate(amount = parse_number(amount)) %>% 
  summarise(amount = sum(amount), .groups = "drop") %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(amount = log(amount)) %>% 
  mutate(status = case_when(stem_bool == 1 & dei_bool == 1 ~ "STEM + DEI grants", 
                            stem_bool == 0 & dei_bool == 0 ~ "Non-STEM non-DEI grants", 
                            stem_bool == 1 ~ "STEM-related grants", 
                            dei_bool == 1 ~ "DEI-related grants")) %>% 
  ggplot(aes(x = year1, y = amount, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank())  +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 28. Ford Foundation grants to DEI and STEM-related projects",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "div") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2014-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 28.png"), height = 5,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 28.svg"), height = 5,  width = 6, dpi = 300)


#' 
#' The data shows that (in the wheat and brown colors, above) the dollar amount going to DEI-related grants is almost as much as that going to all other grants, and also (in the aqua and teal colors below) that of the funding dedicated to all STEM-related grants, nearly as much is given to DEI-related projects as non-DEI related. 
#' 
#' The Ford Foundation's grant database also contains a column for "Benefiting Populations." Of the total 352 grants that contain both STEM and DEI language, 208 of these refer to "BIPOC" (Black, Indigenous, People or Color), females, or transgender/non-binary populations. Of the remaining 144, only five indicate some other beneficary population ("United States' race ethnicity" three times, "West Africa's race ethnicity", and "Quilombolas"), while the rest do not declare any.
#' 
## ----echo=FALSE, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------
ff[stem_dei_bool == 1 & str_to_lower(benefiting_populations) %like% "latin|black|minority|asian|female|color|indigenous|native|trans", bipoc_female := 1]
ff[is.na(bipoc_female), bipoc_female := 0]
ff[stem_dei_bool == 1 & str_to_lower(benefiting_populations) %like% "race ethnicity|West Africa|Quilombolas", other_beneficiary := 1]
ff[is.na(other_beneficiary), other_beneficiary := 0]

ff[stem_dei_bool == 1 & year < "2021-01-01"] %>% 
  group_by(year, bipoc_female) %>% 
  mutate(amount = parse_number(amount)) %>% 
  summarise(amount = sum(amount), .groups = "drop") %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(amount = log(amount)) %>% 
  mutate(status = case_when(bipoc_female == 1 ~ "BIPOC/female/trans", 
                            bipoc_female == 0 ~ "None",
                            # other_beneficiary == 1 ~ "Other beneficiary population"
                            )) %>% 
  ggplot(aes(x = year1, y = amount, colour = as.factor(status))) + 
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank())  +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 29. Ford Foundation grants to DEI and STEM projects \nwith identified beneficiary populations",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2014-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 29.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 29.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' # Scientific literature
#' 
#' In this section, we address the prevalence of DEI terminology in published scientific literature. Many of the previous sections were concerned with areas closely *related* to scientific work, but here we are interested specifically in the most direct outputs of the scientific process: publications.
#' 
#' We explore four datasets: the Web of Science, Google Scholar, arXiv, and PubMed. These are among the largest and most authoritative scientific databases in the world, each with its own focus and characteristics, discussed below.
#' 
#' ## Web of Science
#' 
#' Web of Science, published by Clarivate, is one of the largest commercial citation indexes in the world. It contains 1.9 billion cited references from over 171 million records. We used the Web of Science online search interface in January and March 2022, searching inside the following citation indexes: *Science Citation Index Expanded (SCI-EXPANDED), Conference Proceedings Citation Index ??? Science (CPCI-S), Current Chemical Reactions (CCR-EXPANDED), Index Chemicus (IC), Emerging Sources Citation Index (ESCI).* We searched the default Topic field. We excluded the three social science indexes: Social Sciences Citation Index (SSCI), Arts & Humanities Citation Index (AHCI), and Conference Proceedings Citation Index ??? Social Science & Humanities (CPCI-SSH). 
#' 
#' As with searches of the NIH database, obtaining the full database was not feasible. Searches are thus simple DEI terms, with the control phrase "science" to ensure that the patterns observed in the DEI terms indeed represent the overall content of the database.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
wos <- fread(glue("{here}/out/scholarship/march_2022_searches.csv"), sep = "|")
wos <- wos[year > 2004 & year < 2022]

wos[year > 2005 & year < 2022] %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  rename(count = hits) %>% 
  mutate(count = parse_number(count)) %>% 
  # filter(year > "2014-01-01") %>% 
  ggplot(aes(x = year, y = count)) +
  facet_wrap(~topic, scales = "free", ncol = 2, nrow = 5) +
  geom_line() + 
  theme_classic()  +
  labs(title = "Fig 30. Web of Science count of DEI terms over time (with control)",
       x = "time", y = "count") +
  scale_color_brewer(type = "qual") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 30.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 30.png"), height = 8,  width = 6, dpi = 300)


#' The above shows obvious growth in DEI-related terms in science-only indexes in particular since 2016 -- but this conceals how rapidly DEI terms have grown and over how many orders of magnitude. The control term, "science" (covering all publications in general, obtained by searching in the Web of Science Categories), is meant as a control to capture the total number of citations captured by Web of Science in general. The general publication trend reflect a rise over time -- but this is quite unlike the sharp uptick shown in DEI-related terms. These grow through several orders of magnitude, beginning at zero and within four years growing to hundreds or thousands. 
#' 
#' The simplest way of representing this differential is the average growth rate for all of the years under observation for each of the categories. This is shown in the following table. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
GrowthRate <- function(x) (x/lag(x)-1)*100 
MakeGrowthRateTable <- function(x){
  r1 <- GrowthRate(as.integer(rev(wos[topic==x]$hits)))
  r1 <- r1[!is.na(r1)]
  r1 <- mean(r1)
  r1 <- tibble(topic = x, average_growth_rate = r1)
  return(r1)
}
growth_rates <- map_df(unique(wos$topic), ~MakeGrowthRateTable(.x))
growth_rates <- growth_rates %>% arrange(average_growth_rate)
growth_rates$average_growth_rate <- paste0(round(growth_rates$average_growth_rate, 0),  "%")

#' *Average annual rates of growth of different topics in the Web of Science,  2010-2021*
#' 
#' |Topic                   | Rate |
#' |:--------------------------|:-------------------|
#' |science                    |5%                  |
#' |social justice             |15%                 |
#' |racism                     |32%                 |
#' |sexism                     |45%                 |
#' |critical race theory       |62%                 |
#' |diversity equity inclusion |73%                 |
#' |intersectionality          |107%                |
#' |systemic racism            |140%                |
#' |white supremacy            |190%                |
#' |anti racism                |211%                |
#' 
#' This table shows that the growth of reports about antiracist and DEI-topics has grown between 3 and 42 times faster than scientific topics in general in the Web of Science from 2010 to 2021.
#' 
#' ## Google Scholar
#' 
#' Google Scholar is one of the largest repositories of academic and periacademic publications in the world. In comparison to Web of Science, Google Scholar contains a much larger number of preprints or otherwise informally published works. This difference is a helpful distinguishing feature, because it means data from Google Scholar will not be identical to data from Web of Science. 
#' 
#' Google Scholar does not have an application programming interface (API), and Google limits the manner in which web scraping bots can interact with the site. It is thus difficult to get comprehensive information, making it infeasible to use a reliable control search as in Web of Science above. 
#' 
#' The growth of publications for a range of DEI and STEM terms over time is shown below. The data was obtained using open source software that queries Google Scholars and returns a maximum of 1000 results. These results appear to be broadly distributed over all years in which data is available, according to searches for non-DEI terms. Because these searches could not be performed programmatically, we did not satisfy every combination of DEI/STEM terms as in previous cases. Instead, there are only a sample of the total possible combinations. The findings suggest wider applicability than just these cases, however.
#' 
#' All of the searches below were inputted as single strings, i.e. `astronomy critical race theory`, or `biology diversity equity inclusion`. The control we used in this case was the search term `psychology power posing`. This is because it is well known in the social psychology literature that the theory of "power posing" went through a rise and fall in influence and popularity. Therefore, if the method of querying Google Scholar we used was roughly representative of the full content of the database (rather than severely oversampling more recent years, which would be a threat to inference), we would expect to see this reflected in the results for power posing. This is indeed the case. We thus feel comfortable that the following findings broadly reflect the actual content on these topics in the Google Scholar database. Yet another reason this claim is robust is because the query is limited to 1,000 results, and in none of our searches was the limit reached. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
gs <- read_fst(glue("{here}/out/scholarship/google_scholar.fst"))
gs %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  count(topic, year) %>% 
  filter(year < "2022-01-01" & year > "2000-01-01" & topic != "science") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) + 
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 31. Google Scholar results for DEI and STEM terms",
       x = "year", y = "count") +
  theme_classic() +
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 31.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 31.svg"), height = 8,  width = 6, dpi = 300)


#' 
#' ## arXiv
#' 
#' arXiv (pronounced "archive") is the largest and most well-known preprint server for publications in the sciences. We used the arXiv API to conduct searches on DEI-related terms. It was not necessary to search for STEM terms too, because the entire database consists of scientific preprints. 
#' 
#' Results are shown in the following figure. Three search terms -- antiracism, critical race theory, and systemic racism -- were dropped because they yielded no or almost no results.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
arxiv <- fread(glue("{here}/out/scholarship/arxiv.csv"))

arxiv %>%
  mutate(year = floor_date(date, "year")) %>% 
  count(topic, year) %>% 
  filter(topic != "antiracism" & year > 2010) %>% 
  filter(topic != "critical race theory") %>%
  filter(topic != "systemic racism") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 32. arXiv results for DEI terms",
       x = "year", y = "count") +
  theme_classic()
  # scale_y_continuous(labels = scales::comma) +
  # scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"),
  #                                            to = as.Date("2021-01-01"),
  #                                            by = "5 years"), date_labels = "%y",
  #              expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 32.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 32.svg"), height = 8,  width = 6, dpi = 300)

#' 
#' ## PubMed
#' 
#' PubMed, short for PubMed Central, a massive database of biomedical research hosted by the National Institutes of Health, is similar to arXiv and Web of Science in this analysis --- substantially everything in the database is STEM-related, and so we can query it directly for DEI-related terms. 
#' 
#' PubMed does not have an API, and so large-scale programmatic searching was impossible. We thus conducted searches directly for DEI-related terms in the frontend interface. PubMed did, however, have the best control data available so far: numbers of citations annually. This is a close representation of the growth of the overall database, and thus allows for a natural comparison to the growth of DEI-related publications. 
#' 
#' Like the other datasets, we find that use of DEI terminology grew rapidly in PubMed since 2015. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
pubmed <-  read_fst(glue("{here}/out/scholarship/pubmed.fst"))

pubmed %>%
  mutate(create_date = ymd(create_date), 
         year = floor_date(create_date, "year")) %>% 
  count(topic, year) %>%
  filter(year > "2010-01-01" & year < "2022-01-01") %>%
  ggplot(., aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2)  +
  labs(title = "Fig 33. PubMed results for DEI terms (recent)",
       x = "year", y = "count") +
  theme_classic()  +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"),
                                             to = as.Date("2021-01-01"),
                                             by = "2 years"), date_labels = "%y",
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 33.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 33.svg"), height = 8,  width = 6, dpi = 300)


#' 
#' The rapidity of the rise is more apparent when pushing back the x axis to the first time a paper mentioning one of the terms appears -- rather than since 2011 as above. That configuration is shown below.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
pubmed %>%
  mutate(create_date = ymd(create_date), 
         year = floor_date(create_date, "year")) %>% 
  count(topic, year) %>%
  ggplot(., aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2)  +
  labs(title = "Fig 34. PubMed results for DEI terms (longitudinal)",
       x = "year", y = "count") +
  theme_classic() 

ggsave(glue("{here}/graphs/Fig 34.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 34.svg"), height = 8,  width = 6, dpi = 300)


#' 
#' Yet another comparison involves looking at the growth of the overall database. In that case, it is again apparent that DEI-related publications have expanded far more rapidly than the total body of publications in PubMed. This data comes from PubMed's own "PubMed total records by publication year" (https://datadiscovery.nlm.nih.gov/dataset/PubMed-total-records-by-publication-year/eds5-ig9r) dataset. Many of the other data sources we have examined do not provide their own metadata so transparently or accessibly. The comparison with the growth of DEI in the dataset can thus be made particularly clearly here. 
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------
pubmed_control <- fread(glue("{here}/out/scholarship/PubMed_total_records_by_publication_year.csv")) %>% clean_names()

pubmed_control %>%
  mutate(year = paste0(citation_year,"-01-01"),
         year = ymd(year), 
         year = floor_date(year, "year")) %>% 
  filter(year > "1950-01-01" & year < "2022-01-01") %>%
  rename(count = x2022) %>% 
  ggplot(., aes(x=year, y=count)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Fig 34. Growth of PubMed database since 1950",
       x = "year", y = "count") +
  theme_classic() 

ggsave(glue("{here}/graphs/Fig 35.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 35.svg"), height = 4,  width = 6, dpi = 300)


#' 
#' # Technical notes
#' 
#' Much of the work in this report involves original data scraped from the internet, processed, and displayed in graphs. There are many choices at each step of that process that can potentially heavily influence the outcome. The most robust method of ensuring the integrity of such work is a transparent presentation of the exact methods used, along with the code and data, so that other analysts can scrutinize the methods and replicate them.
#' 
#' The best way for an interested reader (or counter-partisan) to do that is to look at the GitHub repository on which all the code is deposited, and then to download from Zenodo the 30gb of raw data that was used, and reproduce the analysis for themselves. This report is itself written in a combination of English and computer code (using R), and is compiled by a number of pieces of software (R, pandoc, etc.) 
#' 
#' To assist interested parties in reproducing the analysis, following are some notes that explain where to find the files and what to do with them, based on the above report. Given that much of this is obvious once examining the files, these notes only deal with a few of the sections. 
#' 
#' ## 1. University websites
#' 
#' 1.  Each university name was extracted from the QS Top Universities list of the 100 top universities in the US as of April 2021.^[O, Craig. "Ranked: The Top 100 Universities In The USA". Top Universities. https://www.topuniversities.com/where-to-study/north-america/united-states/ranked-top-100-us-universities] A dozen of these were updated with more technically-oriented universities. Each university's domain name (i.e. website) was then obtained from other online resources. These were used in combination with DEI and STEM terms to search the Google Search application programming interface (API) for all web pages on those websites that contained a given combination of STEM terms. The script paginated until no more results were reached (for a maximum of 100 results per DEI + STEM + university combination). All results were stored in raw JSON format.^[These are in the `./data/school_websites/json` folder in the project's replication files.]
#' 
#' 2.  This resulted in 8,718 JSON files, each containing a maximum of 10 links. Given that some of these links were dead, duplicates, or otherwise corrupt, and not each file contained the maximum possible 10 links, we were able to download and save to disk 116,324 files. We passed each URL to a hash function (a one-way function that will turn a given string to another string of some fixed length - in this case 32 characters), resulting in files of the form: `school-name_dei-term_stem-term_hash.html`, or, for example: `wpi_inclusion_chemistry_1446e6d34af76ef78c9edf973aebcd3b.html` (where "wpi" is Worcester Polytechnic Institute). We then converted these HTML files to text with the Linux utilities `html2text` and `lynx -dump`. We were able to convert substantially all of them to text, though nearly 2,000 resulted in 0, 1, or 2 byte files, meaning the HTML could not be parsed and the result was empty. Hashing the URLs meant that each file could be uniquely linked to a URL in the dataset (because it is unwieldy to use URLs as file names).
#' 
#' 3.  We then developed and refined a number of regular expressions to extract dates from each of these pages. We filtered out any page that included more than a single date, in order to avoid counting a large number of dates on a single page. This often happens with event listings, and adds noise to the data. As we have designed this enquiry, each date pulled from a page becomes a datapoint --- an instance of a DEI term and a STEM term being on the same page. When performing this operation, the number of observations in the dataset decreases from over 198,710 to just 10,115
#' 
#' 4.  The data is presented below as a raw count across all the pages, and then in a series of different combinations (i.e. Ivy League vs non-Ivy etc.)
#' 
#' 5.  The data was initially collected in September and October of 2021, and the code reran in January of 2022. The results are current as of mid January 2022.
#' 
#' We used the Google JSON Search API to construct the search calls. Each search call consisted of a combination of a DEI term, STEM term, and school.
#' 
#' These were the DEI and STEM terms used.
#' 
#' `dei_terms <- c("diversity", "equity", "inclusion", "systemic racism", "white supremacy" , "anti-racism", "justice", "privilege", "critical race theory")`
#' 
#' `stem_terms <- c("astronomy", "biology", "chemistry", "engineering", "geology", "mathematics", "meteorology", "oceanography", "physics", "science")`
#' 
#' When mapping all unique combinations of these two terms across the 100 universities, this led to 9,000 unique combinations, and thus 9,000 searches.
#' 
#' The `R` files associated with this task are:
#' 
#' -   `1.analyze_json.R`
#' 
#' -   `1.analyze_school_websites.R`
#' 
#' -   `1.clean_school_website_data.R`
#' 
#' -   `1.clean_schools.R`
#' 
#' -   `1.download_school_websites_curl.R`
#' 
#' -   `1.dump_html_txt.R`
#' 
#' -   `1.parse_json_to_csv.R`
#' 
#' -   `1.pull_school_websites_json_api.R`
#' 
#' -   `1.school_list_url.R`
#' 
#' All of the data files associated with the task are in `./out/school_websites/`.
#' 
#' The regular expressions for pulling the dates out of the text files look like this: `"\\b(3[01]|[12][0-9]|0?[1-9])/(1[0-2]|0?[1-9])/(?:[0-9]{2})?[0-9]{2}\\b"` and `"\\b(((Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|` `Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2},\\s+\\d{4}))\\b"`.
#' 
#' Together they'll capture most date combinations, such as Mar 1, 2011, April 19, 2020, 10/7/17, 01/09/2021, etc.
#' 
#' ## 2. Twitter
#' 
#' The `R` files associated with this task are:
#' 
#' -   `2.pull_twitter_feeds.R`
#' -   `2.parse_twitter_data.R`
#' 
#' All of the data files associated with the task are in `./out/twitter/`.
#' 
#' ## 3. academic associations
#' 
#' The `R` files associated with this task are:
#' 
#' -   `3.downloads_learned_societies.R`
#' -   `3.clean_learned_societies_data.R`
#' -   `3.convert_pdf_text.R`
#' 
#' All of the data files associated with the task are in `./out/learned_societies/`.
#' 
#' The analysis of the grant and scholarship data follows an identical format: The `R` files are designated by the "4." and "5." prefix respectively, and the relevant files are appropriately named in the `./out/` and `./data/` folders. 
