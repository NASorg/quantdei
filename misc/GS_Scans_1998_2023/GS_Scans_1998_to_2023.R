# Google Scholar Scans for "Woke" Publications - 1998 to 2023
# By Mr. Mason Goad, Research Fellow
# The National Association of Scholars

# Set Working Directory =======================================================
getwd()
setwd("C:/Users/mason/OneDrive/Desktop/RStudio/GS_Scans_1998_2023")

# Load Required Libraries ====================================================
require(janitor)
require(dplyr)
require(tidyverse)
require(scales)
require(ggplot2)
require(ggthemes)

# 1. Clean Scans for "Diversity | Equity | Inclusion" ========================

r1 <- read.csv("1_diversity_equity_inclusion.csv")

r1_i = c(
  "diversity, equity, inclusion", "Diversity, Equity, Inclusion",
  "diversity and inclusion", "Diversity and Inclusion",
  "Black People", "Black Men", "Black Women",
  "Homosexuality", "LGBT", "LGBTQIA", "Cisgender", "Cisnormative",
  "diversity in elementary", "equity in academia",
  "feature diversity?", "Linguistic diversity", "diversity initiatives",
  "Social inclusion", "Racism", "racism", "Anti-Racism", "anti-racism",
  "Antiracism", "antiracism", "group inclusion", "workers with disabilities",
  "diversity ideals in academic", "gender diversity", "equity impact", 
  "higher education", "Higher Education", "University", "Universities",
  "university", "universities", "College", "college", "Colleges", "colleges",
  "Has inclusion gone", "health equity", "Inclusion of the other:",
  "Equity-based", "equity-based", "Gender diversity", 
  "social impacts of inclusion", "decolonizing diversity", 
  "culturally responsive", "learning and equity", "diversity research",
  "digital humanities", "intersectional", "Intersectional", "Justice",
  "justice", "social justice", "Social justice", "Social Justice",
  "language diversity", "equity, environment", "intellectual disabilities",
  "diversity characteristics", "Beyond inclusion:", "committment to equity",
  "Diversity and inclusion", "Cultural diversity", "equity is addressed",
  "diversity of conceptualizations", "Pluralism:", "minorities", "Minorities",
  "homophobia", "climate change", "University diversity", 
  "diversity to inclusion", "Gender equity", "gender equity", 
  "Multiculturalism", "multiculturalism", "Campus", "campus", 
  "Affirmative action", "affirmative action", "Leadership and diversity",
  "Transgender", "transgender", "Ethnic diversity", "Diversity management",
  "social inclusion", "social responsibility", "create equity", 
  "to inclusion", "inclusion attitude", "Sexual orientation", 
  "sexual orientation", "Gender and sex diversity", "power of diversity",
  "diversity in the digital age", "Equity in health", "diversity affect",
  "Campus climate", "campus climate", "gay", "bisexual", 
  "Equity and achievement", "Inclusion and burnout", "Educational equity",
  "social equity", "equity in health", "Inclusion and democracy",
  "Equity and excellence", "Cultural diversity", "Women", "women",
  "work group diversity", "Enhancing diversity", "race", "Race"
)

c1 <- r1 %>% filter(str_detect(Title, paste(r1_i, collapse = '|')))
 
# 2. Clean Scans for "Anti-Blackness | Anti blackness" =========================
r2 <- read.csv("2_anti_blackness.csv")

r2_i = c(
  "Blackness", "blackness", "Anti-blackness", "Antiblackness", "anti-blackness",
  "White Supremacy","white supremacy", "Racism", "racism", "anti-Blackness",
  "Anti-racism", "Black", "settler colonial", "white rage", "Blackness", 
  "Diversity", "diversity", "Inclusion", "inclusion",
  "Black students", "racial"
)


c2 <- r2 %>% filter(str_detect(Title, paste(r2_i, collapse = '|')))

 
# 3. Clean Scans for "Critical Race Theory | critical race theory" ==============
r3 <- read.csv("3_critical_race_theory.csv")

r3_i = c(
  "Critical race", "Critical Race", "critical race", "white racism",
  "Racism", "racism", "Queer", "queer", "white people", "racial", 
  "intersectionality","Intersectionality", "Race", "race", "diversity",
  "Diversity", "queer", "oppression", "Oppression", "heirarchy", "heirarchies",
  "Inequality", "inequality", "culturally responsive", "Culturally responsive",
  "Indigenous", "indigenous", "Colonialism", "colonialism", "Feminist",
  "Feminism", "feminism", "feminist", "Gender", "gender", "ethnicity",
  "Ethnicity", "White", "white", "Postcolonial", "postcolonial", "African",
  "minority", "Minority", "minorities", "Minorities", "civil rights", 
  "Civil Rights", "Social Justice", "social justice", "Black", "black",
  "neocolonial", "neo-colonial", "racists", "Ain't", "Discrimination",
  "discrimination", "Queering", "queering", "Microaggression", "microaggression",
  "Micro-aggression", "micro-aggression"
  )

c3 <- r3 %>% filter(str_detect(Title, paste(r3_i, collapse = '|')))

# 4. Clean Scans for "Anti-Racism | Anti racism | etc." ========================
r4 <- read.csv("4_anti_racism.csv")

r4_i = c(
  "Anti-Racism", "anti-racism", "Antiracism", "antiracism",
  "Blackness", "blackness", "racist", "racists", "Racist", "Racists",
  "Race", "race", "Racism", "racism", "Whiteness", "whiteness",
  "racisms", "Black Power", "black power", "multiculturalism",
  "Feminist", "Feminism", "feminist", "feminism",
  "Angela Y. Davis", "Angela Davis", "Power", "power", "privilege",
  "Privilege", "White", "white", "Black", "black", "postracial", "post-racial"
  )


c4 <- r4 %>% filter(str_detect(Title, paste(r4_i, collapse = '|')))
 
# 5. Clean Scans for "Transgender | Trans-gender | etc." ========================
r5 <- read.csv("5_transgender.csv")

r5_i = c(
  "Transgender", "Trans-gender", "transgender", "trans-gender",
  "Gender", "gender", "Queer", "queer"
)

c5 <- r5 %>% filter(str_detect(Title, paste(r5_i, collapse = '|')))
 
# 6. Clean Scans for "Queer | Queering | etc." =================================
r6 <- read.csv("6_queer.csv")

r6_i = c(
  "Queer", "queer", "Queering", "queering"
)

c6 <- r6 %>% filter(str_detect(Title, paste(r6_i, collapse = '|')))


# 7. Clean Scans for "Postcolonialism | Post-Colonialism | etc." ===============
r7 <- read.csv("7_postcolonialism.csv")

r7_i = c(
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial"
)

c7 <- r7 %>% filter(str_detect(Title, paste(r7_i, collapse = '|')))
 
# 8. Clean Scans for "LGBT | LGBTQIA ===========================================
r8 <- read.csv("8_LGBT.csv")

r8_i = c(
  "LGBT", "LGBTQIA", "Gay", "Lesbian", "Bisexual", "Transgender",
  "gay", "lesbian", "bisexual", "transgender"
)

c8 <- r8 %>% filter(str_detect(Title, paste(r8_i, collapse = '|')))
 
# 9. Clean Scans for "Systemic Racism | Systemic Oppression" ===================
r9 <- read.csv("9_systemic_racism.csv")

r9_i = c(
  "Systemic Racism", "Systemic racism", "systemic racism", 
  "Oppression", "oppression", 
  "Systemic Oppression", "Systemic oppression", "systemic oppression",
  "LGBT", "LGBTQIA", "Gay", "Lesbian", "Bisexual", "Transgender",
  "gay", "lesbian", "bisexual", "transgender", 
  "diversity, equity, inclusion", "Diversity, Equity, Inclusion",
  "diversity and inclusion", "Diversity and Inclusion",
  "Black People", "Black Men", "Black Women",
  "Homosexuality", "LGBT", "LGBTQIA", "Cisgender", "Cisnormative",
  "diversity in elementary", "equity in academia",
  "feature diversity?", "Linguistic diversity", "diversity initiatives",
  "Social inclusion", "Racism", "racism", "Anti-Racism", "anti-racism",
  "Antiracism", "antiracism", "group inclusion", "workers with disabilities",
  "diversity ideals in academic", "gender diversity", "equity impact", 
  "higher education", "Higher Education", "University", "Universities",
  "university", "universities", "College", "college", "Colleges", "colleges",
  "Has inclusion gone", "health equity", "Inclusion of the other:",
  "Equity-based", "equity-based", "Gender diversity", 
  "social impacts of inclusion", "decolonizing diversity", 
  "culturally responsive", "learning and equity", "diversity research",
  "digital humanities", "intersectional", "Intersectional", "Justice",
  "justice", "social justice", "Social justice", "Social Justice",
  "language diversity", "equity, environment", "intellectual disabilities",
  "diversity characteristics", "Beyond inclusion:", "committment to equity",
  "Diversity and inclusion", "Cultural diversity", "equity is addressed",
  "diversity of conceptualizations", "Pluralism:", "minorities", "Minorities",
  "homophobia", "climate change", "University diversity", 
  "diversity to inclusion", "Gender equity", "gender equity", 
  "Multiculturalism", "multiculturalism", "Campus", "campus", 
  "Affirmative action", "affirmative action", "Leadership and diversity",
  "Transgender", "transgender", "Ethnic diversity", "Diversity management",
  "social inclusion", "social responsibility", "create equity", 
  "to inclusion", "inclusion attitude", "Sexual orientation", 
  "sexual orientation", "Gender and sex diversity", "power of diversity",
  "diversity in the digital age", "Equity in health", "diversity affect",
  "Campus climate", "campus climate", "gay", "bisexual", 
  "Equity and achievement", "Inclusion and burnout", "Educational equity",
  "social equity", "equity in health", "Inclusion and democracy",
  "Equity and excellence", "Cultural diversity", "Women", "women",
  "work group diversity", "Enhancing diversity", "race", "Race"
)

c9 <- r9 %>% filter(str_detect(Title, paste(r9_i, collapse = '|')))
 
# 10. Clean Scans for "Drag Queen | Drag King" ================================
r10 <- read.csv("10_drag_queen_king.csv")

r10_i = c(
  "Drag Queen", "Drag queen", "Drag King", "Drag king", "Masculine", "Feminine",
  "Cisgender", "cisgender", "Cis-Gender", "Cis-gender", "cis-gender",
  "Cisgendered", "cisgendered", "Cis-Gendered", "Cis-gendered", "cis-gendered",
  "Erotic", "erotic", "Drag kinging", "Drag queening", "Heteronormativity",
  "heteronormativity", "Male", "male", "Female", "female", "GENDERED POWER", 
  "Queer", "queer", "QUEER", "Trans youth", "transgender", "Gay", "gay",
  "Devil in Drag", "Eroticizing", "Animal drag", "trans people", "Drag and Trans",
  "RuPaul", "Masculine", "masculine", "Queerness", "queerness", "gender",
  "Gender")

c10 <- r10 %>% filter(str_detect(Title, paste(r10_i, collapse = '|')))


# 11. Clean Scans for "Gender Binary | Gender Spectrum" ========================
r11 <- read.csv("11_gender_binary.csv")

r11_i = c(
  "Gender", "gender", "Gendered", "gendered", "Genderqueer", "genderqueer",
  "Non-binary", "non-binary", "Trans-affirmative", "Trans-affirming",
  "trans-affirmative", "trans-affirming", "Gender-affirmative",
  "gender-affirmative", "Transgender", "transgender", "gender dysphoria",
  "Gender dysphoria", "feminist", "feminism", "gender diverse",
  "Gender ideology", "gender ideology"
)

c11 <- r11 %>% filter(str_detect(Title, paste(r11_i, collapse = '|')))

 
# 12. Clean Scans for "Fat Studies | Fat Liberation" ===========================
r12 <- read.csv("12_fat_studies.csv")

r12_i = c(
  "Fat studies", "Fat Studies", "fat studies", "Fat liberation", "Fat Liberation",
  "fat liberation", "Fat Acceptance", "Fat acceptance", "fat acceptance",
  "Feminism", "feminism", "Feminist", "feminist", "Fat Pedagogy", 
  "Fat pedagogy", "fat pedagogy", "Fat activism", "Fat Activism",
  "fat activism" ,"Gender", "gender", "Gendered", "gendered", "Genderqueer", 
  "genderqueer", "Non-binary", "non-binary", "Trans-affirmative", 
  "Trans-affirming", "trans-affirmative", "trans-affirming", 
  "Gender-affirmative", "gender-affirmative", "Transgender", "transgender", 
  "gender dysphoria", "Gender dysphoria", "feminist", "feminism", 
  "gender diverse", "Gender ideology", "gender ideology", "gendered fat",
  "hair/style", "food justice", "Food justice", "Critical theory",
  "Critical Theory", "critical theory", "Liberating", "liberating",
  "Marcuse", "Foucault", "liberation theology", "gender studies",
  "Marxist", "neo-Marxist", "whiteness", "race"
)

c12 <- r12 %>% filter(str_detect(Title, paste(r12_i, collapse = '|')))

# 13. Clean Scans for "Postmodernism | Post-modernism" =========================
r13 <- read.csv("13_postmodernism.csv")

r13_i = c(
  "Postmodernism", "postmodernism", "Post-modernism", "post-modernism"
)

c13 <- r13 %>% filter(str_detect(Title, paste(r13_i, collapse = '|')))

# 14. Clean Scans for "Critical Theory | critical theory" ======================
r14 <- read.csv("14_critical_theory.csv")

r14_i = c(
  "Critical Theory", "Critical theory", "critical theory",
  "Marcuse", "Frankfurt School", "sexual politics", "Sexual politics",
  "Gender", "gender", "Gendered", "gendered", "Critical Race Theory",
  "critical race theory", "Critical race theory", "Critical disability theory",
  "Critical Disability Theory", "critical disability theory", 
  "Queer Theory", "Queer theory", "queer theory", "Critical Legal Studies",
  "Critical legal studies", "critical legal studies", 
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial",
  "Queer", "queer", "Queering", "queering", "Critical disability",
  "critical disability"
)

c14 <- r14 %>% filter(str_detect(Title, paste(r14_i, collapse = '|')))

# 15. Clean Scans for "Diversity, Equity, and Inclusion | DEI" (PT2) ===========
r15 <- read.csv("15_diversity_equity_inclusion_pt2.csv")

r15_i = c(
  "Critical Theory", "Critical theory", "critical theory",
  "Marcuse", "Frankfurt School", "sexual politics", "Sexual politics",
  "Gender", "gender", "Gendered", "gendered", "Critical Race Theory",
  "critical race theory", "Critical race theory", "Critical disability theory",
  "Critical Disability Theory", "critical disability theory", 
  "Queer Theory", "Queer theory", "queer theory", "Critical Legal Studies",
  "Critical legal studies", "critical legal studies", 
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial",
  "Queer", "queer", "Queering", "queering", "Critical disability",
  "critical disability",
  "diversity, equity, inclusion", "Diversity, Equity, Inclusion",
  "diversity and inclusion", "Diversity and Inclusion",
  "Black People", "Black Men", "Black Women",
  "Homosexuality", "LGBT", "LGBTQIA", "Cisgender", "Cisnormative",
  "diversity in elementary", "equity in academia",
  "feature diversity?", "Linguistic diversity", "diversity initiatives",
  "Social inclusion", "Racism", "racism", "Anti-Racism", "anti-racism",
  "Antiracism", "antiracism", "group inclusion", "workers with disabilities",
  "diversity ideals in academic", "gender diversity", "equity impact", 
  "higher education", "Higher Education", "University", "Universities",
  "university", "universities", "College", "college", "Colleges", "colleges",
  "Has inclusion gone", "health equity", "Inclusion of the other:",
  "Equity-based", "equity-based", "Gender diversity", 
  "social impacts of inclusion", "decolonizing diversity", 
  "culturally responsive", "learning and equity", "diversity research",
  "digital humanities", "intersectional", "Intersectional", "Justice",
  "justice", "social justice", "Social justice", "Social Justice",
  "language diversity", "equity, environment", "intellectual disabilities",
  "diversity characteristics", "Beyond inclusion:", "committment to equity",
  "Diversity and inclusion", "Cultural diversity", "equity is addressed",
  "diversity of conceptualizations", "Pluralism:", "minorities", "Minorities",
  "homophobia", "climate change", "University diversity", 
  "diversity to inclusion", "Gender equity", "gender equity", 
  "Multiculturalism", "multiculturalism", "Campus", "campus", 
  "Affirmative action", "affirmative action", "Leadership and diversity",
  "Transgender", "transgender", "Ethnic diversity", "Diversity management",
  "social inclusion", "social responsibility", "create equity", 
  "to inclusion", "inclusion attitude", "Sexual orientation", 
  "sexual orientation", "Gender and sex diversity", "power of diversity",
  "diversity in the digital age", "Equity in health", "diversity affect",
  "Campus climate", "campus climate", "gay", "bisexual", 
  "Equity and achievement", "Inclusion and burnout", "Educational equity",
  "social equity", "equity in health", "Inclusion and democracy",
  "Equity and excellence", "Cultural diversity", "Women", "women",
  "work group diversity", "Enhancing diversity", "race", "Race",
  "Blackness", "blackness", "Anti-blackness", "Antiblackness", "anti-blackness",
  "White Supremacy","white supremacy", "Racism", "racism", "anti-Blackness",
  "Anti-racism", "Black", "settler colonial", "white rage", "Blackness", 
  "Diversity", "diversity", "Inclusion", "inclusion",
  "Black students", "racial", "multiculturalism",
  "Critical race", "Critical Race", "critical race", "white racism",
  "Racism", "racism", "Queer", "queer", "white people", "racial", 
  "intersectionality","Intersectionality", "Race", "race", "diversity",
  "Diversity", "queer", "oppression", "Oppression", "heirarchy", "heirarchies",
  "Inequality", "inequality", "culturally responsive", "Culturally responsive",
  "Indigenous", "indigenous", "Colonialism", "colonialism", "Feminist",
  "Feminism", "feminism", "feminist", "Gender", "gender", "ethnicity",
  "Ethnicity", "White", "white", "Postcolonial", "postcolonial", "African",
  "minority", "Minority", "minorities", "Minorities", "civil rights", 
  "Civil Rights", "Social Justice", "social justice", "Black", "black",
  "neocolonial", "neo-colonial", "racists", "Ain't", "Discrimination",
  "discrimination", "Queering", "queering", "Microaggression", "microaggression",
  "Micro-aggression", "micro-aggression", "higher education", "identity",
  "Inclusion"
  )

c15 <- r15 %>% filter(str_detect(Title, paste(r15_i, collapse = '|')))

 
# 16. Clean Scans for "Anti-Blackness | Anti blackness" (PT2) ==================
r16 <- read.csv("16_anti_blackness_pt2.csv")

r16_i = c(
  "Blackness", "blackness", "Anti-blackness", "Antiblackness", "anti-blackness",
  "White Supremacy","white supremacy", "Racism", "racism", "anti-Blackness",
  "Anti-racism", "Black", "settler colonial", "white rage", "Blackness", 
  "Diversity", "diversity", "Inclusion", "inclusion",
  "Black students", "racial", "Racism", "African", "Queer", "queer")


c16 <- r16 %>% filter(str_detect(Title, paste(r16_i, collapse = '|')))


# 17. Clean Scans for "Transgender | transgender" (PT2) ========================
r17 <- read.csv("17_transgender_pt2.csv")

r17_i = c(
  "Transgender", "Trans-gender", "transgender", "trans-gender",
  "Gender", "gender", "Queer", "queer", "Queering", "queering",
  "gendered", "Gendered", "Transphobia", "transphobia",
  "Homophobia", "homophobia", "Transfeminist", "transfeminist")

c17 <- r17 %>% filter(str_detect(Title, paste(r17_i, collapse = '|')))

 
# 18. Clean Scans for "Systemic Racism | Systemic Oppression" (PT2) ============
r18 <- read.csv("18_systemic_racism_pt2.csv")

r18_i = c(
  "Systemic Racism", "Systemic racism", "systemic racism", 
  "Oppression", "oppression", 
  "Systemic Oppression", "Systemic oppression", "systemic oppression",
  "LGBT", "LGBTQIA", "Gay", "Lesbian", "Bisexual", "Transgender",
  "gay", "lesbian", "bisexual", "transgender", 
  "diversity, equity, inclusion", "Diversity, Equity, Inclusion",
  "diversity and inclusion", "Diversity and Inclusion",
  "Black People", "Black Men", "Black Women",
  "Homosexuality", "LGBT", "LGBTQIA", "Cisgender", "Cisnormative",
  "diversity in elementary", "equity in academia",
  "feature diversity?", "Linguistic diversity", "diversity initiatives",
  "Social inclusion", "Racism", "racism", "Anti-Racism", "anti-racism",
  "Antiracism", "antiracism", "group inclusion", "workers with disabilities",
  "diversity ideals in academic", "gender diversity", "equity impact", 
  "higher education", "Higher Education", "University", "Universities",
  "university", "universities", "College", "college", "Colleges", "colleges",
  "Has inclusion gone", "health equity", "Inclusion of the other:",
  "Equity-based", "equity-based", "Gender diversity", 
  "social impacts of inclusion", "decolonizing diversity", 
  "culturally responsive", "learning and equity", "diversity research",
  "digital humanities", "intersectional", "Intersectional", "Justice",
  "justice", "social justice", "Social justice", "Social Justice",
  "language diversity", "equity, environment", "intellectual disabilities",
  "diversity characteristics", "Beyond inclusion:", "committment to equity",
  "Diversity and inclusion", "Cultural diversity", "equity is addressed",
  "diversity of conceptualizations", "Pluralism:", "minorities", "Minorities",
  "homophobia", "climate change", "University diversity", 
  "diversity to inclusion", "Gender equity", "gender equity", 
  "Multiculturalism", "multiculturalism", "Campus", "campus", 
  "Affirmative action", "affirmative action", "Leadership and diversity",
  "Transgender", "transgender", "Ethnic diversity", "Diversity management",
  "social inclusion", "social responsibility", "create equity", 
  "to inclusion", "inclusion attitude", "Sexual orientation", 
  "sexual orientation", "Gender and sex diversity", "power of diversity",
  "diversity in the digital age", "Equity in health", "diversity affect",
  "Campus climate", "campus climate", "gay", "bisexual", 
  "Equity and achievement", "Inclusion and burnout", "Educational equity",
  "social equity", "equity in health", "Inclusion and democracy",
  "Equity and excellence", "Cultural diversity", "Women", "women",
  "work group diversity", "Enhancing diversity", "race", "Race"
)

c18 <- r18 %>% filter(str_detect(Title, paste(r18_i, collapse = '|')))







 
# 19. Clean Scans for "LGBT | LGBTQIA | Queer | queer | etc." (PT2) ============
r19 <- read.csv("19_LGBT_pt2.csv")

r19_i = c(
  "LGBT", "LGBTQIA", "Gay", "Lesbian", "Bisexual", "Transgender",
  "gay", "lesbian", "bisexual", "transgender", "Queer", "queer",
  "Queering", "queering"
)

c19 <- r19 %>% filter(str_detect(Title, paste(r19_i, collapse = '|')))
 
# 20. Clean Scans for "Anti-Racism | Anti racism | etc." (PT2) =================
r20 <- read.csv("20_anti_racism_pt2.csv")

r20_i = c(
  "Anti-Racism", "anti-racism", "Antiracism", "antiracism",
  "Blackness", "blackness", "racist", "racists", "Racist", "Racists",
  "Race", "race", "Racism", "racism", "Whiteness", "whiteness",
  "racisms", "Black Power", "black power", "multiculturalism",
  "Feminist", "Feminism", "feminist", "feminism", "Kendi", 
  "Angela Y. Davis", "Angela Davis", "Power", "power", "privilege",
  "Privilege", "White", "white", "Black", "black", "postracial", "post-racial"
)


c20 <- r20 %>% filter(str_detect(Title, paste(r20_i, collapse = '|')))
 
# 21. Clean Scans for "Critical Race Theory | critical race theory" (PT2) ======
r21 <- read.csv("21_critical_race_theory_pt2.csv")

r21_i = c(
  "Critical race", "Critical Race", "critical race", "white racism",
  "Racism", "racism", "Queer", "queer", "white people", "racial", 
  "intersectionality","Intersectionality", "Race", "race", "diversity",
  "Diversity", "queer", "oppression", "Oppression", "heirarchy", "heirarchies",
  "Inequality", "inequality", "culturally responsive", "Culturally responsive",
  "Indigenous", "indigenous", "Colonialism", "colonialism", "Feminist",
  "Feminism", "feminism", "feminist", "Gender", "gender", "ethnicity",
  "Ethnicity", "White", "white", "Postcolonial", "postcolonial", "African",
  "minority", "Minority", "minorities", "Minorities", "civil rights", 
  "Civil Rights", "Social Justice", "social justice", "Black", "black",
  "neocolonial", "neo-colonial", "racists", "Ain't", "Discrimination",
  "discrimination", "Queering", "queering", "Microaggression", "microaggression",
  "Micro-aggression", "micro-aggression"
)

c21 <- r21 %>% filter(str_detect(Title, paste(r21_i, collapse = '|')))

# 22. Clean Scans for "Implicit Bias | implicit bias" ==========================
r22 <- read.csv("22_implicit_bias.csv")

r22_i = c(
  "Microaggression", "microaggression",
  "Micro-aggression", "micro-aggression", "intersectionality","Intersectionality",
  "Implicit Bias", "Implicit bias", "implicit bias", "racial bias", "gender bias",
  "Implicit Association Test", "Implicit association test",
  "implicit association test", "Banaji", "banaji", "Mahzarin Banaji",
  "Mahzarin banaji", "mahzarin banaji", "IAT", "Prejudice", "prejudice",
  "Unconcious Bias", "Unconcious bias", "unconcious bias", "African American",
  "African american", "african america", "Equity", "equity", "racism"
  )

c22 <- r22 %>% filter(str_detect(Title, paste(r22_i, collapse = '|')))



# 23. Clean Scans for "Gender Binary | Gender Spectrum (PT2) ===================
r23 <- read.csv("23_gender_binary_pt2.csv")

r23_i = c(
  "Gender", "gender", "Gendered", "gendered", "Genderqueer", "genderqueer",
  "Non-binary", "non-binary", "Trans-affirmative", "Trans-affirming",
  "trans-affirmative", "trans-affirming", "Gender-affirmative",
  "gender-affirmative", "Transgender", "transgender", "gender dysphoria",
  "Gender dysphoria", "feminist", "feminism", "gender diverse",
  "Gender ideology", "gender ideology", "Queer", "queer", "Queering",
  "queering", "social justice", "Social justice", "feminist", "Feminist",
  "Feminism", "feminism", "male", "Male", "female", "Female", "Masculine",
  "masculine", "Feminine", "feminine", "Masculinity", "masculinity",
  "Femininity", "femininity", "Intersectional", "intersectional",
  "Intersectionality", "intersectionality", "Boy", "Girl", "boy", "girl"
)

c23 <- r23 %>% filter(str_detect(Title, paste(r23_i, collapse = '|')))

 
# 24. Clean Scans for "Postcolonialism | Decolonization" (PT2) ===============
r24 <- read.csv("24_postcolonialism_pt2.csv")

r24_i = c(
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial", "Decolonization", "decolonization",
  "Decolonisation", "decolonisation", "decolonize", "Decolonize", "Edward Said",
  "Critical Theory", "Critical theory", "critical theory", "DECOLONIZATION",
  "Frantz Fanon", "Queer", "queer", "Marxism", "Marxist"
)

c24 <- r24 %>% filter(str_detect(Title, paste(r24_i, collapse = '|')))

# 25. Cleans Scans for "Critical Theory | Postmodernism" (PT2) =================
r25 <- read.csv("25_critical_theory_postmodernism_pt2.csv")

r25_i = c(
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial", "Decolonization", "decolonization",
  "Decolonisation", "decolonisation", "decolonize", "Decolonize", "Edward Said",
  "Critical Theory", "Critical theory", "critical theory", "DECOLONIZATION",
  "Frantz Fanon", "Queer", "queer", "Marxism", "Marxist",
  "Critical Theory", "Critical theory", "critical theory",
  "Marcuse", "Frankfurt School", "sexual politics", "Sexual politics",
  "Gender", "gender", "Gendered", "gendered", "Critical Race Theory",
  "critical race theory", "Critical race theory", "Critical disability theory",
  "Critical Disability Theory", "critical disability theory", 
  "Queer Theory", "Queer theory", "queer theory", "Critical Legal Studies",
  "Critical legal studies", "critical legal studies", 
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Postmodernism", "Post-modernism",
  "postmodernism", "post-modernism", "Indigenous knowledge", "Indigenous",
  "indigenous", "Postmodern", "postmodern", "feminist", "Feminist", "feminism",
  "Feminism", "postcolonial", "Postcolonial", "Queer", "queer", "Queering", 
  "queering", "Critical disability", "critical disability", "Postmodernism",
  "postmodernism", "Post-modernism", "post-modernism", "Patriarchy", "patriarchy"
)

c25 <- r25 %>% filter(str_detect(Title, paste(r25_i, collapse = '|')))



# Bind, De-Duplicate, and Filter Data ==========================================

# Step One: Bind
c_all <- rbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, 
               c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, 
               c21, c22, c23, c24, c25) 

# Step Two: De-Duplicate
c_all <- c_all %>% distinct(Title, .keep_all = T)


# Step Three: Filter
excluded_terms = c(
  "Genetic diversity", "genetic diversity", "Chemical diversity",
  "chlorhexidine", "Photodynamic nasal", "Staphylococcus", 
  "Staphylococcus aureus", "Enterobacteriaceae", 
  "guidelines on decolonization of multidrug-resistant ",
  "Chlorhexidine-Based", "Plasma brain natriuretic peptide concentration",
  "peptide", " mitochondrial DNA", "mitochondria", "somatic symptoms",
  "photic vs. nonphotic", "Raman spectroscopy", "Aquila earthquake",
  "hidden morbidities ", "emergency operators", "Intestinal microbiota",
  "average spectrum (LTAS) analysis", "gender ratio of autism spectrum",
  "Gender and geographic differences in the prevalence",
  "marketisation", "weight change over 7 years", 
  "R&D alliances and firm performance",
  "Long-term suicide risk in schizophrenia ",
  "Stage- and Gender-Specific Proteomic Analysis ",
  "Decolonization of orthopedic surgical team S. aureus carriers",
  "Unmaking the public university",
  "natalizumab therapy in patients of African descent",
  "Microbial diversity ", "Liberating Kosovo",
  "Cognitive flexibility impairments in children with autism spectrum",
  "Ecological diversity ", "Heterozygous Ambra1 Deficiency ",
  "first-episode psychosis at 5-year ", "Female mice liberated for inclusion ",
  "biological diversity", "Biodiversity",
  "gastrointestinal carriage of vancomycin-resistant Enterococcus faecium",
  "Acid Black 1 dye decolonization in aqueous",
  "Fecal microbiota transplantation for the intestinal decolonization",
  "Gut microbiota and intestinal decolonization",
  "Gut microbiota modulation for multidrug-resistant organism decolonization",
  "Augmenting the Activity of Chlorhexidin",
  "Optimization of decolorizing conditions of alizarin red",
  "Nasal decolonization: what antimicrobials",
  "Microbiome predictors of dysbiosis and VRE decolonization"
)

c_all <- c_all %>% 
  filter(!str_detect(Title, paste(excluded_terms, collapse = '|')))


# Step Four: Write New CSV
write.csv(c_all, 
"C:/Users/mason/OneDrive/Desktop/RStudio/GS_Scans_1998_2023/c_all.csv",
          row.names = FALSE)



# Graph Data and Create Sub-Categories =========================================
c_all <- read.csv("c_all.csv")

c_all %>% group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.2, color="#ba0d0d") +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1998, 2024)) +
  labs(title = "DEI Related Publications Added to Google Scholar (1998-2023)",
       x = "Time", y = "Count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"))


# Category: Race ===========================================================
race_terms = c(
  "Antiracism", "Anti-Racism", "Anti-racism", "antiracism",
  "Antiblackness", "Anti-Blackness", "Anti-blackness", "antiblackness", "anti-blackness",
  "African", "Afro", "african", "afro", "Afro-", "afro-", "Blackness", "blackness",
  "black", "Black", "Cyberracism", "cyberracism", "Racism", "racism", "Racial", "racial",
  "Racist", "racist", "Racial Bias", "Racial bias", "racial bias", "Critical Race Theory",
  "Critical race theory", "critical race theory", "White", "white", "Whiteness",
  "whiteness", "white rage", "race", "white supremacy", "Black Power", "Black power",
  "black power", "White Privilige", "White privilige", "white privilege",
  "Postracial", "Post-racial", "postracial", "post-racial", "Angela Y. Davis", "Angela Davis",
  "Minority", "minority", "Minorities", "minorities", "Civil Rights", "Civil rights",
  "civil rights", "Ethnicities", "ethnicities", "Ethnicity", "ethnicity", 
  "Multiculturalism", "multiculturalism", "Multi-culturalism", "multi-culturalism",
  "Multicultural", "multicultural", "Multi-cultural", "multi-cultural"
  )

c_race <- c_all %>% 
  filter(str_detect(Title, paste(race_terms, collapse = '|')))

c_race %>% group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.2, color="#753b08") +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1998, 2024)) +
  labs(title = "DEI Related Publications Added to Google Scholar (1998-2023) | Category: Race",
       subtitle = "Example Terms: Antiracism, Antiblackness, White Privilege, Whiteness, Etc.",
       x = "Time", y = "Count") + theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black")) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", color = "black"))
  



# Category: Sex & Gender =======================================================
sex_terms = c(
  "Transgender", "Trans-gender", "transgender", "trans-gender",
  "Gender", "gender", "Queer", "queer", "Women", "women", "Men", "men",
  "Drag Queen", "Drag queen", "Drag King", "Drag king", "Masculine", "Feminine",
  "Cisgender", "cisgender", "Cis-Gender", "Cis-gender", "cis-gender",
  "Cisgendered", "cisgendered", "Cis-Gendered", "Cis-gendered", "cis-gendered",
  "Erotic", "erotic", "Drag kinging", "Drag queening", "Heteronormativity",
  "heteronormativity", "Male", "male", "Female", "female", "GENDERED POWER", 
  "Queer", "queer", "QUEER", "Trans youth", "transgender", "Gay", "gay",
  "Devil in Drag", "Eroticizing", "Animal drag", "trans people", "Drag and Trans",
  "RuPaul", "Masculine", "masculine", "Queerness", "queerness", "gender", "Gender",
  "Gender", "gender", "Gendered", "gendered", "Genderqueer", "genderqueer",
  "Non-binary", "non-binary", "Trans-affirmative", "Trans-affirming",
  "trans-affirmative", "trans-affirming", "Gender-affirmative",
  "gender-affirmative", "Transgender", "transgender", "gender dysphoria",
  "Gender dysphoria", "feminist", "feminism", "gender diverse",
  "Gender ideology", "gender ideology", "LGBT", "LGBTQIA", "Gay", "Lesbian", 
  "Bisexual", "Transgender", "gay", "lesbian", "bisexual", "transgender", 
  "Homophobia", "homophobia", "Transfeminist", "transfeminist")

c_sex <- c_all %>% 
  filter(str_detect(Title, paste(sex_terms, collapse = '|')))

c_sex %>% group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.2, color="#fe019a") +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1998, 2024)) +
  labs(title = "DEI Related Publications Added to Google Scholar (1998-2023) | Category: Sex",
       subtitle = "Example Terms: Feminism, Women, Transgender, Queer, LGBT, Homophobia, Etc.",
       x = "Time", y = "Count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black")) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", color = "black"))


# Category: Post-Colonialism ===================================================
postcolonial_terms = c(
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Indigenous knowledge", "Indigenous",
  "indigenous", "postcolonial", "Postcolonial", "Indigenous methodologies",
  "Postcolonialism", "Post-colonialism", "postcolonialism", "post-colonialism",
  "Colonialism", "colonialism", "Indigenous knowledge", "Indigenous", "indigenous",
  "postcolonial", "Postcolonial", "Decolonization", "decolonization",
  "Decolonisation", "decolonisation", "decolonize", "Decolonize", "Edward Said",
  "DECOLONIZATION", "Frantz Fanon", "indigenous methodologies")

c_postcolonial <- c_all %>% 
  filter(str_detect(Title, paste(postcolonial_terms, collapse = '|')))

c_postcolonial %>% group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.2, color="#A88905") +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1998, 2024)) +
  labs(title = "DEI Related Publications Added to Google Scholar (1998-2023) | Category: Post-Colonialism",
       subtitle = "Example Terms: Post-Colonialism, Decolonization, Indigenous Knowledge, Etc.",
       x = "Time", y = "Count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black")) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", color = "black"))


# Category: Transgenderism =====================================================

transgender_terms = c(
  "Transgender", "Trans-gender", "transgender", "trans-gender",
  "Transfeminist", "transfeminist", "Transphobia", "transphobia",
  "Trans-affirming", "trans-affirming", "Trans-affirmative", "trans-affirmative",
  "Gender-affirming", "gender-affirming", "Gender affirming", "gender affirming",
  "Non-binary", "non-binary", "Gender dysphoria", "gender dysphoria",
  "Gender ideology", "gender ideology", "Genderqueer", "genderqueer",
  "Queer", "queer", "Queering", "queering", "Drag Queen", "Drag queen",
  "Drag King", "Drag king")

c_transgender <- c_all %>% 
  filter(str_detect(Title, paste(transgender_terms, collapse = '|')))

c_transgender %>% group_by(Year) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Year, y=total, group = 1)) + geom_line(size = 1.2, color="#BF40BF") +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(0,0)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1998, 2024)) +
  labs(title = "DEI Related Publications Added to Google Scholar (1998-2023) | Category: Transgender",
       subtitle = "Example Terms: Transgender, Gender-affirming, Gender Dysphoria, Queer, Drag Queen, Etc.",
       x = "Time", y = "Count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black")) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", color = "black"))

# Additional Information =======================================================

# Find Total Raw Material
r_all <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
               r11, r12, r13, r14, r15, r16, r17, r18, r19, r20,
               r21, r22, r23, r24, r25)

# Sample Titles
c_sample <- sample(c_all$Title, 10)
view(c_sample)

# Find The Most Productive Authors
c_authors <- c_all %>% get_dupes(Authors)

# Check Citation Count Distribution
c_all %>% group_by(Cites) %>% summarise(total =n(), .groups = 'keep') %>%
  ggplot(., aes(x=Cites, y=total, group = 1)) + #geom_line(size = 1.2, color="#ba0d0d") +
  geom_line(size = .8) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, 400)) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(0, 400)) +
  labs(title = "Distribution of Citations in the Full Google Scholar Data Set",
       x = "Number of Citations Received", y = "Number of Titles") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face ="bold", color = "black")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 11, color = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"))
