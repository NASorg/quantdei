source("./code/setup.R")


scholarship <- fread("./data/scholar/google_scholar/astronomy_critical-race-theory.csv")
scholarship <- fread("./data/scholar/google_scholar/science_diversity-inclusion-equity.csv")
scholarship <- fread("./data/scholar/google_scholar/science.csv")
scholarship <- fread("./data/scholar/google_scholar/physics_diversity-equity-inclusion.csv")
scholarship <- fread("./data/scholar/google_scholar/biology_diversity-equity-inclusion.csv")
scholarship <- fread("./data/scholar/google_scholar/stem.csv")
scholarship <- fread("./data/scholar/google_scholar/psychology_power-posing.csv")

# options(datatable.prettyprint.char=20L)

scholarship[!is.na(Year) & Year > 1910] %>% 
  count(Year) %>% 
  ggplot() +
  geom_col(aes(x = Year, y = n))
  