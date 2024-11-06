# Working directory -------------------------------------------------------------------
getwd()

# Libraries -------------------------------------------------------------------------------
library(dplyr) # for easier operation on dataframes
library(tidyr) # for easy handling messy data

# Reading the original data -----------------------------------------------------------------------------

# CEPII
# In the project I use the 202211 version of the database.
# It may be dowloaded from https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# Due to its huge size (over 100 MB) I could not include it in the GitHub repository.

path <-  r"(C:\Users\tymme\Dropbox\Studia\Podstawowe_modele_panelowe\Gravity_rds_V202211\Gravity_V202211.rds)"
gravity <- readRDS(path)

str(gravity)

# Actual data preparation ----------------------------------------------------------------------------

# Checking which columns contains only description of primary sources
sources <- grepl("source",colnames(gravity))
colnames(gravity)[sources]
# They may be deleted to reduce the size of the table
gravity <- gravity %>% select(-colnames(gravity)[sources])
rm(sources)

# Trimming the observations to years 2000-2020
# For the year 2021 (newest in the database) observations have many NAs.
# For the years before 2000 there are also many NAs + theoretical explanation in the article (specifi hypothesis being tested that require relatively contemporary data).
gravity <- gravity[gravity$year>1999 & gravity$year<2021, ]

# Deleting non-existing states
gravity <- gravity %>% 
  filter(country_exists_o == 1) %>%
  filter(country_exists_d ==1) %>%
  select(-country_exists_o, -country_exists_d)

# Deleting trade within one country and deleting additional columns
gravity <- gravity %>% 
  filter(country_id_o != country_id_d) %>%
  select(-iso3num_o, -iso3num_d, -iso3_o, -iso3_d)

# Adding a unique index for every trade connection
gravity$ID <- paste(gravity$country_id_o, gravity$country_id_d, sep="")

# Moving the index to the first column
gravity <- gravity %>% relocate(ID)

# checking whether any duplicates exist
gravity$ID_year <- paste(gravity$ID, gravity$year, sep="")

dup <- gravity %>% 
  group_by(ID_year) %>%
  filter(n()>1) %>%
  ungroup()

length(dup$year)
# There are none, we may proceed further
rm(dup)

# Deleting connections, for which important variables for the analysis are NA or for which GDP or distance equals 0
gravity <- gravity %>%
  drop_na(gdp_o, gdp_d, dist, distw_harmonic, contig, comlang_ethno, eu_o, eu_d) %>%
  filter(gdp_o != 0) %>%
  filter(gdp_d != 0) %>%
  filter(dist != 0) %>%
  filter(distw_harmonic !=0)

# Generating logs of selected variables
gravity$lnGDP_o = log(gravity$gdp_o)
gravity$lnGDP_d = log(gravity$gdp_d)
gravity$lnDIST = log(gravity$dist)
gravity$lnDIST_h <- log(gravity$distw_harmonic)
gravity$lnGDP_cap_o = log(gravity$gdpcap_o)
gravity$lnGDP_cap_d = log(gravity$gdpcap_d)

# Balanced datasets ----------- --------------------------------------------------------------------
# Creating a balanced panel (keeping only those connections, for which I have observations for full 21 years)
gravity_balanced_all <- gravity %>%
  group_by(ID) %>%
  filter(n() == 21) %>%
  ungroup()

# Only those connections, where trade is positive (where any trade was registered whatsoever). Making sure it is still balanced.
gravity_balanced_positive <- gravity %>%
  filter(tradeflow_baci > 0)%>%
  group_by(ID) %>%
  filter(n() == 21) %>%
  ungroup()
# Saving the tables ----------------------------------------------------------------------------------------------------------------------

# Most effective way to save a file (in terms of required storage space) is RDS file
saveRDS(gravity_balanced_all, file = "Data/gravity_balanced_all.rds")
saveRDS(gravity_balanced_positive, file = "Data/gravity_balanced_positive.rds")

# One may like to save a file to csv as well
#write.csv(gravity_balanced_all, file = "gravity_balanced_all.csv")
#write.csv(gravity_balanced_positive, file = "gravity_balanced_positive.csv")
