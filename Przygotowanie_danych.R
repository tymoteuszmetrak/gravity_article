# Środowisko robocze --------------------------------------------------------------------------
getwd()

# Biblioteki ----------------------------------------------------------------------------------
library(dplyr) # łatwiejsze operacje na ramkach danych
library(tidyr) # do łatwiejszych operacji na danych

# Czytanie danych pierwotnych -------------------------------------------------------------------

# CEPII
# W projekcie korzystam z wersji 202211 bazy danych.
# Można ją pobrać ze strony https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# Ze względu na duży rozmiar (powyżej 100 MB) nie mogłem zamieścić jej w repozytorium GitHub

path <-  r"(C:\Users\tymme\Dropbox\Studia\Podstawowe_modele_panelowe\Gravity_rds_V202211\Gravity_V202211.rds)"
gravity <- readRDS(path)

str(gravity)

# Przetwarzanie danych ------------------------------------------------------------------------

# Sprawdzenie jakie kolumny zawierają źródło danych
sources <- grepl("source",colnames(gravity))
colnames(gravity)[sources]
# Można ja usunąć, żeby baza była nieco lżejsza
gravity <- gravity %>% select(-colnames(gravity)[sources])
rm(sources)

# Ograniczam do lat 2000-2020
# Dla roku 2021 (najnowszego w bazie) obserwacje są niepełne, zawierają wiele NA.
# Dla lat przed 2000 też wiele NA + uzasadnienie teoretyczne w artykule.
gravity <- gravity[gravity$year>1999 & gravity$year<2021, ]

# Usuwam nieistniejące państwa
gravity <- gravity %>% 
  filter(country_exists_o == 1) %>%
  filter(country_exists_d ==1) %>%
  select(-country_exists_o, -country_exists_d)

# Usuwam handel państwa samo ze sobą i usuwam zbędne kolmny indeksów
gravity <- gravity %>% 
  filter(country_id_o != country_id_d) %>%
  select(-iso3num_o, -iso3num_d, -iso3_o, -iso3_d)

# Dodaję indeks dla połączenia handlowego
gravity$ID <- paste(gravity$country_id_o, gravity$country_id_d, sep="")

# Przeniesienie indeksu do pierwszej kolumny
gravity <- gravity %>% relocate(ID)

# Sprawdzam czy są duplikaty w bazie
gravity$ID_year <- paste(gravity$ID, gravity$year, sep="")

dup <- gravity %>% 
  group_by(ID_year) %>%
  filter(n()>1) %>%
  ungroup()

length(dup$year)
# Nie ma, mogę pracować dalej
rm(dup)

# Usuwam połączenia, w których gdp, odległości i inne wazne zmienne wynoszą NA lub dla których odległość lub gdp są równe 0
gravity <- gravity %>%
  drop_na(gdp_o, gdp_d, dist, distw_harmonic, contig, comlang_ethno, eu_o, eu_d) %>%
  filter(gdp_o != 0) %>%
  filter(gdp_d != 0) %>%
  filter(dist != 0) %>%
  filter(distw_harmonic !=0)

# Generowanie logarytmów zmiennych
gravity$lnGDP_o = log(gravity$gdp_o)
gravity$lnGDP_d = log(gravity$gdp_d)
gravity$lnDIST = log(gravity$dist)
gravity$lnDIST_h <- log(gravity$distw_harmonic)
gravity$lnGDP_cap_o = log(gravity$gdpcap_o)
gravity$lnGDP_cap_d = log(gravity$gdpcap_d)


# Zbilansowane bazy danych --------------------------------------------------------------------
# Tworzę zbilansowany panel (tylko te połączenia handlowe, dla których mam 21 lat)
gravity_balanced_all <- gravity %>%
  group_by(ID) %>%
  filter(n() == 21) %>%
  ungroup()

# Tylko te połączenia handlowe, które są większe niz zero (zarejestrowano jakikolwiek handel). Nadal zbilansowany panel
gravity_balanced_positive <- gravity %>%
  filter(tradeflow_baci > 0)%>%
  group_by(ID) %>%
  filter(n() == 21) %>%
  ungroup()

# Zapisanie tabel ----------------------------------------------------------------------------------------------------------------------

# Najbardziej efektywne pod względem zajmowanego miejsca jest zapisanie w pliku RDS
saveRDS(gravity_balanced_all, file = "Data/gravity_balanced_all.rds")
saveRDS(gravity_balanced_positive, file = "Data/gravity_balanced_positive.rds")

# Można też zapisać do pliku csv
#write.csv(gravity_balanced_all, file = "gravity_balanced_all.csv")
#write.csv(gravity_balanced_positive, file = "gravity_balanced_positive.csv")
