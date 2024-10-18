# Środowisko robocze --------------------------------------------------------------------------
getwd()

# Biblioteki ----------------------------------------------------------------------------------

library(dplyr) # łatwiejsze operacje na ramkach danych
library(tidyr) # do łatwiejszych operacji na danych
library(plm) # do modeli panelowych
library(pglm) # do uogólnionych modeli panelowych
library(stargazer) # Do wyświetlania wyników

# Czytanie danych -----------------------------------------------------------------------------

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

# Usuwam połączenia, w których gdp lub odległości wynoszą NA lub dla których odległość lub gdp są równe 0
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
write.csv(gravity_balanced_all, file = "gravity_balanced_all.csv")
write.csv(gravity_balanced_positive, file = "gravity_balanced_positive.csv")


# !!!!!!!!!!!!!!!!!!!!!!!! OECD???????!!!!!!
# Zostawiam tylko członków OECD
remotes::install_github("caldwellst/whotilities")

# Modele FE ------------------------------------------------------------------------------------
model1 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model1)

model2 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST+
               contig+
               eu_o+
               eu_d+
               comlang_ethno,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model2)

model3 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST_h+
               eu_o+
               eu_d+
               comlang_ethno,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model3)

model4 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST_h+
               contig+
               eu_o+
               eu_d+
               comlang_ethno+
               fta_wto,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model4)

model5 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnGDP_cap_o+
               lnGDP_cap_d+
               lnDIST+
               contig+
               eu_o+
               eu_d+
               comlang_ethno,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model5)

# Modele CRE ---------------------------------------------------------------------------------
modelc1 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnDIST+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnDIST),
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc1)

modelc2 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnDIST+
                contig+
                eu_o+
                eu_d+
                comlang_ethno+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnDIST)+
                Between(eu_o)+
                Between(eu_d),
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc2)

modelc3 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnDIST_h+
                contig+
                eu_o+
                eu_d+
                comlang_ethno+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnDIST_h)+
                Between(eu_o)+
                Between(eu_d),
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc3)

modelc4 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnDIST_h+
                contig+
                eu_o+
                eu_d+
                comlang_ethno+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnDIST_h)+
                Between(eu_o)+
                Between(eu_d)+
                comcol,
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc4)

modelc5 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnGDP_cap_o+
                lnGDP_cap_d+
                lnDIST+
                eu_o+
                eu_d+
                contig+
                comlang_ethno+
                comcol+
                col45+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnGDP_cap_o)+
                Between(lnGDP_cap_d)+
                Between(lnDIST)+
                Between(eu_o)+
                Between(eu_d),
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc5)

modelc6 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnGDP_cap_o+
                lnGDP_cap_d+
                lnDIST+
                eu_o+
                eu_d+
                contig+
                comlang_ethno+
                comcol+
                col45+
                comleg_posttrans+
                Between(lnGDP_o)+
                Between(lnGDP_d)+
                Between(lnGDP_cap_o)+
                Between(lnGDP_cap_d)+
                Between(lnDIST)+
                Between(eu_o)+
                Between(eu_d),
              index=c("ID", "year"),
              model="random",
              data=gravity_balanced_positive)

summary(modelc6)

# Modele PFE ----------------------------------------------------------------------------------
modelp1 = pglm(tradeflow_baci~lnGDP_o+
                 lnGDP_d+
                 lnDIST,
               family=poisson,
               index=c("ID", "year"),
               model="within",
               data=gravity_balanced_positive)

summary(modelp1)

modelp1_all = pglm(tradeflow_baci~lnGDP_o+
                     lnGDP_d+
                     lnDIST,
                   family=poisson,
                   index=c("ID", "year"),
                   model="within",
                   data=gravity_balanced_all)

summary(modelp1_all)

# Model p2 nie działa! Do modelu Poissona nie można dodać zmiennych stałych w czasie, bo wtedy wariuje!
modelp2 = pglm(tradeflow_baci~lnGDP_o+
                 lnGDP_d+
                 lnDIST+
                 contig+
                 eu_o+
                 eu_d+
                 comlang_ethno,
               family=poisson,
               index=c("ID", "year"),
               model="within",
               data=gravity_balanced_all)

summary(modelp2)

modelp4 = pglm(tradeflow_baci~lnGDP_o+
                 lnGDP_d+
                 lnDIST+
                 eu_o+
                 eu_d,
               family=poisson,
               index=c("ID", "year"),
               model="within",
               data=gravity_balanced_all)

summary(modelp4)

modelp5 = pglm(tradeflow_baci~lnGDP_o+
                 lnGDP_d+
                 lnGDP_cap_o+
                 lnGDP_cap_d+
                 lnDIST+
                 eu_o+
                 eu_d,
               family=poisson,
               index=c("ID", "year"),
               model="within",
               data=gravity_balanced_all)

summary(modelp5)

#Model Between
modelb5 = plm(log(tradeflow_baci)~lnGDP_o+
                lnGDP_d+
                lnGDP_cap_o+
                lnGDP_cap_d+
                lnDIST+
                eu_o+
                eu_d-1,
              index=c("ID", "year"),
              model="between",
              data=gravity_balanced_positive)

summary(modelb5)

# Jakie państwa w bazie -------------------------------------------------------------------------------------------------
origin_countries <- gravity_balanced_all%>%distinct(country_id_o) %>% pull()
destination_countries <- gravity_balanced_all%>%distinct(country_id_d) %>% pull()


# Quality Publication Table -----------------------------------------------------------------------------------------------------
stargazer(model3, modelc3,
          title = "Modele FE i CRE",
          column.labels = c("FE","CRE"),
          dep.var.labels = "Logarytm przepływów handlowych",
          type="text")

stargazer(model1, modelp1,
          type="text")
texreg::htmlreg(list(model1, modelp1),
                custom.model.names = c("FE (1)","PFE (2)"),
                custom.coef.names = c("Log. PKB eksportera", "Log. PKB importera","Log odległości"),
                caption = "Tabela 1",
                include.nobs = F,
                custom.gof.rows = list("Liczba obserwacji"=c(297906,596232)),
                file="compare1.html")

texreg::screenreg(list(model1, modelp1),
                  custom.model.names = c("FE (1)","PFE (2)"),
                  custom.coef.names = c("Log. PKB eksportera", "Log. PKB importera","Log odległości"),
                  caption = "Tabela 1",
                  include.nobs = F,
                  custom.gof.rows = list("Liczba obserwacji"=c(297906,596232)))

texreg::htmlreg(list(modelc5, model5,modelp5),
                custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
                custom.coef.names = c("Stała","Log. PKB eksportera", "Log. PKB importera",
                                      "Log. PKB per capita eksportera","Log. PKB per capita importera",
                                      "Log. odległości","Eksporter członkiem UE","Importer członkiem UE","Wspólna granica",
                                      "Wspólny język","Wspólny kolonizator","Powiązania kolonialne po 1945",
                                      rep(NA,7)),
                caption = "Tabela 2",
                include.nobs = F,
                custom.gof.rows = list("Liczba obserwacji"=c(297906,297906,596232)),
                file = "Tabela2.html")

texreg::htmlreg(list(modelc6, model5,modelp5),
                custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
                custom.coef.names = c("Stała","Log. PKB eksportera", "Log. PKB importera",
                                      "Log. PKB per capita eksportera","Log. PKB per capita importera",
                                      "Log. odległości","Eksporter członkiem UE","Importer członkiem UE","Wspólna granica",
                                      "Wspólny język","Wspólny kolonizator","Powiązania kolonialne po 1945","Wspólne źródła systemu prawnego",
                                      rep(NA,7)),
                caption = "Tabela 2",
                include.nobs = F,
                custom.gof.rows = list("Liczba obserwacji"=c(297906,297906,596232)),
                file = "Tabela2.html")

library(xtable)


