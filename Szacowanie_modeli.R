# Środowisko robocze --------------------------------------------------------------------------
getwd()

# Biblioteki ----------------------------------------------------------------------------------
library(dplyr) # łatwiejsze operacje na ramkach danych
library(tidyr) # do łatwiejszych operacji na danych

# Czytanie danych przetworzonych --------------------------------------------------------------
gravity_balanced_all <- readRDS("Data/gravity_balanced_all.rds")
gravity_balanced_positive <- readRDS("Data/gravity_balanced_positive.rds")


# Szacowanie modeli  -----------------------------------------------------------------------------------------------------------------

# Model 1 - FE
model1 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model1)

# Model 4 - FE z dodatkowymi zmiennymi
model4 = plm(log(tradeflow_baci)~lnGDP_o+
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

summary(model4)


# Model 3 - CRE
model3 = plm(log(tradeflow_baci)~lnGDP_o+
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

summary(model3)

# Model 2 - PFE
model2 = pglm(tradeflow_baci~lnGDP_o+
                lnGDP_d+
                lnDIST,
              family=poisson,
              index=c("ID", "year"),
              model="within",
              data=gravity_balanced_positive)

summary(model2)

# Model 5 - PFE z dodatkowymi zmiennymi
model5 = pglm(tradeflow_baci~lnGDP_o+
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

summary(model5)


# Jakie państwa w bazie --------------------------------------------------------------------------------------------------------
# Pozwala na sprawdzenie państw importerów i eksporterów
origin_countries_all <- gravity_balanced_all%>%distinct(country_id_o) %>% pull()
destination_countries_all <- gravity_balanced_all%>%distinct(country_id_d) %>% pull()

origin_countries_positive <- gravity_balanced_positive%>%distinct(country_id_o) %>% pull()
destination_countries_positive <- gravity_balanced_positive%>%distinct(country_id_d) %>% pull()

# Liczba obserwacji -------------------------------------------------------------------------------------------------------------
nobs_all <- dim(gravity_balanced_all)[1]
nobs_positive <- dim(gravity_balanced_positive)[1]

# Quality Publication Table -----------------------------------------------------------------------------------------------------

#Tabela 1

# Podgląd - wyświetlenie tabeli w konsoli
screenreg(list(model1, model2),
          custom.model.names = c("FE (1)","PFE (2)"),
          custom.coef.names = c("Log. PKB eksportera", "Log. PKB importera","Log odległości"),
          include.nobs = F,
          custom.gof.rows = list("Liczba obserwacji"=c(nobs_positive,nobs_all)))

# Zapisanie tabeli do pliku doc
htmlreg(list(model1, model2),
        custom.model.names = c("FE (1)","PFE (2)"),
        custom.coef.names = c("Log. PKB eksportera", "Log. PKB importera","Log odległości"),
        caption = NA,
        include.nobs = F,
        custom.gof.rows = list("Liczba obserwacji"=c(nobs_positive,nobs_all)),
        file="Tabele/Tabela1.doc")


# Tabela 2

# Podgląd - wyświetlenie tabeli w konsoli
screenreg(list(model3, model4,model5),
          custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
          custom.coef.names = c("Stała","Log. PKB eksportera", "Log. PKB importera",
                                "Log. PKB per capita eksportera","Log. PKB per capita importera",
                                "Log. odległości","Eksporter członkiem UE","Importer członkiem UE","Wspólna granica",
                                "Wspólny język","Wspólny kolonizator","Powiązania kolonialne po 1945","Wspólne źródła systemu prawnego",
                                rep(NA,7)),
          include.nobs = F,
          custom.gof.rows = list("Liczba obserwacji"=c(nobs_positive,nobs_positive,nobs_all)))


# Zapisanie tabeli do pliku doc
htmlreg(list(model3, model4,model5),
        custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
        custom.coef.names = c("Stała","Log. PKB eksportera", "Log. PKB importera",
                              "Log. PKB per capita eksportera","Log. PKB per capita importera",
                              "Log. odległości","Eksporter członkiem UE","Importer członkiem UE","Wspólna granica",
                              "Wspólny język","Wspólny kolonizator","Powiązania kolonialne po 1945","Wspólne źródła systemu prawnego",
                              rep(NA,7)),
        caption = NA,
        include.nobs = F,
        custom.gof.rows = list("Liczba obserwacji"=c(nobs_positive,nobs_positive,nobs_all)),
        file = "Tabele/Tabela2.doc")

