# Środowisko robocze -------------------------------------------------------------------
getwd()

# Biblioteki -------------------------------------------------------------------------------

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
