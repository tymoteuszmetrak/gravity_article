# Working directory -------------------------------------------------------------------
getwd()

# Libraries -------------------------------------------------------------------------------

library(dplyr) # for easier oparation on dataframes
library(tidyr) # for easy handling messy data
library(plm) # For Panel Linear Models
library(pglm) # For Panel Generalized Linear Models
library(stargazer) # for  quality publication tables

# Reading the data -----------------------------------------------------------------------------

# CEPII
# In the project I use the 202211 version of the database.
# It may be dowloaded from https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# Due to its huge size (over 100 MB) I could not include it in the GitHub repository.

path <-  r"(C:\Users\tymme\Dropbox\Studia\Podstawowe_modele_panelowe\Gravity_rds_V202211\Gravity_V202211.rds)"
gravity <- readRDS(path)

str(gravity)