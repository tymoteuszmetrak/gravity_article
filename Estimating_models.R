# Working directory ------------------------------------------------------------------------------------
getwd()

# Libraries --------------------------------------------------------------------------------------------
library(dplyr) # for easier operation on dataframes
library(tidyr) # for easy handling messy data
library(plm) # For Panel Linear Models
library(pglm) # For Panel Generalized Linear Models
library(texreg) # for  quality publication tables

# Reading the processed data -----------------------------------------------------------------------------
gravity_balanced_all <- readRDS("Data/gravity_balanced_all.rds")
gravity_balanced_positive <- readRDS("Data/gravity_balanced_positive.rds")

# Estimating models  --------------------------------------------------------------------------------------

# Model 1 - FE
model1 = plm(log(tradeflow_baci)~lnGDP_o+
               lnGDP_d+
               lnDIST,
             index=c("ID", "year"),
             model="within",
             data=gravity_balanced_positive)

summary(model1)

# Model 4 - FE z with additional variables
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

# Model 5 - PFE with additional variables
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

# Which countries in the datasets  --------------------------------------------------------------------------------------------------------
# It allows to check origin and destination countries
origin_countries_all <- gravity_balanced_all%>%distinct(country_id_o) %>% pull()
destination_countries_all <- gravity_balanced_all%>%distinct(country_id_d) %>% pull()

origin_countries_positive <- gravity_balanced_positive%>%distinct(country_id_o) %>% pull()
destination_countries_positive <- gravity_balanced_positive%>%distinct(country_id_d) %>% pull()

# Number of observations -------------------------------------------------------------------------------------------------------------
nobs_all <- dim(gravity_balanced_all)[1]
nobs_positive <- dim(gravity_balanced_positive)[1]

# Quality Publication Table -----------------------------------------------------------------------------------------------------

#Table 1

# Preview - see the table in the console
screenreg(list(model1, model2),
          custom.model.names = c("FE (1)","PFE (2)"),
          custom.coef.names = c("Log GDP origin", "Log GDP destination","Log distance"),
          include.nobs = F,
          custom.gof.rows = list("Number of obs"=c(nobs_positive,nobs_all)))

# Saving the table to the doc file
htmlreg(list(model1, model2),
        custom.model.names = c("FE (1)","PFE (2)"),
        custom.coef.names = c("Log GDP origin", "Log GDP destination","Log distance"),
        caption = NA,
        include.nobs = F,
        custom.gof.rows = list("Number of obs"=c(nobs_positive,nobs_all)),
        file="QPT/Table1.doc")


# Table 2

# Preview - see the table in the console
screenreg(list(model3, model4,model5),
          custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
          custom.coef.names = c("Constant","Log GDP origin", "Log GDP destination",
                                "Log GDP per capita origin","Log GDP per capita destination",
                                "Log distance","Origin state EU member","Destination state EU member","Common border",
                                "Common language","common colonizer","Colonial ties after 1945","Common source of legal system",
                                rep(NA,7)),
          include.nobs = F,
          custom.gof.rows = list("Number of obs"=c(nobs_positive,nobs_positive,nobs_all)))


# Saving the table to the doc file
htmlreg(list(model3, model4,model5),
        custom.model.names = c("CRE (3)","FE (4)","PFE (5)"),
        custom.coef.names = c("Constant","Log GDP origin", "Log GDP destination",
                              "Log GDP per capita origin","Log GDP per capita destination",
                              "Log distance","Origin state EU member","Destination state EU member","Common border",
                              "Common language","common colonizer","Colonial ties after 1945","Common source of legal system",
                              rep(NA,7)),
        caption = NA,
        include.nobs = F,
        custom.gof.rows = list("Number of obs"=c(nobs_positive,nobs_positive,nobs_all)),
        file = "QPT/Table2.doc")
