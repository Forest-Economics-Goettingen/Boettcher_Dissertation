#### Gesamtdatensatz schreiben ####

# Autor: Fabian Böttcher
# Datum: 17.01.2020

#### Pakete laden ####
library(MASS)
library(sqliter)
library(DBI)
library(tidyverse)
library(cowplot)
library(readxl)
library(corrplot)
library(mgcv)

#### Datenbank einbinden ####
con <- dbConnect(RSQLite::SQLite(), "./data_gerlau_new.db")

#### Daten einlesen ####
stk_ls1 <- dbGetQuery(con, "SELECT * FROM ls_1_stk")
baum_ls1 <- dbGetQuery(con, "SELECT * FROM ls_1_baum")
stk_ls2 <- dbGetQuery(con, "SELECT * FROM ls_2_stk")
baum_ls2 <- dbGetQuery(con, "SELECT * FROM ls_2_baum")
stk_he1 <- dbGetQuery(con, "SELECT * FROM he_1_stk")
baum_he1 <- dbGetQuery(con, "SELECT * FROM he_1_baum")
stk_he2 <- dbGetQuery(con, "SELECT * FROM he_2_stk_w")
baum_he2 <- dbGetQuery(con, "SELECT * FROM he_2_baum_w")
stk_he3 <- dbGetQuery(con, "SELECT * FROM he_3_stk")
baum_he3 <- dbGetQuery(con, "SELECT * FROM he_3_baum")

#### Gesamtdatensatz schreiben
baum_ges <- do.call("rbind", list(baum_ls1, baum_ls2, baum_he1, baum_he2, baum_he3))
stk_ges <- do.call("rbind", list(stk_ls1, stk_ls2, stk_he1, stk_he2, stk_he3))

#### Fahrervariable schreiben
baum_ges <- baum_ges %>% 
  mutate(Fahrer = ifelse(Flaeche %in% c("ls_1", "ls_2"), 1,
         ifelse(Flaeche == "he_1", 2,
                ifelse(Flaeche == "he_2", 3,
                       4))))

stk_ges <- stk_ges %>% 
  mutate(Fahrer = ifelse(Flaeche %in% c("ls_1", "ls_2"), 1,
                         ifelse(Flaeche == "he_1", 2,
                                ifelse(Flaeche == "he_2", 3,
                                       4))))

# abspeichern
dbWriteTable(con, name = "baum_ges", baum_ges, overwrite = TRUE)
dbWriteTable(con, name = "stk_ges", stk_ges, overwrite = TRUE)
rm(list = ls())



