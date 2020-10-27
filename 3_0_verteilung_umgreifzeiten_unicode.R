#### Umgreifen raw to fm ####

# Autor: Fabian Böttcher
# Datum: 13.11.2019

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

#### Fläche wählen ####
flaeche <- "ls_2"
## Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 ##

#### Daten einlesen ####
sqlStatement1 <- paste0("SELECT * FROM ",flaeche,"_stk")
sqlStatement2 <- paste0("SELECT * FROM ",flaeche,"_baum")
stk <- dbGetQuery(con, sqlStatement1)
baum <- dbGetQuery(con, sqlStatement2)

## schreiben der Anzahl Umgreifen ##
stk$Umgreifen_Anzahl <- stk %>% 
  select(z.1_raw:z.15_raw) %>%
  apply(1, function(x) sum(!is.na(x)))

#### Z-Zeiten, NAs zu 0en, rausgefiltert die Datensätze bei denen ich keine Fix-Zeiten habe ####
# stk2 <- stk %>% 
#   mutate(Fehlstueck = ifelse(is.na(Fehlstueck), 0, Fehlstueck)) %>% 
#   filter(Fehlstueck !=2) %>% 
#   mutate_at(vars(contains("raw")), ~replace(., is.na(.), 0))

#### Z-Zeiten, NAs zu 0en, rausgefiltert die Datensätze bei denen ich keine Fix-Zeiten habe ####
stk2 <- stk %>%
  mutate(Fehlstueck = ifelse(is.na(Fehlstueck), 0, Fehlstueck)) %>%
  mutate_at(vars(contains("raw")), ~replace(., is.na(.), 0))


#### Datentransformation ####
stk3 <- stk2[0, ]

for(i in unique(stk2$Baum_Nr)){
  tempLoop <- stk2 %>% filter(Baum_Nr == i) %>% mutate(
    z.1 = Volumen/sum(filter(stk2, Baum_Nr == i, z.1_raw != 0) %>% pull(Volumen))*z.1_raw,
    z.2 = Volumen/sum(filter(stk2, Baum_Nr == i, z.2_raw != 0) %>% pull(Volumen))*z.2_raw,
    z.3 = Volumen/sum(filter(stk2, Baum_Nr == i, z.3_raw != 0) %>% pull(Volumen))*z.3_raw,
    z.4 = Volumen/sum(filter(stk2, Baum_Nr == i, z.4_raw != 0) %>% pull(Volumen))*z.4_raw,
    z.5 = Volumen/sum(filter(stk2, Baum_Nr == i, z.5_raw != 0) %>% pull(Volumen))*z.5_raw,
    z.6 = Volumen/sum(filter(stk2, Baum_Nr == i, z.6_raw != 0) %>% pull(Volumen))*z.6_raw,
    z.7 = Volumen/sum(filter(stk2, Baum_Nr == i, z.7_raw != 0) %>% pull(Volumen))*z.7_raw,
    z.8 = Volumen/sum(filter(stk2, Baum_Nr == i, z.8_raw != 0) %>% pull(Volumen))*z.8_raw,
    z.9 = Volumen/sum(filter(stk2, Baum_Nr == i, z.9_raw != 0) %>% pull(Volumen))*z.9_raw,
    z.10 = Volumen/sum(filter(stk2, Baum_Nr == i, z.10_raw != 0) %>% pull(Volumen))*z.10_raw,
    z.11 = Volumen/sum(filter(stk2, Baum_Nr == i, z.11_raw != 0) %>% pull(Volumen))*z.11_raw,
    z.12 = Volumen/sum(filter(stk2, Baum_Nr == i, z.12_raw != 0) %>% pull(Volumen))*z.12_raw,
    z.13 = Volumen/sum(filter(stk2, Baum_Nr == i, z.13_raw != 0) %>% pull(Volumen))*z.13_raw,
    z.14 = Volumen/sum(filter(stk2, Baum_Nr == i, z.14_raw != 0) %>% pull(Volumen))*z.14_raw,
    z.15 = Volumen/sum(filter(stk2, Baum_Nr == i, z.15_raw != 0) %>% pull(Volumen))*z.15_raw)
  
  stk3 <- bind_rows(stk3, tempLoop)
} 

## NA zu 0en und Datensatz zurück zu stk ##
stk2 <- stk3 %>% 
  mutate_at(vars("z.1":"z.15"), ~replace(., is.na(.), 0))

## schreiben der Summenspalte für Umgreifen ##
stk <- stk2 %>% 
  mutate(Zeit_U = z.1+z.2+z.3+z.4+z.5+z.6+z.7+z.8+z.9+z.10+z.11+z.12+z.13+z.14+z.15)

## schreiben fix.fm.z und fix.fm.g ##
stk <- stk %>% 
  mutate(fix.fm.z = Zeit_U/Volumen) %>% 
  mutate(fix.fm.g = fix.fm + fix.fm.z)

## schreiben der Anzahl und der Gesamtzeit Umgreifen an Baumdatensatz
l2 <- stk %>%
  select(Baum_Nr,z.1:z.15) %>% 
  group_by(Baum_Nr) %>% 
  summarise_each(funs(sum(., na.rm = T)))

l2$count <- l2 %>% 
  mutate_all(~replace(., . == 0, NA)) %>%
  select(z.1:z.15) %>% 
  apply(1, function(x) sum(!is.na(x)))

l2$Zeit_U <- rowSums(l2[,2:16])
l2 <- l2[,c(1,17:18)]

baum2 <- merge(baum, l2, by.x = "Baum_Nr", by.y = "Baum_Nr", all.x = TRUE)
baum <- baum2 %>% 
  mutate(Umgreifen = Zeit_U) %>% 
  rename(Umgreifen_Anzahl = count)
baum <- baum[,c(1:32)]

#### Speichern ####
sqlStatement3 <- paste0(flaeche,"_stk")
sqlStatement4 <- paste0(flaeche,"_baum")
dbWriteTable(con, name = sqlStatement3, stk, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement4, baum, overwrite = TRUE)
rm(list = ls())

