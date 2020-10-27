#### Datenaufbereitung ls_1 ####

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
con.sql <- dbConnect(RSQLite::SQLite(), "./data_gerlau.db")
dbListTables(con.sql)

#### Hilffunktionen laden ####
source("0_1_Hilfsfunktionen.R")

#### Daten einlesen ####
#### Fläche wählen ####
flaeche <- "ls_2"
## Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 ##

#### Daten einlesen ####
sqlStatement1 <- paste0("SELECT * FROM ",flaeche,"_stk")
sqlStatement2 <- paste0("SELECT * FROM ",flaeche,"_baum")
stk_raw <- dbGetQuery(con.sql, sqlStatement1)
baum_raw <- dbGetQuery(con.sql, sqlStatement2)

## volumen zum plotten berechnen ##
stk <- stk_raw %>% 
  mutate(Volumen = ((pi/4)* (DM_m^2)) * Laenge_m )
baum <- baum_raw

## stk-Datensatz mit Baumvaribalen schreiben ## 
ges <- inner_join(x = stk, y = baum[,c(1,5:31)], by = "Baum_Nr")
colSums(is.na(ges))

#### Längenausgleich ####

## Länge der Sortimente anschauen ##
boxplot(ges$Laenge_m ~ ges$Sortiment)

## IS über mean / Median ist 3.1, mean ist 3,06 ##
mean.is <- mean(ges$Laenge_m[ges$Sortiment == "IS"], na.rm = T)

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "IS" & is.na(Laenge_m),
                                       mean.is,
                                       Laenge_m))

ges <- ges %>% mutate(Laenge_cm = ifelse(Sortiment == "IS" & is.na(Laenge_cm),
                                        mean.is*100,
                                        Laenge_cm))

## SH über BHD des eigentlichen Baumes 
sum(is.na(ges$Laenge_cm[ges$Sortiment == "SH"]))
# -> hier nicht notwendig da nur IS fehlt ## 
# filt <- ges[which(ges$Sortiment == "SH"),]
# lm.1 <- lm(filt$Laenge_m ~ filt$BHD_cm)
# lm.1.coef <- coef(lm.1[1])
# summary(lm.1)
# 
# ges <- ges %>% 
#   mutate(Laenge_cm = ifelse(Sortiment == "SH" & is.na(Laenge_cm),
#                             lm.1.coef[1]+lm.coef[2]*BHD_cm,
#                             Laenge_cm))

#### auffüllen der Durchmesserwerte , auf Wechselwirkungen getestet - Verlauf auf Zeit integriert ####
lm.2 <- lm(DM_cm ~ Sortiment + Stamm_Krone + Abschnitt + Verlauf, data = ges)
step.lm.2 <- stepAIC(lm.2, direction = "backward", trace = TRUE)

gam.2 <- gam(DM_cm ~ Sortiment + Stamm_Krone + s(Abschnitt) + Verlauf, data = ges)
hist(resid(gam.2, type = "response")) # Nimm das GAM man
hist(resid(step.lm.2))

## Ausfüllen dm_cm ##
ges2 <- datenFuellen(dat = ges, modell = gam.2)

## Auffüllen der dm_m - Werte ##
ges <- ges2 %>% 
  mutate(DM_m = ifelse(is.na(DM_m),
                            DM_cm/100,
                            DM_m))

#### Volumen überschreiben ####
ges <- ges %>% 
  mutate(Volumen = ((pi/4)* (DM_m^2)) * Laenge_m )

#### Datensatz trennen ####
stk <- ges[,c(1:58)]
stk <- stk %>% 
  rename(Bemerkungen = Bemerkungen.x)

## schreiben von fix.fm / Anzahl gründe ##
stk <- stk %>% 
  mutate(fix.fm = Zeit_N/Volumen)

stk$Grund_Anzahl <- stk %>% 
  select(Grund_A:Grund_Z) %>%
  apply(1, function(x) sum(!is.na(x)))

#### schreiben des Baumvolumens ####
## Baumvolumen über Funktion / Buche Derbholzfunktion nach Bergel ##
baum$Volumen_func <- bergel(h = baum$Hoehe_m, d = baum$BHD_cm)

## Baumvolumen über Summe der Abschnitte ##
l1 <- aggregate(Volumen ~ Baum_Nr, data = stk, FUN = sum)
baum <- merge(baum, l1, by.x = "Baum_Nr", by.y = "Baum_Nr", all.x = TRUE)
baum <- baum %>% mutate(Volumen_stk = Volumen)
baum <- baum[,c(1:31)]

#### Datensatz speichern ####
con <- dbConnect(RSQLite::SQLite(), "data_gerlau_new.db")
sqlStatement3 <- paste0(flaeche,"_stk")
sqlStatement4 <- paste0(flaeche,"_baum")
dbWriteTable(con, name = sqlStatement3, stk, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement4, baum, overwrite = TRUE)
rm(list = ls())
