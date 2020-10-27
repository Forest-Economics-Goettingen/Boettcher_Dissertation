#### Datenaufbereitung ls_1 ####

# Autor: Fabian Böttcher
# Datum: 13.11.2019

#### Pakete laden ####
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
flaeche <- "he_1"
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

#### Ausgleich der restlichen Kovariablen bis auf die Zeiten ####

## Stamm_Krone ##
stk %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1)) %>% 
  ggplot(aes(Abschnitt, Stamm_Krone)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "gam", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit")

logit.sk <- ges %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1))
logit.sk1 <- glm(Stamm_Krone ~ Abschnitt, family = binomial(link = "logit"), data = logit.sk)
logit.sk2 <- gam(Stamm_Krone ~ s(Abschnitt), family = binomial(link = "logit"), data = logit.sk)
summary(logit.sk2)
hist(resid(logit.sk1, type = "response"))
hist(resid(logit.sk2, type = "response")) # besser

logit.sk <- datenFuellen(dat = logit.sk, modell = logit.sk2, logit = TRUE)
ges <- logit.sk %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == 0, "S", "K"))

## Verlauf, Zeiten sind unbekannt ##
logit.ver <- ges %>% 
  mutate(Sortiment = ifelse(Sortiment == "IS", 0, 1)) %>% 
  mutate(Verlauf = ifelse(Verlauf == "N", 0, 1)) %>% 
  mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1))
logit.ver1 <- glm(Verlauf ~ Abschnitt, family = binomial(link = "logit"), data = logit.ver)
logit.ver2 <- gam(Verlauf ~ s(BHD_cm) + Abschnitt, family = binomial(link = "logit"), data = logit.ver)

summary(logit.ver2)
hist(resid(logit.ver1, type = "response"))
hist(resid(logit.ver2, type = "response"))

ges <- datenFuellen(dat = logit.ver, modell = logit.ver2, logit = TRUE)
ges <- ges %>% 
  mutate(Sortiment = ifelse(Sortiment == 0, "IS", "PA")) %>% 
  mutate(Verlauf = ifelse(Verlauf == 0, "N", "J")) %>% 
  mutate(Stamm_Krone = ifelse(Stamm_Krone == 0, "S", "K"))

#### Längenausgleich ####

## Länge der Sortimente anschauen ##
boxplot(ges$Laenge_m ~ ges$Sortiment)

## IS über mean##
mean.is <- mean(ges$Laenge_m[ges$Sortiment == "IS"], na.rm = T)

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "IS" & is.na(Laenge_m),
                                       mean.is,
                                       Laenge_m))

ges <- ges %>% mutate(Laenge_cm = ifelse(Sortiment == "IS" & is.na(Laenge_cm),
                                        mean.is*100,
                                        Laenge_cm))

## PA über mean ##
mean.pa <- mean(ges$Laenge_m[ges$Sortiment == "PA"], na.rm = T)

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "PA" & is.na(Laenge_m),
                                        mean.pa,
                                        Laenge_m))

ges <- ges %>% mutate(Laenge_cm = ifelse(Sortiment == "PA" & is.na(Laenge_cm),
                                         mean.pa*100,
                                         Laenge_cm))

#### auffüllen der Durchmesserwerte , auf Wechselwirkungen getestet - Verlauf auf Zeit integriert ####
lm.2 <- lm(DM_cm ~ Sortiment + Stamm_Krone + Abschnitt + Verlauf, data = ges)
step.lm.2 <- stepAIC(lm.2, direction = "backward", trace = TRUE)

gam.2 <- gam(DM_cm ~ Sortiment + Stamm_Krone + s(Abschnitt) + Verlauf + s(BHD_cm), data = ges)
hist(resid(gam.2, type = "response")) # Nimm das GAM man
hist(resid(step.lm.2))

## Ausfüllen dm_cm ##
ges2 <- datenFuellen(dat = ges, modell = gam.2, logit = FALSE)

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

