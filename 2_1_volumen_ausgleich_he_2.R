#### Datenaufbereitung he_2 ####

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
flaeche <- "he_2"
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

## wegen Fehlern in der Datenaufnahme werden die Datensätze gelöscht,
## die keine Zeiten aufweisen und bei denen nichts zu den Abschnitten
## bekannt ist
colSums(is.na(baum))

list_0 <- stk %>% 
  select(Baum_Nr) %>% 
  unique()
list_1 <- baum %>% 
  select(Baum_Nr) %>% 
  unique()
list_2 <- anti_join(list_1, list_0)
list_3 <- stk %>% 
  filter(is.na(Zeit_N) | is.na(DM_cm)) %>%
  select(Baum_Nr) %>% 
  unique()
list_4 <- baum %>% 
  filter(is.na(Greifen) & is.na(Faellung) & is.na(Kronenrest) | is.na(BHD_cm)) %>% 
  select(Baum_Nr)
list_5 <- baum %>% 
  filter(is.na(BHD_cm)) %>% 
  select(Baum_Nr)

# weiche Variante | die Bäume bei denen keine Abschnittsdaten bekannt sind 
# plus die Bäume von denen keine stand-Informationen existieren (BHD, Höhe)
# fliegen raus
list_6 <- full_join(list_2, list_5)
stk1 <- stk %>% filter(!Baum_Nr %in% list_6$Baum_Nr)
baum1 <- baum %>% filter(!Baum_Nr %in% list_6$Baum_Nr)

# harte Variante | die Bäume bei denen keine Zeiten/BHD existieren und
# keine Abschnittsdaten existieren fliegen raus
list_7 <- full_join(list_4, list_2)
stk2 <- stk %>% filter(!Baum_Nr %in% list_7$Baum_Nr)
baum2 <- baum %>% filter(!Baum_Nr %in% list_7$Baum_Nr)

#-------------------------------------#
#### Datensatz 1 - weiche Variante ####
#-------------------------------------#

## stk-Datensatz mit Baumvaribalen schreiben ## 
ges <- inner_join(x = stk1, y = baum1[,c(1,5:31)], by = "Baum_Nr")
#ges2 <- inner_join(x = stk2, y = baum2[,c(1,5:31)], by = "Baum_Nr")

#### Ausgleich der restlichen Kovariablen bis auf die Zeiten ####

## Stamm_Krone ##
stk1 %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1)) %>% 
  ggplot(aes(Abschnitt, Stamm_Krone)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit")

logit.sk <- ges %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1))
logit.sk1 <- glm(Stamm_Krone ~ Abschnitt, family = binomial(link = "logit"), data = logit.sk)
logit.sk2 <- gam(Stamm_Krone ~ s(Abschnitt), family = binomial(link = "logit"), data = logit.sk)
summary(logit.sk1)
hist(resid(logit.sk1, type = "response"))
hist(resid(logit.sk2, type = "response")) # egal -> spline wird zur geraden

logit.sk <- datenFuellen(dat = logit.sk, modell = logit.sk2, logit = TRUE)
ges <- logit.sk %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == 0, "S", "K"))

## Verlauf, Zeiten sind unbekannt ##
logit.ver <- ges %>% 
  mutate(Verlauf = ifelse(Verlauf == "N", 0, 1))
logit.ver1 <- glm(Verlauf ~ Abschnitt + BHD_cm, family = binomial(link = "logit"), data = logit.ver)
logit.ver2 <- gam(Verlauf ~ s(BHD_cm) + s(Abschnitt), family = binomial(link = "logit"), data = logit.ver)

summary(logit.ver2)
hist(resid(logit.ver1, type = "response"))
hist(resid(logit.ver2, type = "response"))

ges <- datenFuellen(dat = logit.ver, modell = logit.ver2, logit = TRUE)
ges <- ges %>% 
  mutate(Verlauf = ifelse(Verlauf == 0, "N", "J"))

#### Längenausgleich ####

## Länge der Sortimente anschauen ##

#--> auf Skriptbasis 1_x verbleiben alle Abschnitte
# ab Skript 4_x werden die Daten abgefiltert (keine Soprtimentszeit und keine Baumzeit)
boxplot(ges$Laenge_cm ~ ges$Sortiment)
sum(is.na(ges$DM_cm))
sum(is.na(ges$Laenge_m))

## IS über mean ##
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

## S über mean ##
mean.s <- mean(ges$Laenge_m[ges$Sortiment == "S"], na.rm = T)

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "S" & is.na(Laenge_m),
                                        mean.s,
                                        Laenge_m))

ges <- ges %>% mutate(Laenge_cm = ifelse(Sortiment == "S" & is.na(Laenge_cm),
                                         mean.s*100,
                                         Laenge_cm))

## SH über BHD des eigentlichen Baumes -> hier nicht notwendig da nur IS fehlt ## 
lm.1 <- lm(Laenge_cm ~ BHD_cm, data = ges[ges$Sortiment == "SH",])
lm.1.coef <- coef(lm.1[1])
summary(lm.1)

ges <- ges %>%
  mutate(Laenge_cm = ifelse(Sortiment == "SH" & is.na(Laenge_cm),
                            lm.1.coef[1]+lm.1.coef[2]*BHD_cm,
                            Laenge_cm))

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "SH" & is.na(Laenge_m),
                                         Laenge_cm/100,
                                         Laenge_m))

#### auffüllen der Durchmesserwerte , auf Wechselwirkungen getestet - Verlauf auf Zeit integriert ####
lm.2 <- lm(DM_cm ~ Sortiment + Stamm_Krone + Abschnitt + Verlauf, data = ges)
step.lm.2 <- stepAIC(lm.2, direction = "backward", trace = TRUE)

gam.2 <- gam(DM_cm ~ Sortiment + Stamm_Krone + s(Abschnitt) + Verlauf + s(BHD_cm), data = ges)
summary(gam.2)
hist(resid(gam.2, type = "response")) # Nimm das GAM man
hist(resid(step.lm.2))

## Ausfüllen dm_cm ##
ges <- datenFuellen(dat = ges, modell = gam.2)
colSums(is.na(ges))

## Auffüllen der dm_m - Werte ##
ges <- ges %>% 
  mutate(DM_m = ifelse(is.na(DM_m),
                            DM_cm/100,
                            DM_m))

boxplot(ges$DM_cm ~ ges$Sortiment)

#### Volumen überschreiben ####
ges <- ges %>% 
  mutate(Volumen = ((pi/4)* (DM_m^2)) * Laenge_m )

#### Datensatz trennen ####
stk1 <- ges[,c(1:58)]
stk1 <- stk1 %>% 
  rename(Bemerkungen = Bemerkungen.x)

## schreiben von fix.fm / Anzahl gründe ##
stk1 <- stk1 %>% 
  mutate(fix.fm = Zeit_N/Volumen)

stk1$Grund_Anzahl <- stk1 %>% 
  select(Grund_A:Grund_Z) %>%
  apply(1, function(x) sum(!is.na(x)))

#### schreiben des Baumvolumens ####
## Baumvolumen über Funktion / Buche Derbholzfunktion nach Bergel ##
baum1$Volumen_func <- bergel(h = baum1$Hoehe_m, d = baum1$BHD_cm)

## Baumvolumen über Summe der Abschnitte ##
l1 <- aggregate(Volumen ~ Baum_Nr, data = stk1, FUN = sum)
baum1 <- merge(baum1, l1, by.x = "Baum_Nr", by.y = "Baum_Nr", all.x = TRUE)
baum1 <- baum1 %>% mutate(Volumen_stk = Volumen)
baum1 <- baum1[,c(1:31)]

#-------------------------------------#
#### Datensatz 2 - harte Variante ####
#-------------------------------------#

## stk-Datensatz mit Baumvaribalen schreiben ## 
ges <- inner_join(x = stk2, y = baum2[,c(1,5:31)], by = "Baum_Nr")

#### Ausgleich der restlichen Kovariablen bis auf die Zeiten ####

## Stamm_Krone ##
stk2 %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1)) %>% 
  ggplot(aes(Abschnitt, Stamm_Krone)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit")

logit.sk <- ges %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == "S", 0, 1))
logit.sk1 <- glm(Stamm_Krone ~ Abschnitt, family = binomial(link = "logit"), data = logit.sk)
logit.sk2 <- gam(Stamm_Krone ~ s(Abschnitt), family = binomial(link = "logit"), data = logit.sk)
summary(logit.sk1)
hist(resid(logit.sk1, type = "response"))
hist(resid(logit.sk2, type = "response")) # egal -> spline wird zur geraden

logit.sk <- datenFuellen(dat = logit.sk, modell = logit.sk2, logit = TRUE)
ges <- logit.sk %>% mutate(Stamm_Krone = ifelse(Stamm_Krone == 0, "S", "K"))

## Verlauf, Zeiten sind unbekannt ##
logit.ver <- ges %>% 
  mutate(Verlauf = ifelse(Verlauf == "N", 0, 1))
logit.ver1 <- glm(Verlauf ~ Abschnitt + BHD_cm, family = binomial(link = "logit"), data = logit.ver)
logit.ver2 <- gam(Verlauf ~ s(BHD_cm) + s(Abschnitt), family = binomial(link = "logit"), data = logit.ver)

summary(logit.ver2)
hist(resid(logit.ver1, type = "response"))
hist(resid(logit.ver2, type = "response"))

ges <- datenFuellen(dat = logit.ver, modell = logit.ver2, logit = TRUE)
ges <- ges %>% 
  mutate(Verlauf = ifelse(Verlauf == 0, "N", "J"))

#### Längenausgleich ####

## Länge der Sortimente anschauen ##

#--> auf Skriptbasis 1_x verbleiben alle Abschnitte
# ab Skript 4_x werden die Daten abgefiltert (keine Soprtimentszeit und keine Baumzeit)
boxplot(ges$Laenge_cm ~ ges$Sortiment)
sum(is.na(ges$DM_cm))
sum(is.na(ges$Laenge_m))

## IS über mean ##
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

## S über mean ##
mean.s <- mean(ges$Laenge_m[ges$Sortiment == "S"], na.rm = T)

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "S" & is.na(Laenge_m),
                                        mean.s,
                                        Laenge_m))

ges <- ges %>% mutate(Laenge_cm = ifelse(Sortiment == "S" & is.na(Laenge_cm),
                                         mean.s*100,
                                         Laenge_cm))

## SH über BHD des eigentlichen Baumes -> hier nicht notwendig da nur IS fehlt ## 
lm.1 <- lm(Laenge_cm ~ BHD_cm, data = ges[ges$Sortiment == "SH",])
lm.1.coef <- coef(lm.1[1])
summary(lm.1)

ges <- ges %>%
  mutate(Laenge_cm = ifelse(Sortiment == "SH" & is.na(Laenge_cm),
                            lm.1.coef[1]+lm.1.coef[2]*BHD_cm,
                            Laenge_cm))

ges <- ges %>% mutate(Laenge_m = ifelse(Sortiment == "SH" & is.na(Laenge_m),
                                        Laenge_cm/100,
                                        Laenge_m))

#### auffüllen der Durchmesserwerte , auf Wechselwirkungen getestet - Verlauf auf Zeit integriert ####
lm.2 <- lm(DM_cm ~ Sortiment + Stamm_Krone + Abschnitt + Verlauf, data = ges)
step.lm.2 <- stepAIC(lm.2, direction = "backward", trace = TRUE)

gam.2 <- gam(DM_cm ~ Sortiment + Stamm_Krone + s(Abschnitt) + Verlauf + s(BHD_cm), data = ges)
summary(gam.2)
hist(resid(gam.2, type = "response")) # Nimm das GAM man
hist(resid(step.lm.2))

## Ausfüllen dm_cm ##
ges <- datenFuellen(dat = ges, modell = gam.2)
colSums(is.na(ges))

## Auffüllen der dm_m - Werte ##
ges <- ges %>% 
  mutate(DM_m = ifelse(is.na(DM_m),
                       DM_cm/100,
                       DM_m))

boxplot(ges$DM_cm ~ ges$Sortiment)

#### Volumen überschreiben ####
ges <- ges %>% 
  mutate(Volumen = ((pi/4)* (DM_m^2)) * Laenge_m )

#### Datensatz trennen ####
stk2 <- ges[,c(1:58)]
stk2 <- stk2 %>% 
  rename(Bemerkungen = Bemerkungen.x)

## schreiben von fix.fm / Anzahl gründe ##
stk2 <- stk2 %>% 
  mutate(fix.fm = Zeit_N/Volumen)

stk2$Grund_Anzahl <- stk2 %>% 
  select(Grund_A:Grund_Z) %>%
  apply(1, function(x) sum(!is.na(x)))

#### schreiben des Baumvolumens ####
## Baumvolumen über Funktion / Buche Derbholzfunktion nach Bergel ##
baum2$Volumen_func <- bergel(h = baum2$Hoehe_m, d = baum2$BHD_cm)

## Baumvolumen über Summe der Abschnitte ##
l1 <- aggregate(Volumen ~ Baum_Nr, data = stk2, FUN = sum)
baum2 <- merge(baum2, l1, by.x = "Baum_Nr", by.y = "Baum_Nr", all.x = TRUE)
baum2 <- baum2 %>% mutate(Volumen_stk = Volumen)
baum2 <- baum2[,c(1:31)]


#### Datensatz speichern ####
con <- dbConnect(RSQLite::SQLite(), "data_gerlau_new.db")
sqlStatement3 <- paste0(flaeche,"_stk_w")
sqlStatement4 <- paste0(flaeche,"_baum_w")
sqlStatement5 <- paste0(flaeche,"_stk_h")
sqlStatement6 <- paste0(flaeche,"_baum_h")

dbWriteTable(con, name = sqlStatement3, stk1, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement4, baum1, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement5, stk2, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement6, baum2, overwrite = TRUE)
rm(list = ls())

