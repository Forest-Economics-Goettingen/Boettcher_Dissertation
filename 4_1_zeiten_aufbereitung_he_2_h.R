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
con <- dbConnect(RSQLite::SQLite(), "./data_gerlau_new.db")

#### Hilffunktionen laden ####
source("0_1_Hilfsfunktionen.R")

#### Fläche wählen ####
flaeche <- "he_2"
## Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 ##

#### Daten einlesen ####
sqlStatement1 <- paste0("SELECT * FROM ",flaeche,"_stk_h")
sqlStatement2 <- paste0("SELECT * FROM ",flaeche,"_baum_h")
stk <- dbGetQuery(con, sqlStatement1)
baum <- dbGetQuery(con, sqlStatement2)

## abfiltern der Bäume die nicht gefällt wurden ## 
baum <- baum[!baum$Geerntet %in% 1,]

# Hoehe / Ka_m
lm1 <- glm(Hoehe_m ~ BHD_cm, data = baum)
summary(lm1)
hist(resid(lm1))
baum <- datenFuellen(dat = baum, modell = lm1)

lm2 <- glm(KA_m ~ Hoehe_m + Qualitaet + BHD_cm, data = baum)
summary(lm2)
hist(resid(lm2))
baum <- datenFuellen(dat = baum, modell = lm2)

#Baumvolumen über Funktion / Buche Derbholzfunktion nach Bergel ##
baum$Volumen_func <- bergel(h = baum$Hoehe_m, d = baum$BHD_cm)
colSums(is.na(baum))

#### Auswertung ####

#### Datenausgleich N-Zeit ####

### Aufarbeitung vor ausgleich ###
plot1 <- stk %>% 
  ggplot(aes(x=Volumen, y=Zeit_N, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  geom_text(aes(label = Abschnittsdaten_ID))+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./piece]", title = "Log processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")+
  scale_color_discrete(name="additional activities",
                       breaks=c("J", "N"),
                       labels=c("yes", "no"))

plot2 <- stk %>% 
  ggplot(aes(x=Volumen, y=fix.fm, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE, method = "nls",size = 2, formula = y~A+(B/x), method.args = list(start=c(A=1, B=1)))+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./m³]", title = "Log processing - Hyperbole")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")+
  scale_color_discrete(name="Process",
                       breaks=c("J", "N"),
                       labels=c("disturbed", "normal"))

plot_grid(plot1, plot2)


## auffüllen der Zeit_N Wert, keine Wechselwirkungen gefunden ##
lm.1 <- lm(Zeit_N ~ Sortiment + Stamm_Krone + Abschnitt + Verlauf + DM_cm + Laenge_m, data = stk)
step.lm.1 <- stepAIC(lm.1, direction = "backward", trace = TRUE)

gam.1 <- gam(Zeit_N ~ Sortiment + Stamm_Krone + s(Abschnitt) + Verlauf + s(DM_cm) + s(Laenge_m), data = stk)
hist(resid(gam.1, type = "response"))
hist(resid(step.lm.1))

# Ausfüllen der Zeit_N Werte ##
stk <- datenFuellen(dat = stk, modell = gam.1)

## schreiben fix.fm.z und fix.fm.g ##
stk <- stk %>% 
  mutate(fix.fm.z = Zeit_U/Volumen) %>%
  mutate(fix.fm = Zeit_N/Volumen) %>% 
  mutate(fix.fm.g = fix.fm + fix.fm.z)

### Aufarbeitung nach ausgleich ###
plot3 <- stk %>% 
  ggplot(aes(x=Volumen, y=Zeit_N, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./piece]", title = "Log processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")+
  scale_color_discrete(name="additional activities",
                       breaks=c("J", "N"),
                       labels=c("yes", "no"))

plot4 <- stk %>% 
  ggplot(aes(x=Volumen, y=fix.fm, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE, method = "nls",size = 2, formula = y~A+(B/x), method.args = list(start=c(A=1, B=1)))+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./m³]", title = "Log processing - Hyperbole")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")+
  scale_color_discrete(name="Process",
                       breaks=c("J", "N"),
                       labels=c("disturbed", "normal"))

plot_grid(plot3, plot4)

## Z-Zeiten ausgleichen - Versuch ##
stk %>% 
  ggplot(aes(x=Volumen, y=Zeit_U, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./piece]", title = "Log processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")
# Z-Zeiten ausgleichen macht auf Abschnittsebene keinen Sinn, da erst eine Wahrscheinlichkteit ob Umgreifen 0|1 ist
# im Anschluss müsste Regression erfolgen um die Zeiten auszugleichen

## zweiter Ansatz ##
# Anzahl der Umgreifvorgänge pro Abschnitt über Regression schätzen
# Wert über mean/median pro Anzahl Umgreifen
boxplot(stk$Zeit_U ~ stk$Umgreifen_Anzahl)

# Ausgleich erfolgt aber auf Baumebene
# In den Auswertungen zu den Z-Zeiten auf Abschnittsebene werden nur Abschnitte mit Var. Fehlstück != 2 genutzt

# --> Abschnittsdatenausgleich ist abgeschlossen

#### Datenprüfung Bäume ####
## Baumzeit N über Summe der Abschnitte ##
l1 <- aggregate(Zeit_N ~ Baum_Nr, data = stk, FUN = sum)
baum2 <- merge(baum, l1, by.x = "Baum_Nr", by.y = "Baum_Nr", all.x = TRUE)
baum <- baum2 %>% mutate(Sortimente = Zeit_N)
baum <- baum[,c(1:32)]

## aggregation einer Sortimentsvariablen ##
test1 <- tibble(Baum_Nr = stk$Baum_Nr %>% unique())
test2 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "SH"] %>% 
                  unique(), Sorten = "SH")
test3 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "S"] %>% 
                  unique(), Sorten = "S")
test4 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "PA"] %>% 
                  unique(), Sorten = "PA")
test5 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "IS"] %>% 
                  unique(), Sorten = "IS")
test1 <- left_join(test1, test2, by = "Baum_Nr")
test1 <- left_join(test1, test3, by = "Baum_Nr")
test1 <- left_join(test1, test4, by = "Baum_Nr")
test1 <- left_join(test1, test5, by = "Baum_Nr")
test1$Anzahl_Sorten <- test1 %>% 
  select(Sorten.x:Sorten.y.y) %>%
  apply(1, function(x) sum(!is.na(x)))
test1[is.na(test1)] <- ""
test1 <- test1 %>% mutate(Sorten = paste(Sorten.x,Sorten.y,Sorten.x.x,Sorten.y.y, sep = "-"))
baum <- left_join(baum, select(test1, "Baum_Nr", "Sorten", "Anzahl_Sorten"), by = "Baum_Nr")


## Fuellen der Verlaufs_Variablen mit N (Test) ##
baum <- baum %>% 
  mutate(Verlauf_Anfahrt = coalesce(Verlauf_Anfahrt, "N"),
         Verlauf_Greifen = coalesce(Verlauf_Greifen, "N"),
         Verlauf_Faellung = coalesce(Verlauf_Faellung, "N"),
         Verlauf_Kronenrest = coalesce(Verlauf_Kronenrest, "N"))


# plot normal Zeit_N # 
baum %>% 
  ggplot(aes(x=Volumen_stk, y=Sortimente+Umgreifen, color = as.factor(Anzahl_Sorten)))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "bottom")

# Vergleich Baumvolumen über die Summe der Abschnitte mit der Volumenberechnung über Hilfsfunktion
baum %>% 
  ggplot(aes(x=Volumen_stk, y=Volumen_func))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

## Faellung ; noch ausgleichen ##
baum %>% 
  ggplot(aes(x=Volumen_stk, y=Faellung, color = Verlauf_Faellung))+
  geom_point(alpha = 0.6)+
  geom_text(aes(label = Baum_Nr))+
  geom_smooth(se = FALSE,size = 2, method = "gam", formula = y~s(x))+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

# lm und testen auf Wechselwirkungen; übrig bleibt nur Volumen_stk; getestet: Qualität, Kronentyp, BHD, Höhe
# poisson-Verteilung probiert, klappt aber nicht

# Es gibt 6 Bäume, die nicht gefällt wurden. Hier gibt es dementsprechend auch kein gemessenes Volumen. Deshalb werden
# diese 6 fehlenden Volumina mit der Bergel Funkti0n ausgeglichen, damit das Füll-Modell parametrisiert werden kann.
dim(baum[is.na(baum$Volumen_stk),])
baum[is.na(baum$Volumen_stk), "Volumen_stk"] <- baum[is.na(baum$Volumen_stk), "Volumen_func"]

lm.b1 <- glm(Faellung ~ Volumen_stk + Verlauf_Faellung, data = baum)
gam.b1 <- gam(Faellung ~ s(Volumen_stk) + Verlauf_Faellung, data = baum)
summary(gam.b1)
summary(lm.b1)

hist(resid(gam.b1, type = "response"))
hist(resid(lm.b1))

baum <- datenFuellen(dat = baum, modell = gam.b1)

## Greifen  ##
baum %>%
  ggplot(aes(x=Volumen_stk, y=Greifen, color = Verlauf_Greifen))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = T,size = 2, method = "glm", formula = y~x)+
  geom_text(aes(label = Baum_Nr))+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

# lm.b2 <- glm(Greifen ~ Volumen_stk, data = baum)
# summary(lm.b2)
lm.b2 <- glm(Greifen ~ Volumen_stk + Verlauf_Greifen, data = baum)
gam.b2 <- gam(Greifen ~ s(Volumen_stk) + Verlauf_Greifen, data = baum)
summary(lm.b2)
summary(gam.b2)
hist(resid(lm.b2))
hist(resid(gam.b2, type = "response"))


baum <- datenFuellen(dat = baum, modell = gam.b2)

## Kronentyp ##
baum %>% ggplot(aes(x = Kronentyp, y = Kronenrest)) + geom_boxplot()
# Offenbar sind die Kronen bei denen Zeiten fehlen alle Typ 2.
baum$Kronentyp[is.na(baum$Kronentyp)] <- 2
colSums(is.na(baum))
## Kronenrest ##
baum %>% 
  ggplot(aes(x=Volumen_stk, y=Kronenrest, color = Kronentyp))+
  geom_point(alpha = 0.6)+
  geom_text(aes(label = Baum_Nr))+
  geom_smooth(se = FALSE,size = 2, method = "gam")+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

baum %>% ggplot(aes(x = Kronentyp, y = Kronenrest)) + geom_boxplot()

lm.b3 <- gam(Kronenrest ~ Volumen_stk + Kronentyp, data = baum)
gam.b3 <- gam(Kronenrest ~ s(Volumen_stk) + Kronentyp, data = baum)
summary(lm.b3)
summary(gam.b3)
hist(resid(lm.b3))
hist(resid(gam.b3, type = "response"))

baum <- datenFuellen(dat = baum, modell = lm.b3)

## Anfahrt ; noch ausgleichen --> Mittelwert##
baum %>% 
  ggplot(aes(x=Volumen_stk, y=Anfahrt))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

# Entfernung macht Sinn, wurde aber in dieser Fläche nicht erhoben.
baum %>% 
  ggplot(aes(x=Entfernung_Baum, y=Anfahrt, color = Verlauf_Anfahrt))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Single tree volume [m³/tree]", y="Processing time [sec./piece]", title = "Tree processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")

lm.b4 <- gam(Anfahrt ~ Entfernung_Baum, data = baum)
gam.b4 <- gam(Anfahrt ~ s(Entfernung_Baum), data = baum)
summary(lm.b4)
summary(gam.b4)
hist(resid(lm.b4))
hist(resid(gam.b4, type = "response"))

baum <- datenFuellen(dat = baum, modell = gam.b4)

## Umgreifen ##

# boxplot(stk$Zeit_U ~ stk$Umgreifen_Anzahl)
# boxplot(baum$Umgreifen ~ baum$Umgreifen_Anzahl)
# sum(is.na(baum$Umgreifen))
# 
# lm.b4 <- glm(Umgreifen_Anzahl ~ Volumen_stk + Kronentyp, data = baum) # Besser
# gam.b4 <- gam(Umgreifen_Anzahl ~ s(Volumen_stk) + s(Sortimente) + Sorten + Kronentyp, data = baum)
# summary(gam.b4)
# summary(lm.b4)
# hist(resid(lm.b4))
# hist(resid(gam.b4, type = "response"))
# 
# baum <- datenFuellen(dat = baum, modell = lm.b4)
# 
colSums(is.na(baum))

#### Speichern ####
sqlStatement3 <- paste0(flaeche,"_stk_h")
sqlStatement4 <- paste0(flaeche,"_baum_h")
dbWriteTable(con, name = sqlStatement3, stk, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement4, baum, overwrite = TRUE)
rm(list = ls())


