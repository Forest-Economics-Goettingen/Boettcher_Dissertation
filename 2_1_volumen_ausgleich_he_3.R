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
flaeche <- "he_3"
## Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 ##

#### Daten einlesen ####
sqlStatement1 <- paste0("SELECT * FROM ",flaeche,"_stk")
sqlStatement2 <- paste0("SELECT * FROM ",flaeche,"_baum")
sqlStatement3 <- paste0("SELECT * FROM ",flaeche,"_zeit")
sqlStatement4 <- paste0("SELECT * FROM ",flaeche,"_harv")
sqlStatement5 <- paste0("SELECT * FROM ls_1_stk")
sqlStatement6 <- paste0("SELECT * FROM ls_1_baum")
stk <- dbGetQuery(con.sql, sqlStatement1)
baum <- dbGetQuery(con.sql, sqlStatement2)
zeit <- dbGetQuery(con.sql, sqlStatement3)
harv <- dbGetQuery(con.sql, sqlStatement4)
stk.names <- dbGetQuery(con.sql, sqlStatement5)
baum.names <- dbGetQuery(con.sql, sqlStatement6)

#----------------------------#
#### Datenaufbereitung II ####
###          BEST          ###
#----------------------------#

stk$Volumen <- ((pi / 4) * ((stk$MDM_gemittelt) / 1000 )^2) * stk$Laenge_m 

## Abschnittsdaten ##
# aufbereiten der Zeitdaten und aussortieren
zeit2 <- zeit[, c(1:6,9,10,12)]
zeit3 <- aggregate(zeit2$Arbeitsschrittzeit, by=list(ID=zeit2$ID_Messung), FUN=sum)
stk <- merge(stk, zeit3, by.x = "ID", by.y = "ID", all.x = TRUE) %>% 
  rename("Zeit_N"=x)
stk$fix.fm <- stk$Zeit_N/stk$Volumen
stk <- stk[stk$Zeit_N > 0 & !(is.na(stk$Zeit_N)),]
# Zusatzzeiten nach Kleinschmidt vielleich noch anheften

# stk ist bereinigt --> zur Abschnittsauswertung nutzen

## Baumdaten ##

# Faellung
zeit4 <- zeit2[zeit2$Abschnitt == "F" &! is.na(zeit2$Abschnitt),]
zeit4 <- aggregate(zeit4$Arbeitsschrittzeit, by=list(ID=zeit4$Baum_Nr), FUN=sum)
baum <- merge(baum, zeit4, by.x = "ID_Baum_stehend", by.y = "ID", all.x = TRUE) %>% 
  rename("Faellung"=x)
# Kronenzeit
zeit4 <- zeit2[zeit2$Abschnitt == "KR" &! is.na(zeit2$Abschnitt),]
zeit4 <- aggregate(zeit4$Arbeitsschrittzeit, by=list(ID=zeit4$Baum_Nr), FUN=sum)
baum <- merge(baum, zeit4, by.x = "ID_Baum_stehend", by.y = "ID", all.x = TRUE) %>% 
  rename("Kronenrest"=x)
# Anfahrt
zeit4 <- zeit2[zeit2$Abschnitt == "A" &! is.na(zeit2$Abschnitt),]
zeit4 <- aggregate(zeit4$Arbeitsschrittzeit, by=list(ID=zeit4$Baum_Nr), FUN=sum)
baum <- merge(baum, zeit4, by.x = "ID_Baum_stehend", by.y = "ID", all.x = TRUE) %>% 
  rename("Anfahrt"=x)
# Zusatzzeiten
zeit2$hv_z <- str_detect(zeit2$Abschnitt, "z")
zeit4 <- zeit2[zeit2$hv_z == "TRUE" &! is.na(zeit2$Abschnitt),]
zeit4 <- aggregate(zeit4$Arbeitsschrittzeit, by=list(ID=zeit4$Baum_Nr), FUN=sum)
baum <- merge(baum, zeit4, by.x = "ID_Baum_stehend", by.y = "ID", all.x = TRUE) %>% 
  rename("Umgreifen"=x)
# Sortimentszeiten
zeit2$h1 <- str_extract(zeit2$ID_Messung, "[VSTPAFKRAz]")
zeit5 <- zeit2[is.na(zeit2$h1),]
zeit5 <- zeit5[zeit5$Arbeitsschrittzeit > 0 & !(is.na(zeit5$Arbeitsschrittzeit)),]

h1 <- stk %>% count(BaumNr)
h2 <- zeit5 %>% count(Baum_Nr) 

h3 <- merge(x=h1, y=h2, by.x = "BaumNr", by.y = "Baum_Nr")
h3 <- filter(h3, h3$n.x==h3$n.y)
h3 <- h3[,1]

zeit6 <- subset(zeit5, Baum_Nr %in% h3) 
zeit7 <- aggregate(zeit6$Arbeitsschrittzeit, by=list(ID=zeit6$Baum_Nr), FUN=sum)
baum <- merge(baum, zeit7, by.x = "ID_Baum_stehend", by.y = "ID", all.x = TRUE) %>% 
  rename("Sortimente"=x)
# anheften des Volumens

l1 <- aggregate(Volumen ~ BaumNr, data = stk, FUN = sum)
baum <- merge(baum, l1, by.x = "ID_Baum_stehend", by.y = "BaumNr", all.x = TRUE)

## rename der Variablen
baum <- baum %>% 
  rename(Baum_Nr = ID_Baum_stehend,
         KA_m = Kronenansatz_m,
         Ast_m = Zwieselhoehe,
         Volumen_stk = Volumen)

stk  <- stk %>% 
  rename(Abschnittsdaten_ID = ID,
         Baum_Nr = BaumNr,
         DM_mm = MDM_gemittelt)

# Auflistung der Variablen, die es in stk nicht gibt
stk.names <- colnames(stk.names)
namesNotInStk <- stk.names[!stk.names %in% names(stk)]
namesOnlyInStk <- colnames(stk[!names(stk) %in% stk.names])
stk.order <- c(stk.names, namesOnlyInStk)
# # 49 der 58 Variblen kommen in stk nicht vor. Es werden leere Spalten mit diesen namen erzeugt und zunächst rechts an stk angeheftet
emptyNewColumns <- data.frame(matrix(nrow = dim(stk)[1], ncol = length(namesNotInStk)))
names(emptyNewColumns) <- namesNotInStk
stk <- cbind(stk, emptyNewColumns)
stk <- stk %>% select(stk.order)

# Auflistung der Variablen, die es in baum nicht gibt
baum.names <- colnames(baum.names)
namesNotInbaum <- baum.names[!baum.names %in% names(baum)]
namesOnlyInbaum <- colnames(baum[!names(baum) %in% baum.names])
baum.order <- c(baum.names, namesOnlyInbaum)
# # 49 der 58 Variblen kommen in baum nicht vor. Es werden leere Spalten mit diesen namen erzeugt und zunächst rechts an baum angeheftet
emptyNewColumns <- data.frame(matrix(nrow = dim(baum)[1], ncol = length(namesNotInbaum)))
names(emptyNewColumns) <- namesNotInbaum
baum <- cbind(baum, emptyNewColumns)
baum <- baum %>% select(baum.order)

## wegen Fehlern in der Datenaufnahme werden die Datensätze gelöscht,
## die keine Zeiten aufweisen und bei denen nichts zu den Abschnitten
## bekannt ist
list_0 <- stk %>% 
  select(Baum_Nr) %>% 
  unique()
list_1 <- baum %>% 
  select(Baum_Nr) %>% 
  unique()
list_2 <- anti_join(list_1, list_0)
list_3 <- stk %>% 
  filter(is.na(Zeit_N) | is.na(DM_mm)) %>%
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
stk <stk %>% filter(!Baum_Nr %in% list_6$Baum_Nr)
baum <- baum %>% filter(!Baum_Nr %in% list_6$Baum_Nr)

## Spalten auffüllens
stk %>% 
  ggplot(aes(x=Laenge_m, y=Zeit_N))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  theme_gray(base_size = 15)
# -> es gibt 3 Sortimente IS 3m, S 5m und SH 8m
stk <- stk %>% 
  mutate(Sortiment = ifelse(Laenge_m <= 4.5, "IS", ifelse(Laenge_m >= 6.5, "SH", "S"))) %>% 
  mutate(DM_cm = DM_mm/10) %>% 
  mutate(DM_m = DM_mm/1000)

baum$Volumen_func <- bergel(h = baum$Hoehe_m, d = baum$BHD_cm)
baum$Umgreifen_Anzahl <- NA

## Datensatz trennen
baum1 <- baum[,c(1:31,34)]
stk1 <- stk[,c(1:58)]

## Sortimentsvariable an baum1 anhängen
## aggregation einer Sortimentsvariablen ##
test1 <- tibble(Baum_Nr = stk$Baum_Nr %>% unique())
test2 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "SH"] %>% 
                  unique(), Sorten = "SH")
test3 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "S"] %>% 
                  unique(), Sorten = "S")
test4 <- tibble(Baum_Nr = stk$Baum_Nr[stk$Sortiment %in% "IS"] %>% 
                  unique(), Sorten = "IS")
test1 <- left_join(test1, test2, by = "Baum_Nr")
test1 <- left_join(test1, test3, by = "Baum_Nr")
test1 <- left_join(test1, test4, by = "Baum_Nr")

test1$Anzahl_Sorten <- test1 %>% 
  select(Sorten.x:Sorten) %>%
  apply(1, function(x) sum(!is.na(x)))

# Test Kai #
test1$SortenKai <- test1 %>% select(Sorten.x : Sorten) %>% apply(1, function(x) {(paste(x[!is.na(x)], collapse = "-"))})
baum1 <- left_join(baum1, select(test1, "Baum_Nr", "SortenKai", "Anzahl_Sorten"), by = "Baum_Nr")
baum1 <- baum1 %>% 
  rename(Sorten = SortenKai)

## Flächenvariable schreiben
baum1$Flaeche <- "he_3"
stk1$Flaeche <- "he_3"

#### Datensatz speichern ####
con <- dbConnect(RSQLite::SQLite(), "data_gerlau_new.db")
sqlStatement10 <- paste(flaeche,"_stk", sep = "")
sqlStatement11 <- paste(flaeche,"_baum", sep = "")
dbWriteTable(con, name = sqlStatement10, stk1, overwrite = TRUE)
dbWriteTable(con, name = sqlStatement11, baum1, overwrite = TRUE)
rm(list = ls())



