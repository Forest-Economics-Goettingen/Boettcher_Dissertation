#### Datensicht ####

# Autor: Fabian Böttcher
# Datum: 25.11.2019

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
con <- dbConnect(RSQLite::SQLite(), "./data_gerlau.db")

#### Fläche wählen ####
flaeche <- "he_2"
## Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 ##

#### Daten einlesen ####
sqlStatement1 <- paste("SELECT * FROM ",flaeche,"_stk", sep = "")
sqlStatement2 <- paste("SELECT * FROM ",flaeche,"_baum", sep = "")
stk <- dbGetQuery(con, sqlStatement1)
baum <- dbGetQuery(con, sqlStatement2)
dbDisconnect(con)

#### Datensicht Abschnitte ####
boxplot(stk$Laenge_m ~ stk$Sortiment)
boxplot(stk$DM_m ~ stk$Sortiment)

stk %>% 
  ggplot(aes(x=DM_cm, y=Zeit_N, color = Verlauf))+
  geom_point(alpha = 0.6)+
  geom_smooth(se = FALSE,size = 2, method = "glm", formula = y~x)+
  labs(x="Log volume [m³/piece]", y="Processing time [sec./piece]", title = "Log processing - linear")+
  theme_gray(base_size = 15)+
  theme(legend.position = "none")+
  scale_color_discrete(name="additional activities",
                       breaks=c("J", "N"),
                       labels=c("yes", "no"))



#### Datensicht Baum ####
boxplot(baum$Faellung)
boxplot(baum$Kronenrest)
boxplot(baum$Faellung)


summary(baum)

#### Datensicht Baum ####

rm(list = ls())
