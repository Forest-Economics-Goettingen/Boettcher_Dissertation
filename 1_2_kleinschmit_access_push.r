#### Access -> Sqlite - BEST Kleinschmit ####

# Autor: Fabian Böttcher
# Datum: 14.11.2019

#### Pakete laden ####
library(RODBC)
library(sqliter)
library(DBI)

#### Daten einlesen ####
con <- odbcConnectAccess2007("./data_kleinschmit.accdb")

# Eigentliches Einlesen
sort <- sqlFetch(channel = con, sqtable = 'Mollenfelde_Handmaß_20130115')
harv <- sqlFetch(channel = con, sqtable = 'Mollenfelde_Harvestermaß_20130115')
stan <- sqlFetch(channel = con, sqtable = 'Mollenfelde_Stehendaufnahme_20121025')
zeit <- sqlFetch(channel = con, sqtable = 'Mollenfelde_Zeitstudie_20130118')

#### Beschreiben der SQLite DB ####
con.sql <- dbConnect(RSQLite::SQLite(), "data_gerlau.db")
dbListTables(con.sql)

#### Schreiben der SQL-Tabellen ####
dbWriteTable(con.sql, name = "he_3_baum", stan, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_3_stk", sort, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_3_zeit", zeit, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_3_harv", harv, overwrite = TRUE)
dbDisconnect(con.sql)

#### Datenabschluss ####

odbcCloseAll()
rm(list = ls())
