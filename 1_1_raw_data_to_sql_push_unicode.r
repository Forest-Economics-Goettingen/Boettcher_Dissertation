#### Access -> SQLite ####

# Autor: Fabian Böttcher
# Datum: 13.11.2019

#### Pakete laden ####
library(MASS)
library(sqliter)
library(DBI)
library(tidyverse)
library(cowplot)
library(readxl)

#### Daten einlesen Abschnitte ####
ls_1_stk <- read_excel("data_gerlau.xlsx", sheet = "stk_ls_1")
ls_2_stk <- read_excel("data_gerlau.xlsx", sheet = "stk_ls_2")

he_1_stk <- read_excel("data_gerlau.xlsx", sheet = "stk_he_1")
he_2_stk <- read_excel("data_gerlau.xlsx", sheet = "stk_he_2")

#### Daten einlesen Bäume ####
ls_1_baum <- read_excel("data_gerlau.xlsx", sheet = "baum_ls_1")
ls_2_baum <- read_excel("data_gerlau.xlsx", sheet = "baum_ls_2")
ls_3_baum <- read_excel("data_gerlau.xlsx", sheet = "baum_ls_3")

he_1_baum <- read_excel("data_gerlau.xlsx", sheet = "baum_he_1")
he_2_baum <- read_excel("data_gerlau.xlsx", sheet = "baum_he_2")

#### Änderungen der coloumn-types ls #### 
ls_1_stk[, c(1,4:6,9:13,16:17,24:58)] <- sapply(ls_1_stk[, c(1,4:6,9:13,16:17,24:58)], as.numeric)
ls_1_stk[, c(2:3,15)] <- sapply(ls_1_stk[, c(2:3,15)], as.character)
ls_1_stk[, c(7:8,14,18:23)] <- sapply(ls_1_stk[, c(7:8,14,18:23)], factor)

ls_2_stk[, c(1,4:6,9:13,16:17,24:58)] <- sapply(ls_2_stk[, c(1,4:6,9:13,16:17,24:58)], as.numeric)
ls_2_stk[, c(2:3,15)] <- sapply(ls_2_stk[, c(2:3,15)], as.character)
ls_2_stk[, c(7:8,14,18:23)] <- sapply(ls_2_stk[, c(7:8,14,18:23)], factor)

ls_1_baum[, c(1,4,6:11,13:16,18:23)] <- sapply(ls_1_baum[, c(1,4,6:11,13:16,18:23)], as.numeric)
ls_1_baum[, c(2:3,17)] <- sapply(ls_1_baum[, c(2:3,17)], as.character)
ls_1_baum[, c(5,12,24:31)] <- sapply(ls_1_baum[, c(5,12,24:31)], factor)

ls_2_baum[, c(1,4,6:11,13:16,18:23)] <- sapply(ls_2_baum[, c(1,4,6:11,13:16,18:23)], as.numeric)
ls_2_baum[, c(2:3,17)] <- sapply(ls_2_baum[, c(2:3,17)], as.character)
ls_2_baum[, c(5,12,24:31)] <- sapply(ls_2_baum[, c(5,12,24:31)], factor)

ls_3_baum[, c(1,4,6:11,13:16,18:23)] <- sapply(ls_3_baum[, c(1,4,6:11,13:16,18:23)], as.numeric)
ls_3_baum[, c(2:3,17)] <- sapply(ls_3_baum[, c(2:3,17)], as.character)
ls_3_baum[, c(5,12,24:31)] <- sapply(ls_3_baum[, c(5,12,24:31)], factor)

#### Änderungen der coloumn-types he #### 
he_1_stk[, c(1,4:6,9:13,16:17,24:58)] <- sapply(he_1_stk[, c(1,4:6,9:13,16:17,24:58)], as.numeric)
he_1_stk[, c(2:3,15)] <- sapply(he_1_stk[, c(2:3,15)], as.character)
he_1_stk[, c(7:8,14,18:23)] <- sapply(he_1_stk[, c(7:8,14,18:23)], factor)

he_2_stk[, c(1,4:6,9:13,16:17,24:58)] <- sapply(he_2_stk[, c(1,4:6,9:13,16:17,24:58)], as.numeric)
he_2_stk[, c(2:3,15)] <- sapply(he_2_stk[, c(2:3,15)], as.character)
he_2_stk[, c(7:8,14,18:23)] <- sapply(he_2_stk[, c(7:8,14,18:23)], factor)

he_1_baum[, c(1,4,6:11,13:16,18:23)] <- sapply(he_1_baum[, c(1,4,6:11,13:16,18:23)], as.numeric)
he_1_baum[, c(2:3,17)] <- sapply(he_1_baum[, c(2:3,17)], as.character)
he_1_baum[, c(5,12,24:31)] <- sapply(he_1_baum[, c(5,12,24:31)], factor)

he_2_baum[, c(1,4,6:11,13:16,18:23)] <- sapply(he_2_baum[, c(1,4,6:11,13:16,18:23)], as.numeric)
he_2_baum[, c(2:3,17)] <- sapply(he_2_baum[, c(2:3,17)], as.character)
he_2_baum[, c(5,12,24:31)] <- sapply(he_2_baum[, c(5,12,24:31)], factor)

#### schreiben eines Gesamt-Datensatzes ####
ges_stk <- rbind(ls_1_stk, ls_2_stk, he_1_stk, he_2_stk)
ges_baum <- rbind(ls_1_baum, ls_2_baum, he_1_baum, he_2_baum)

#### Beschreiben der SQLite DB ####
con.sql <- dbConnect(RSQLite::SQLite(), "data_gerlau.db")
dbListTables(con.sql)

#### Schreiben der SQL-Tabellen ####
dbWriteTable(con.sql, name = "ls_1_stk", ls_1_stk, overwrite = TRUE)
dbWriteTable(con.sql, name = "ls_2_stk", ls_2_stk, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_1_stk", he_1_stk, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_2_stk", he_2_stk, overwrite = TRUE)
dbWriteTable(con.sql, name = "ges_stk", ges_stk, overwrite = TRUE)

dbWriteTable(con.sql, name = "ls_1_baum", ls_1_baum, overwrite = TRUE)
dbWriteTable(con.sql, name = "ls_2_baum", ls_2_baum, overwrite = TRUE)
dbWriteTable(con.sql, name = "ls_3_baum", ls_3_baum, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_1_baum", he_1_baum, overwrite = TRUE)
dbWriteTable(con.sql, name = "he_2_baum", he_2_baum, overwrite = TRUE)
dbWriteTable(con.sql, name = "ges_baum", ges_baum, overwrite = TRUE)

dbDisconnect(con.sql)
rm(list = ls())
