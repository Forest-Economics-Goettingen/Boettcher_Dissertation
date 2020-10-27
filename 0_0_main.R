#-----------------------------------------#
#### Mainscript - control data fill ####
#-----------------------------------------#


## sourcen der scripte ##



## Auswahl der Versuchsfläche ##
flaeche <- "he_1"
# Auswahl| ls_1;ls_2;ls_3;he_1;he_2;he_3 #
source("1_1_raw_data_to_sql_push_unicode.r", echo = TRUE, encoding = "UTF-8")

source(paste0("2_1_volumen_ausgleich_",flaeche,".R"), echo = TRUE, encoding = "UTF-8")


## datapush .xlsx to sqlite ##
source("1_1_raw_data_to_sql_push_unicode.r", echo = TRUE, encoding = "UTF-8")
# kleinschmit daten sind gepusht / RODBC wirft aber jetzt Fehler aus --> readR?
# source("1_2_kleinschmit_access_push.r", echo = TRUE, encoding = "UTF-8")

## fill 1 | Volumen, Durchmesser, Längen, Stamm/Krone, Abschnitt etc.
flaeche <- "he_1"
source(paste0("2_1_volumen_ausgleich_",flaeche,".R"), echo = TRUE)
## fill 1.5 | Umgreifen-loop



## fill 2 | Zeiten

