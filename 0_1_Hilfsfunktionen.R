#--------------------------------------------------#
#### Funktionensammlung für die Analysen in 3_1 ####
#--------------------------------------------------#

# Kai
# Tue Nov 19 13:54:43 2019 ------------------------------

# tbd. Es gibt noch keine Abfrage, ob unter den kovariablen für die Vorhersage vielleicht auch NA sind. Das muss unbedingt noch nachgeholt werden.
datenFuellen <- function (dat, modell, logit = FALSE) {
  # Erkennt die Variablen des Modells
  yVar <- names(modell$model)[1]
  xVar <- names(modell$model)[2 : length(names(modell$model))]
 
   # Abfragen, ob es überhaupt y-Variablen zum Ausgleichen gibt

  if (!any(is.na(dat[, yVar]))) {
    cat("Es gibt hier nichts zum ausgleichen.")
    break()
  }

  
  # Abfragen, ob der x-Variablen Datensatz keine NA enthält
  if (any(is.na(dat[, xVar]))) {
    cat("Es gab NA in den Kovariablen, deshalb konnte keine Datenvervollstaendigung durchgefuehrt werden.")
    return(apply(dat[, xVar], 2, is.na) %>% apply(2, any))
    break()
  }
  
  # Sucht die Spalten in dat, in denen die y Variable NA ist und füllt sie mit der Vorhersage über das Modell
  newData <- dat %>% filter(is.na(!!as.name(yVar))) %>%  select(xVar) %>% as.data.frame()
  if(logit) {
    dat[is.na(dat[,yVar]), yVar] <-
      as.numeric(predict(modell, newdata = newData, type = "response") > runif(length(dat[is.na(dat[,yVar]), yVar])))
    
  } else {
    dat[is.na(dat[,yVar]), yVar] <- predict(modell, newdata = newData)
  }
  
  return(dat)
}

#### Volumenfunktion nach Bergel für Buche Derbholz #### 
bergel <- function (h, d) {
  3.141592 * h * (d/200)^2 * (0.4039+0.0017335*h+1.1267/h-118.188/(d^3)+0.0000042*d^2)
}
