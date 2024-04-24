# 1 - Libraries ----
library(insee)
library(tseries)
library(ggplot2)


# 2 - Importation de données ----
data = as.data.frame(insee::get_insee_idbank("010767678"))
data[,"DATE"] = as.Date(paste(data[,"TIME_PERIOD"], "-01", sep=""))
data= data[, c("DATE", "OBS_VALUE")]

# 3 - Stationnarité de la série de base ----
tseries::adf.test(data[, "OBS_VALUE"]) # p-value = 0.75, pas stationnaire
tseries::kpss.test(data[, "OBS_VALUE"]) # p-value = 0.01, pas stationnaire
tseries::pp.test(data[, "OBS_VALUE"]) # p-value = 0.01, stationnaire

# 4 - Différenciation et stationnarité de la série différenciée ----
data[, "diff1"] <- c(NA, diff(data[, "OBS_VALUE"]))

tseries::adf.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire
tseries::kpss.test(na.omit(data[, "diff1"])) # p-value = 0.1, limite stationnaire
tseries::pp.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire


# 5 - Visualisation -----

#Avant le traitement 
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line(color = 'turquoise') + 
  
  labs(title = "Valeurs observées non traitées de la série indice de la production de bière",
       x = "Date",
       y = "Indice de la production de bière") +
  scale_x_date(date_breaks = "year", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 10))

# Après traitement (passage à la première différence)

ggplot(data, aes(x = DATE, y = diff1)) +
  geom_line(color = 'orange') + 
  
  labs(title = "Valeurs observées stationnarisées de la série indice de la production de bière",
       x = "Date",
       y = "Indice de la production de bière") +
  scale_x_date(date_breaks = "year", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 10))

