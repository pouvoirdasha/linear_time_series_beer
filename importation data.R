# 1 - Libraries ----
library(insee)
library(tseries)


# 2 - Importation de données ----
data = as.data.frame(insee::get_insee_idbank("010767678"))
data= data[, c("TIME_PERIOD", "OBS_VALUE")]

# 3 - Stationnarité de la série de base ----
tseries::adf.test(data[, "OBS_VALUE"]) # p-value = 0.75, pas stationnaire
tseries::kpss.test(data[, "OBS_VALUE"]) # p-value = 0.01, pas stationnaire
tseries::pp.test(data[, "OBS_VALUE"]) # p-value = 0.01, stationnaire

# 4 - Différenciation et stationnarité de la série différenciée ----
data[, "diff1"] <- c(NA, diff(data[, "OBS_VALUE"]))

tseries::adf.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire
tseries::kpss.test(na.omit(data[, "diff1"])) # p-value = 0.1, limite stationnaire
tseries::pp.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire


