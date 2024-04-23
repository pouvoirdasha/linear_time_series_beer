library(insee)

data = as.data.frame(insee::get_insee_idbank("010767678"))

data= data[, c("TIME_PERIOD", "OBS_VALUE")]
