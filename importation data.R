# 1 - Libraries ----
library(insee)
library(tseries)
library(ggplot2)


# 2 - Import of data ----
data = as.data.frame(insee::get_insee_idbank("010767678"))
data[,"DATE"] = as.Date(paste(data[,"TIME_PERIOD"], "-01", sep=""))
data= data[, c("DATE", "OBS_VALUE")]

# 3 - Stationnarity tests for basic series -----
tseries::adf.test(data[, "OBS_VALUE"]) # p-value = 0.75, pas stationnaire
tseries::kpss.test(data[, "OBS_VALUE"]) # p-value = 0.01, pas stationnaire
tseries::pp.test(data[, "OBS_VALUE"]) # p-value = 0.01, stationnaire


# 4 - Stationnarity for the differentiated series ----
data[, "diff1"] <- c(NA, diff(data[, "OBS_VALUE"]))

tseries::adf.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire
tseries::kpss.test(na.omit(data[, "diff1"])) # p-value = 0.1, limite stationnaire
tseries::pp.test(na.omit(data[, "diff1"])) # p-value = 0.01, stationnaire


# 5 - Visualization -----

#Before treating the series
ggplot(data, aes(x = DATE, y = OBS_VALUE)) +
  geom_line(color = 'turquoise') + 
  
  labs(title = "Before transformation - index of industrial production of beer manufacturing, France",
       x = "Date",
       y = "Index of industrial production of beer manufacturing") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 8))

# After 1-st order differentiation 

ggplot(data, aes(x = DATE, y = diff1)) +
  geom_line(color = 'orange') + 
  
  labs(title = "After transformation - index of industrial production of beer manufacturing, France",
       x = "Date",
       y = "Index of industrial production of beer manufacturing") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size=7, angle=90), 
        axis.title.y = element_text(size = 8))


#6 - Picking ARMA 



