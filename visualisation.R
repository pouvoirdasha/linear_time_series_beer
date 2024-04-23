library(ggplot)
# 3 - Visualisation -----
ggplot(data, aes(x = TIME_PERIOD)) +
  geom_line(aes(y = OBS_VALUE, color = "pink")) + 
   
  labs(title = "Valeurs observées de la série indice de la production de bière",
       x = "Date",
       y = "Indice de la production de bière",
       color = "Légende") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10))
 
