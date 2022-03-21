# Coeficiente de correlación 
library(ggplot2)

llamadas <- c(96, 40, 104, 128, 164,  
              76, 72, 80 , 36, 84,  
              180, 132, 120, 44, 84) 
ventas <- c(41, 41, 51, 60, 61, 
            29, 39, 50, 28, 43, 
            70, 56, 45, 31, 30)

ggplot() +
  geom_point(aes(x = llamadas, y=ventas), col='red') +
  geom_vline(xintercept = mean(llamadas), col='blue') +
  geom_hline(yintercept = mean(ventas), col='blue')+
  ggtitle(label = "Dispersión Llamadas y ventas", 
          subtitle = paste("Media llamadas=", mean(llamadas), " ; ", "Media ventas", mean(ventas)))


