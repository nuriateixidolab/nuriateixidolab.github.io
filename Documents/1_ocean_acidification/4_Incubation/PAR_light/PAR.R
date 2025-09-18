
library (ggplot2)

par1 <- read.csv("13341.TXT", header=TRUE, sep = ',')

par1_filt <- par1 %>%
  filter(Time >= as.POSIXct("2025-09-17 10:28:00") &
           Time <= as.POSIXct("2025-09-17 11:47:00"))

ggplot(par1, aes(x = Time, y = CALIBRATED.VALUE)) +
  geom_line() +             
  geom_point() +              
  labs(title = "PAR (13341)",
       x = "Time", 
       y = "PAR") +
  theme_bw()