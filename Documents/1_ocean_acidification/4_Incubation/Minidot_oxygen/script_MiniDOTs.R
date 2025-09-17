# LOW PH AND LIGHT INCUBATION CHAMBER
# Alejandro, Holly and Pol

library (ggplot2)
library(dplyr)
library(readxl)


setwd("C:/Github/nuriateixidolab.github.io/Documents/1_ocean_acidification/4_Incubation/Minidot_oxygen")


# MiniDOT 6:
miniDOT_6 <- read.table("17_09_2025_miniDOT_6_light_low.txt", 
                     sep = ",",
                     header = TRUE)

miniDOT_6$Time..sec.<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_6$Time..sec.

miniDOT_6_filt <- miniDOT_6 %>%
  filter(Time..sec. >= as.POSIXct("2025-09-17 10:32:00") &
           Time..sec. <= as.POSIXct("2025-09-17 11:52:00"))

miniDOT_6_filt <- miniDOT_6_filt %>%
  mutate(Time..sec. = Time..sec. - lubridate::hours(2))

ggplot(miniDOT_6_filt, aes(x = Time..sec., y = DO..mg.l.)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Low pH + LIGHT (MiniDOT 6)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()





# MiniDOT 7: 
miniDOT_7 <- read.table("17_09_2025_miniDOT_7_light_low.txt", 
                        sep = ",",
                        header = TRUE)

miniDOT_7$Time..sec.<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_7$Time..sec.

miniDOT_7_filt <- miniDOT_7 %>%
  filter(Time..sec. >= as.POSIXct("2025-09-17 10:29:00") &
           Time..sec. <= as.POSIXct("2025-09-17 11:55:00"))

miniDOT_7_filt <- miniDOT_7_filt %>%
  mutate(Time..sec. = Time..sec. - lubridate::hours(2))

ggplot(miniDOT_7_filt, aes(x = Time..sec., y = DO..mg.l.)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Low pH + LIGHT (MiniDOT 7)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()

m_miniDOT_7 <- lm(DO..mg.l. ~ Time..sec., data = miniDOT_7_filt)
summary(m_miniDOT_7)








# MiniDOT 3:
miniDOT_3 <- read.table("minidot3_2025-09-17 072300Z.txt", 
                        sep = ",",
                        header = TRUE)

miniDOT_3$Time..sec.<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_3$Time..sec.

miniDOT_3_filt <- miniDOT_3 %>%
  filter(Time..sec. >= as.POSIXct("2025-09-17 10:28:00") &
           Time..sec. <= as.POSIXct("2025-09-17 11:50:00"))

miniDOT_3_filt <- miniDOT_3_filt %>%
  mutate(Time..sec. = Time..sec. - lubridate::hours(2))


ggplot(miniDOT_3_filt, aes(x = Time..sec., y = DO..mg.l.)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Low pH + DARK (MiniDOT 3)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()


# MiniDOT 8:
miniDOT_8 <- read.table("minidot8_2025-09-17 072300Z.txt", 
                        sep = ",",
                        header = TRUE)

miniDOT_8$Time..sec.<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_8$Time..sec.

miniDOT_8_filt <- miniDOT_8 %>%
  filter(Time..sec. >= as.POSIXct("2025-09-17 10:28:00") &
           Time..sec. <= as.POSIXct("2025-09-17 11:47:00"))

miniDOT_8_filt <- miniDOT_8_filt %>%
  mutate(Time..sec. = Time..sec. - lubridate::hours(2))

ggplot(miniDOT_8_filt, aes(x = Time..sec., y = DO..mg.l.)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Low pH + DARK (MiniDOT 8)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()



# MiniDOT 1:
miniDOT_1 <- read_excel("minidot1_AMB_DARK.xlsx")

miniDOT_1_filt <- miniDOT_1 %>%
  filter(`UTC_Date_&_Time` >= as.POSIXct("2025-09-17 08:27:00") &
           `UTC_Date_&_Time` <= as.POSIXct("2025-09-17 10:02:00"))


miniDOT_1_filt$`Dissolved Oxygen` <- as.numeric(miniDOT_1_filt$`Dissolved Oxygen`)
miniDOT_1_filt$`UTC_Date_&_Time` <- as.POSIXct(miniDOT_1_filt$`UTC_Date_&_Time`, format = "%Y-%m-%d %H:%M:%S")

ggplot(miniDOT_1_filt, aes(x = `UTC_Date_&_Time`, y = `Dissolved Oxygen`)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + DARK (MiniDOT 1)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()




# MiniDOT 4:
miniDOT_4 <- read_excel("minidot4_AMB_DARK.xlsx")

miniDOT_4_filt <- miniDOT_4 %>%
  filter(`UTC_Date_&_Time` >= as.POSIXct("2025-09-17 08:28:00") &
           `UTC_Date_&_Time` <= as.POSIXct("2025-09-17 10:04:00"))


miniDOT_4_filt$`Dissolved Oxygen` <- as.numeric(miniDOT_4_filt$`Dissolved Oxygen`)
miniDOT_4_filt$`UTC_Date_&_Time` <- as.POSIXct(miniDOT_4_filt$`UTC_Date_&_Time`, format = "%Y-%m-%d %H:%M:%S")

ggplot(miniDOT_4_filt, aes(x = `UTC_Date_&_Time`, y = `Dissolved Oxygen`)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + DARK (MiniDOT 4)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()




# MiniDOT 2:
miniDOT_2 <- read_excel("2025-09-17 072300Z_AMB_LIGHT_sensor2_.xlsx")

miniDOT_2_filt <- miniDOT_2 %>%
  filter(Time >= as.POSIXct("2025-09-17 08:28:00") &
           Time <= as.POSIXct("2025-09-17 9:58:00"))


miniDOT_2_filt$`DO (mg/l)` <- as.numeric(miniDOT_2_filt$`DO (mg/l)`)
miniDOT_2_filt$Time <- as.POSIXct(miniDOT_2_filt$Time, format = "%Y-%m-%d %H:%M:%S")

ggplot(miniDOT_2_filt, aes(x = Time, y = `DO (mg/l)`)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + LIGHT (MiniDOT 2)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()


# MiniDOT 5:
miniDOT_5 <- read_excel("2025-09-17 072400Z_AMB_LIGHT_sensor5_.xlsx")

miniDOT_5_filt <- miniDOT_5 %>%
  filter(Time >= as.POSIXct("2025-09-17 08:27:00") &
           Time <= as.POSIXct("2025-09-17 10:01:00"))


miniDOT_5_filt$`DO (mg/l)` <- as.numeric(miniDOT_5_filt$`DO (mg/l)`)
miniDOT_5_filt$Time <- as.POSIXct(miniDOT_5_filt$Time, format = "%Y-%m-%d %H:%M:%S")

ggplot(miniDOT_5_filt, aes(x = Time, y = `DO (mg/l)`)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + LIGHT (MiniDOT 5)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()

