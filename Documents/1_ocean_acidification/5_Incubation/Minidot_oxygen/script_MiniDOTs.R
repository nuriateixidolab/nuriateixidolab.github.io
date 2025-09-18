# LOW PH AND LIGHT INCUBATION CHAMBER
# Alejandro, Holly and Pol

library (ggplot2)
library(dplyr)
library(readxl)
library(purrr)
library(cowplot)

setwd("C:/Github/nuriateixidolab.github.io/Documents/1_ocean_acidification/4_Incubation/Minidot_oxygen")


# MiniDOT 6:
miniDOT_6 <- read.table("17_09_2025_miniDOT_6_light_low.txt", 
                     sep = ",",
                     header = TRUE)

miniDOT_6$Time<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_6$Time

miniDOT_6_filt <- miniDOT_6 %>%
  filter(Time >= as.POSIXct("2025-09-17 10:32:00") &
           Time <= as.POSIXct("2025-09-17 11:52:00"))

# convert to UTC time (it was in Local time)
miniDOT_6_filt <- miniDOT_6_filt %>%
  mutate(Time = Time - lubridate::hours(2))

miniDOT_6_filt <- miniDOT_6_filt %>%
  # renombrar columnas
  rename(
    Time = Time,
    DO   = DO..mg.l.,
    T = T..deg.C.
  ) %>%
  select(Time, DO, T)

low_light <- ggplot(miniDOT_6_filt, aes(x = Time, y = DO)) +
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

miniDOT_7$Time<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_7$Time

miniDOT_7_filt <- miniDOT_7 %>%
  filter(Time >= as.POSIXct("2025-09-17 10:29:00") &
           Time <= as.POSIXct("2025-09-17 11:55:00"))

# convert to UTC time (it was in Local time)
miniDOT_7_filt <- miniDOT_7_filt %>%
  mutate(Time = Time - lubridate::hours(2))

miniDOT_7_filt <- miniDOT_7_filt %>%
  # renombrar columnas
  rename(
    Time = Time,
    DO   = DO..mg.l.,
    T = T..deg.C.
  ) %>%
  select(Time, DO, T)

ggplot(miniDOT_7_filt, aes(x = Time, y = DO)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Low pH + LIGHT (MiniDOT 7)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()



# MiniDOT 3:
miniDOT_3 <- read.table("minidot3_2025-09-17 072300Z.txt", 
                        sep = ",",
                        header = TRUE)

miniDOT_3$Time<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_3$Time

miniDOT_3_filt <- miniDOT_3 %>%
  filter(Time >= as.POSIXct("2025-09-17 10:28:00") &
           Time <= as.POSIXct("2025-09-17 11:50:00"))

# convert to UTC time (it was in Local time)
miniDOT_3_filt <- miniDOT_3_filt %>%
  mutate(Time = Time - lubridate::hours(2))

miniDOT_3_filt <- miniDOT_3_filt %>%
  # renombrar columnas
  rename(
    Time = Time,
    DO   = DO..mg.l.,
    T = T..deg.C.
  ) %>%
  select(Time, DO, T)

low_dark <- ggplot(miniDOT_3_filt, aes(x = Time, y = DO)) +
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

miniDOT_8$Time<- ISOdatetime(1970,1,1,1,0,0) + miniDOT_8$Time

miniDOT_8_filt <- miniDOT_8 %>%
  filter(Time >= as.POSIXct("2025-09-17 10:28:00") &
           Time <= as.POSIXct("2025-09-17 11:47:00"))


# convert to UTC time (it was in Local time)
miniDOT_8_filt <- miniDOT_8_filt %>%
  mutate(Time = Time - lubridate::hours(2))

miniDOT_8_filt <- miniDOT_8_filt %>%
  # renombrar columnas
  rename(
    Time = Time,
    DO   = DO..mg.l.,
    T = T..deg.C.
  ) %>%
  select(Time, DO, T)

ggplot(miniDOT_8_filt, aes(x = Time, y = DO)) +
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

miniDOT_1_filt <- miniDOT_1_filt %>%
  # renombrar columnas
  rename(
    Time = `UTC_Date_&_Time`,
    DO   = `Dissolved Oxygen`,
    T = Temperature
  ) %>%
  select(Time, DO, T)

ggplot(miniDOT_1_filt, aes(x = Time, y = DO)) +
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

miniDOT_4_filt <- miniDOT_4_filt %>%
  # renombrar columnas
  rename(
    Time = `UTC_Date_&_Time`,
    DO   = `Dissolved Oxygen`,
    T = Temperature
  ) %>%
  select(Time, DO, T)

ambient_dark <- ggplot(miniDOT_4_filt, aes(x = Time, y = DO)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + DARK (MiniDOT 4)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()




# MiniDOT 2:
miniDOT_2 <- read_excel("2025-09-17 072300Z_AMB_LIGHT_sensor2_.xlsx")

miniDOT_2 <- miniDOT_2 %>%
  mutate(Time = as.character(Time))
miniDOT_2$`DO (mg/l)` <- as.numeric(miniDOT_2$`DO (mg/l)`)
miniDOT_2$Time <- as.POSIXct(miniDOT_2$Time, format = "%Y-%m-%d %H:%M:%S")

miniDOT_2 <- miniDOT_2 %>%
  rename(
    Time = Time,
    DO   = `DO (mg/l)`,
    T = `T (deg C)`
  ) %>%
  select(Time, DO, T)

miniDOT_2_filt <- miniDOT_2 %>%
  filter(Time >= as.POSIXct("2025-09-17 08:28:00", format = "%Y-%m-%d %H:%M:%S") &
           Time <= as.POSIXct("2025-09-17 09:58:00", format = "%Y-%m-%d %H:%M:%S"))


ggplot(miniDOT_2_filt, aes(x = Time, y = DO)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + LIGHT (MiniDOT 2)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()


# MiniDOT 5:
miniDOT_5 <- read_excel("2025-09-17 072400Z_AMB_LIGHT_sensor5_.xlsx")

miniDOT_5 <- miniDOT_5 %>%
  mutate(Time = as.character(Time))
miniDOT_5$`DO (mg/l)` <- as.numeric(miniDOT_5$`DO (mg/l)`)
miniDOT_5$Time <- as.POSIXct(miniDOT_5$Time, format = "%Y-%m-%d %H:%M:%S")

miniDOT_5 <- miniDOT_5 %>%
  rename(
    Time = Time,
    DO   = `DO (mg/l)`,
    T = `T (deg C)`
  ) %>%
  select(Time, DO, T)

miniDOT_5_filt <- miniDOT_5 %>%
  filter(Time >= as.POSIXct("2025-09-17 08:27:00", format = "%Y-%m-%d %H:%M:%S") &
           Time <= as.POSIXct("2025-09-17 10:01:00", format = "%Y-%m-%d %H:%M:%S"))


ambient_light <-ggplot(miniDOT_5_filt, aes(x = Time, y = DO)) +
  geom_line() +             
  geom_point() +              
  labs(title = "Ambient pH + LIGHT (MiniDOT 5)",
       x = "Time", 
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw()




# general plot:
plot_grid(low_light, low_dark, ambient_light, ambient_dark, labels = "AUTO", ncol = 2)


# DATAFRAME

Sensor <- list(
  Minidot_1=miniDOT_1_filt,
  Minidot_2=miniDOT_2_filt,
  Minidot_3=miniDOT_3_filt,
  Minidot_4=miniDOT_4_filt,
  Minidot_5=miniDOT_5_filt,
  Minidot_6=miniDOT_6_filt,
  Minidot_7=miniDOT_7_filt,
  Minidot_8=miniDOT_8_filt)

#make colums numeric function

force_numeric <- function(df, col_name) {
  df |>
    mutate(
      !!col_name := as.numeric(.data[[col_name]])
    )
}

# Apply across all datasets, then combine

All_minidots <- Sensor |>
  map(~ force_numeric(.x, "T")) |> 
  map(~ force_numeric(.x, "DO")) |>
  imap_dfr(~ mutate(.x, sensor = .y))


#write.csv(All_minidots, "All_minidots.csv", row.names = FALSE)


