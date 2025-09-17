## This code reads in the PAR data from the experiment ran un 17th sept. 2025 in Euromarine Summer School
## Each group needs to change the start and end values, to fit to their incubation times. 

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df1 <- read.csv("Documents/1_ocean_acidification/4_Incubation/PAR_light/13341.TXT")
df2 <- read.csv("Documents/1_ocean_acidification/4_Incubation/PAR_light/13349.TXT")
df3 <- read.csv("Documents/1_ocean_acidification/4_Incubation/PAR_light/13350.TXT")




### Preparation of data ###

# Convert Time to proper time format
df1$Time <- as.POSIXct(df1$Time, format = "%H:%M", tz = "UTC")
df2$Time <- as.POSIXct(df2$Time, format = "%H:%M", tz = "UTC")
df3$Time <- as.POSIXct(df3$Time, format = "%H:%M", tz = "UTC")

### EACH GROUP CHANGES THIS VALUE FOR THEIR EXPERIMENT ###

# Filter for times between 09:10 and 12:10
start <- as.POSIXct("09:10", format = "%H:%M", tz = "UTC")
end   <- as.POSIXct("12:10", format = "%H:%M", tz = "UTC")

df1 <- df1 %>% filter(Time >= start & Time <= end)
df2 <- df2 %>% filter(Time >= start & Time <= end)
df3 <- df3 %>% filter(Time >= start & Time <= end)

# Rename calibrated value columns
df1 <- df1 %>% rename(Value1 = `CALIBRATED.VALUE`)
df2 <- df2 %>% rename(Value2 = `CALIBRATED.VALUE`)
df3 <- df3 %>% rename(Value3 = `CALIBRATED.VALUE`)

# Merge on Time
merged <- df1 %>%
  inner_join(df2, by = "Time") %>%
  inner_join(df3, by = "Time")


# Compute average at each time
merged <- merged %>%
  mutate(Average = rowMeans(select(., Value1, Value2, Value3), na.rm = TRUE))

# Plot average over time
ggplot(merged, aes(x = Time, y = Average)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Average Calibrated Value (09:10–12:10)",
       x = "Time",
       y = "Calibrated Value") +
  theme_minimal()


# 1) Single overall average across the whole period (a single number)
overall_avg <- merged %>% summarize(overall_average = mean(Average, na.rm = TRUE)) %>% pull(overall_average)
overall_avg
# (prints the number)



# We already have merged with: Time, Value1, Value2, Value3, Average

# Reshape to long format for plotting
plotdata <- merged %>%
  select(Time, Value1, Value2, Value3, Average) %>%
  pivot_longer(cols = -Time, names_to = "Series", values_to = "Calibrated")

# Plot
ggplot(plotdata, aes(x = Time, y = Calibrated, color = Series)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Value1" = "blue", "Value2" = "green", "Value3" = "orange", "Average" = "red")
  ) +
  labs(
    title = "Calibrated Values with Average (09:10–12:10)",
    x = "Time",
    y = "Calibrated Value",
    color = "Series"
  ) +
  theme_minimal()



