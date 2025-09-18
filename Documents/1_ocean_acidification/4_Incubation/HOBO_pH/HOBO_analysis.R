rm(list=ls(all=TRUE))

#### Libraries ----
library(readxl)
library(dplyr)
library(ggplot2)

zscore <- function(x) {abs(x-mean(x))/sd(x)}


#### Data ----
light_low_1 <-read_excel("17_09_2025_HOBOpH_3_light_low(Data CEST).xlsx") %>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
  mutate(site = "low",
         treatment = "light",
         chamber = "A")

light_low_2 <-read_excel("17_09_2025_HOBOpH_5_light_low(Data CEST).xlsx") %>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
  mutate(site = "low",
         treatment = "light",
         chamber = "B")

dark_low_1 <-read_excel("22068748 2025-09-17-hobo7_low_dark.xlsx")%>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
   mutate(site = "low",
          treatment = "dark",
          chamber = "A")


light_amb_1 <-read_excel("22068745 2025-09-17 08_42_53_hobo6_inc3_amb_light.xlsx") %>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
  mutate(site = "ambient",
         treatment = "light",
         chamber = "A")

light_amb_2 <-read_excel("22068746 2025-09-17 14_51_08_hobo2_inc1_amb_light.xlsx") %>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
  mutate(site = "ambient",
         treatment = "light",
         chamber = "B")

dark_amb_1 <-read_excel("22068749 2025-09-17-hobo4_AMB_DARK.xlsx") %>%
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2) %>%
  na.omit() %>%
  mutate(site = "ambient",
         treatment = "dark",
         chamber = "A")

### Joining the data in one dataframe
pH_incubation <- light_low_1 %>%
  full_join(light_low_2, by=c("Date", "Tin", "mV", "pHnbs", "site", "treatment", "chamber")) %>%
  full_join(dark_low_1, by=c("Date", "Tin", "mV", "pHnbs", "site", "treatment", "chamber")) %>%
  full_join(light_amb_1, by=c("Date", "Tin", "mV", "pHnbs", "site", "treatment", "chamber")) %>%
  full_join(light_amb_2, by=c("Date", "Tin", "mV", "pHnbs", "site", "treatment", "chamber")) %>%
  full_join(dark_amb_1, by=c("Date", "Tin", "mV", "pHnbs", "site", "treatment", "chamber")) %>%
  group_by(Date, site, treatment) %>%
  summarize(pHnbs = mean(pHnbs),
            Tin = mean(Tin)) %>%
  #mutate(id = paste(site, "_", treatment)) %>%
  mutate(Date=as.POSIXct(Date, format="%Y-%m-%d %H:%M")) %>%
  filter(!Date<as.POSIXct("2025-09-17 10:30", tz="UTC")) %>% #removing time before start of incubation
  filter(!Date>as.POSIXct("2025-09-17 12:00", tz="UTC")) %>% #removing time after incubation
  #filter(!Date<as.POSIXct("2025-09-17 08:30", tz="UTC")) %>% #removing time before start of incubation
  #filter(!Date>as.POSIXct("2025-09-17 10:00", tz="UTC")) %>% #removing time after incubation
  group_by(site, treatment)%>%
  mutate(z_pH = zscore(pHnbs)) %>%
  filter(!z_pH>3) %>%
  ungroup() %>%
  na.omit()


#### Plots ----
png(file="timeseries_pH_incubations.png", , width=8, height=5, units="in", res=300)

ggplot(data = pH_incubation) +
  geom_point(aes(x=Date, y=pHnbs, color=site), size=1)+
  #geom_line(aes(x=Date, y=pHnbs, color=site, linetype=treatment), linewidth=2)+
  geom_smooth(aes(x=Date, y=pHnbs, color=site, linetype=treatment), method="loess", se=F, span=0.7, linewidth=2)+
  theme_bw()

dev.off() 
