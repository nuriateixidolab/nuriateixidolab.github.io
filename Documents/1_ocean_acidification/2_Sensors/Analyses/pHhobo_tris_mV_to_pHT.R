
####### Script to transform pH nbs scale to total scale using TRIS. 

####### pH HOBO MX2501 pH and Temperature logger 
library(seacarb)
library(readxl)
library(dplyr)
library(ggplot2)

####### Equation Dickson, Calculation of pH,  page 106 du document total  (SOP 6a page 5 of 7). Guide to Best Practices for Ocean CO2 measurements


tris<- read_excel("../Data/22068745 2025-09-16-TRIS-6.xlsx", sheet = 1)
field<- read_excel("../Data/22068745 2025-09-16-hobo6.xlsx", sheet = 1)


tris<- read_excel("../Data/22068748 2025-09-16-TRIS-7.xlsx", sheet = 1)
field<- read_excel("../Data/22068748 2025-09-16-hobo7.xlsx", sheet = 1)

### rename columns tris

tris <- tris %>% 
  select(4, 5)%>%
  rename(
    TTris = 1,
    mVTris = 2)

field <- field %>% 
  select(2,3,4,5)%>%
  rename(
    Date= 1,
    Tin = 3,
    mV = 4,
    pHnbs= 2)

field$pHtotal <- NA # add new column pHtotal scale


###Tris values at different temperature

# Fit linear model
mVTris_t <- lm(mVTris ~ TTris, data = tris)

# Plot TTris vs mVTris
plot(tris$TTris, tris$mVTris, xlab = "Temperature (°C)", ylab = "Millivolts (mV)", main = "TTris vs mVTris")

# Add regression line
abline(mVTris_t, col = "red")
summary(mVTris_t)

####pH
R <- 8.31447215      # Gas constant (J·mol⁻¹·K⁻¹)
F <- 96485.339924      # Faraday constant (C·mol⁻¹)
E0 <- 0         # Reference potential (mV) - adjust based on your calibration
STris<-35

# mvTris Calculation, calculate mvTris based on the coefficients from  linear regression model mVTris_t

mvTris= field$Tin*mVTris_t$coefficients[2]+mVTris_t$coefficients[1]
  
#phTris Calculation, This equation relate  temperature insitu (Tin) and salinity tris (STris), providing the pH for a given temperature.
  

phTris= (11911.08-18.2499*STris-0.039336*STris^2)*(1/(field$Tin +273.15))-366.27059+ 0.53993607*STris+0.00016329*STris^2+(64.52243-0.084041*STris)*log(field$Tin +273.15)-0.11149858*(field$Tin +273.15)

#pHT Calculation: The final pH calculation (pHT) includes the temperature-dependent phTris value and the contribution from mvTris.

pHT=phTris+(mvTris/1000-field$mV/1000)/(R*(field$Tin +273.15)*log(10)/F)

# Add the pHT column to the field dataframe
field$pHtotal <- pHT

# plot nbs and total scale
plot1 <- ggplot(data = field, aes(x = Date)) +
  geom_line(aes(y = pHtotal, color = "pH total"), linewidth = 1) +
  geom_line(aes(y = pHnbs, color = "pH NBS"), linewidth = 1) +
  scale_color_manual(values = c("pH total" = "blue", "pH NBS" = "orange")) +
  labs(y = "pH", color = "Scale") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_blank()
  )

plot1
