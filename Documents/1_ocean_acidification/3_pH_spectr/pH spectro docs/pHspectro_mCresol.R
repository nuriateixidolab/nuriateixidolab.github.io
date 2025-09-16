#Spectrophotometric pH measurement

#Dickson et al., 2007. SOP 6b
#Samir Alliouane, December 2011.
#Aide Tatiana Donnay Fevrier 2012.

#################################################################
# Extrction des données de Densité optique pour nm434, nm578 et nm730 avec moyenne
# de 433.5 ?  434.5 etc..
# Lecture des fichiers .txt (resultats du spectro) dans le dossier "Abs_extraction" et 
# création des tableaus de données dans le dossier "abs_results"
#################################################################
library(seacarb)
library(xlsx)

folder <- "C:/Users/pH/Desktop/pH_extraction" 
setwd(folder)

t<-list.files() 
liste_fichier<-NULL 
fichier<-t 
liste_fichier<-c(liste_fichier,fichier)

DO_data <- data.frame(nm434=NA, nm578=NA, nm730=NA)

folder <- "C:/Users/pH/Desktop/pH_results" 
setwd(folder)

#Initiliation the parameters
#write.table(DO_data, file="SW.csv",append=F, sep=",", row.names=F)
#write.table(DO_data, file="SWDye.csv",append=F, sep=",", row.names=F)

num_sw <- 1
num_swdye <- 2

for(num_file in 1:length(liste_fichier))
  #for(num_file in 1:1)
{
  folder <- "C:/Users/pH/Desktop/pH_extraction" 
  setwd(folder)
  Data_file <- read.csv(liste_fichier[num_file], dec=",", sep="\t", na.strings="na",skip=17, comment.char = ">", head=F)
  colnames(Data_file) <- c("nm","DO")
  Data_file$DO <- sub(",",".",Data_file$DO)
  Data_file$DO <- as.numeric(Data_file$DO)
  a434 <- subset(Data_file, nm >= 433.5 & nm <= 434.55 ) #Measured Absorbance at 434 nm (seawater only)
  a578 <- subset(Data_file, nm >= 577.5 & nm <= 578.55 )  #Measured Absorbance at 578 nm (seawater only)
  a730 <- subset(Data_file, nm >= 729.5 & nm <= 730.55 ) #Measured Absorbance at 730 nm (seawater only)
  
  DO_data$nm434 <-mean(a434$DO) 
  DO_data$nm578 <-mean(a578$DO)
  DO_data$nm730 <-mean(a730$DO)
  
  folder <- "C:/Users/pH/Desktop/pH_results" 
  setwd(folder)
  if (num_file==1)  write.table(DO_data, file="SW.csv",append=F, sep=",", col.names=c("nm434","nm578","nm730"), row.names=F)
  if (num_file==num_sw) { 
    write.table(DO_data, file="SW.csv",append=T, sep=",", col.names=F, row.names=F)
    num_sw <- num_sw + 2
  }
  if (num_file==2)  write.table(DO_data, file="SWDye.csv",append=F, sep=",", col.names=c("nm434","nm578","nm730"), row.names=F)
  
  if (num_file==num_swdye) { 
    write.table(DO_data, file="SWDye.csv",append=T, sep=",", col.names=F, row.names=F)
    num_swdye <- num_swdye + 2
  }
}

##################################################################
## Values to enter : S, t, V, A, B (voir dessous pour les legendes)

S <- 38  				#Salinity
t <- 22.6						#Temperature in Degres Celsius
T <- t + 273.15  	#Temperature in Kalvin

#################################################################
# Export des resultats et calcul
#################################################################

pK2 <- (1245.69/T)+3.8275+0.00211*(35-S)  #Acid dissociation constant for the species HI-

e1 <- 0.00691					#e1(HI-)/e2(HI-) extinction coefficient ratio for mcresol purple
e2 <- 2.2220					#e1(I2-)/e2(HI-)     "          "         "         "         "
e3 <- 0.1331					#e2(I2-)/e2(HI-)     "          "         "         "         "


folder <- "C:/Users/pH/Desktop/pH_results" 
setwd(folder)
tab_SW <- read.csv("SW.csv", dec=".", sep=",", na.strings="na", head=T)
tab_SWDye <- read.csv("SWDye.csv", dec=".", sep=",", na.strings="na", head=T)
str(tab_SWDye)

Rabs <- ((tab_SWDye$nm578 - tab_SW$nm578 - (tab_SWDye$nm730 - tab_SW$nm730))/(tab_SWDye$nm434 - tab_SW$nm434 -(tab_SWDye$nm730-tab_SW$nm730))) #Measured absorbance ratio (without correction)

V <- 0.05			#Volume of dye added in ml
A <- 0.0998021529651323 #Slope. Constant extracted from the linear regression of 10 unknown pHs after a 2nd addition of dye.
B <- 	-0.0901798177776986		#intercept. Constant extracted from the linear regression of 10 unknown pHs after a 2nd addition of dye.
Rcorr <- Rabs-V*(B+A*(Rabs))		#Corrected absorbance ratio

pH <- pK2 + log10((Rcorr-e1)/(e2-Rcorr*e3))

results <- data.frame(tab_SW$nm578, tab_SW$nm434, tab_SW$nm730, tab_SWDye$nm578,tab_SWDye$nm434,tab_SWDye$nm730,pK2, pH)
colnames(results) <- c("sw578","sw434","sw730","swDye578","swDye434","swDye730", "pK2", "pHT")


write.xlsx(results[-1,], "C:/Users/pH/Desktop/pH_results/Results_pH.xlsx") 
#write.table(results[-1,], "C:/Users/pH/Dropbox/Spectrophotometric pH/pH_results/Results_pH.csv", dec=",", sep="\t",row.names=F)
#################################################################
#END OF THE SCRIPT
#################################################################


#tris(35,19.7)
#carb(flag=15, var1=0.00257071, var2=0.00232855, S=38, T=25, P=0, Pt=0, Sit=0, pHscale="T", kf="pf", k1k2="l", ks="d", b="l10")
pHinsi(pH=c(
  7.9939,
  7.9809,
  7.9872,
  7.9916,
  7.9830
), ALK=2.560e-3, Tinsi= 13.4, Tlab=c(
  19.7,
  19.6,
  19.6,
  19.6,
  19.6
  ) ,S=38.008, Pt=0, Sit=0, k1k2 = "x", kf = "x", ks="d", pHscale = "T", b="l10")
