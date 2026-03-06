library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(corrplot)

wtm1 <- readRDS("Wnd_Tmp1993-2025.rds")

# remove and rename variables 
wtm <- wtm1[,-c(6:11)]
wtm <- wtm %>% dplyr::rename("temperature1000"="air1000","temperature925"="air925","temperature850"="air850")
wtm <- wtm %>% filter(YEAR != 2025)
# CALCULATE SEASONAL ANOMALIES FROM CLIMATE NORMAL ####
# Assessing interannual variability of temperature and tailwind within a consistent climatological context
# 1.Compute seasonal mean (March-May) for each variable and year 
seasonal_means <- wtm %>%
  group_by(YEAR) %>%
  summarise(
    mean_temperature1000 = mean(temperature1000, na.rm = TRUE),
    mean_temperature925 = mean(temperature925, na.rm = TRUE),
    mean_temperature850 = mean(temperature850, na.rm = TRUE),
    mean_tailwind1000 = mean(tailwind1000, na.rm = TRUE),
    mean_tailwind925 = mean(tailwind925, na.rm = TRUE),
    mean_tailwind850 = mean(tailwind850, na.rm = TRUE))
# 2.Compute the 30-year climate means, that is the total mean from 1993 to 2023, plus the std deviation 
climate_means <- seasonal_means %>%
filter(YEAR >= 1993 & YEAR <= 2023) %>%
  summarise(
    climate_temperature1000 = mean(mean_temperature1000, na.rm = TRUE),
    climate_temperature925 = mean(mean_temperature925, na.rm = TRUE),
    climate_temperature850 = mean(mean_temperature850, na.rm = TRUE),
    climate_tailwind1000 = mean(mean_tailwind1000, na.rm = TRUE),
    climate_tailwind925 = mean(mean_tailwind925, na.rm = TRUE),
    climate_tailwind850 = mean(mean_tailwind850, na.rm = TRUE),
    sd_temperature1000 = sd(mean_temperature1000, na.rm = TRUE),
    sd_temperature925 = sd(mean_temperature925, na.rm = TRUE),
    sd_temperature850 = sd(mean_temperature850, na.rm = TRUE),
    sd_tailwind1000 = sd(mean_tailwind1000, na.rm = TRUE),
    sd_tailwind925 = sd(mean_tailwind925, na.rm = TRUE),
    sd_tailwind850 = sd(mean_tailwind850, na.rm = TRUE))
# 3.Compute the anomalies for both variables by subtracting the CN from each year’s seasonal mean
climate_data <- seasonal_means %>%
  mutate(
    anom_temperature1000 = mean_temperature1000 - pull(climate_means, climate_temperature1000),
    anom_temperature925 = mean_temperature925 - pull(climate_means, climate_temperature925),
    anom_temperature850 = mean_temperature850 - pull(climate_means, climate_temperature850),
    anom_tailwind1000 = mean_tailwind1000 - pull(climate_means, climate_tailwind1000),
    anom_tailwind925 = mean_tailwind925 - pull(climate_means, climate_tailwind925),
    anom_tailwind850 = mean_tailwind850 - pull(climate_means, climate_tailwind850))

# save the dataset as an r object and as an excel file

# Calculate seasonal trend (March-May) of monthly NAO from 2007 to 2024 ####
mnao <- read_excel("/MonthlyNAO-2025.xlsx") 
# 1.Compute seasonal mean (March-May) for each year 
seasonal_NAOmeans <- mnao %>%
  group_by(YEAR) %>%
  summarise(seasonal_nao = mean(Monthly_NAO,na.rm = TRUE))
# Combine anomalies of weather data with seasonal NAO data ####
swtmNAO <- left_join(climate_data,seasonal_NAOmeans,by="YEAR")
# subset 2007-2024
swtmNAO <- swtmNAO[swtmNAO$YEAR >= 2007, ]
# save and export 
save(swtmNAO, file = "/swtmNAO2024.rds")

# Combine anomalies of weather data with ringing data 
load("P24Species.rds")
pwtm <- left_join(p24species,swtmNAO,by="YEAR")
write_xlsx(pwtm,"/RingingWTN2024.xlsx") 
#*************************************************************************************************************#
# Correlation Matrix Monthly Weather ##
# create a numeric dataframe with only weather data for the three pressure levels
swtmNAO1 <- subset(swtmNAO[,-c(1:7)])
swtmNAO1 <- dplyr::rename(swtmNAO1,"anomalies_tailwind1000"="anom_tailwind1000",
                          "anomalies_tailwind925"="anom_tailwind925",
                          "anomalies_tailwind850"="anom_tailwind850",
                          "anomalies_temperature1000" = "anom_temperature1000",
                          "anomalies_temperature925"="anom_temperature925",
                          "anomalies_temperature850" = "anom_temperature850",
                          "seasonal_NAO"="seasonal_nao")
matrixweather <- cor(swtmNAO1)
png(file = "matrixweather.png",width = 3000,height = 2200,                           
    units = "px",pointsize = 15,bg = "white",res = 300)
corrplot(matrixweather, tl.col = "black", method = "number",
         sig.level=0.05, insig = 'p-value', tl.cex=1)
dev.off()
# Combine all data at daily level (weather + NAO) + ponza data for all years ####
dnao <- read.table("/NAO.csv",
                   header=TRUE, sep = ",", fill = TRUE)

# daily weather data (00-06) from 1993 to 2024
# format DATE
p24species$DATE <- as.Date(paste(YEAR, MONTH, DAY))
dnao$DATE <- as.Date(dnao$DATE, format = "%d/%m/%Y")
wtm$DATE <- as.Date(wtm$DATE, format = "%d/%m/%Y")
# format dnao 
dnao$DAY <- as.numeric(dnao$DAY)
dnao$MONTH <- as.numeric(dnao$MONTH)
dnao$YEAR <- as.numeric(dnao$YEAR)
# subset 2007-2024
wtm <- wtm[wtm$YEAR >= 2007, ]
dnao <- dnao[dnao$YEAR >= 2007,]
# combine weather + NAO data 
wtmNAO <- left_join(wtm,dnao, by = "DATE")
# reorder and rename the variables 
wtmNAO <- wtmNAO[,-c(12:14)]
wtmNAO <- dplyr::rename(wtmNAO,"DAY"="DAY.x","MONTH"="MONTH.x","YEAR"="YEAR.x")
# save and export 
# combine weather and NAO + ponza data
ponzawtm <- left_join(p24species,wtmNAO,by = "DATE")
# reorder and rename the variables 
ponzawtm <- ponzawtm[,-c(19:22)]
ponzawtm <- dplyr::rename(ponzawtm,"JDays"="JDays.x","DAY"="DAY.x",
                          "MONTH"="MONTH.x","YEAR"="YEAR.x")
# check and remove NA values from the dataset
colSums(is.na(wtmNAO))
colSums(is.na(ponzawtm))
ponzawtm <- ponzawtm[!is.na(ponzawtm$Locality), ] # the other NA values are for physiological variables
# save the dataset all ponza and meteo data as a csv file
write.csv(ponzawtm,"PonzaWTM-NAO2007-2024.csv")
# then subset ponza+weather+NAO data by excluding the environmental variables 
ponza = ponzawtm[,-c(19:25)]
# save and export

#*************************************************************************************************************#
# Correlation Matrix Daily Weather ##
# create a numeric dataframe with only weather data for the three pressure levels
wtmNAO1 <- subset(wtmNAO[,-c(1:5)])
matrixweather <- cor(wtmNAO1)
pdf("corrplot.pdf", width = 11, height = 8.5, bg = "white")
corrplot(matrixweather, 
         method = "number", 
         sig.level = 0.05, 
         tl.cex = 1.5,        # Larger text
         tl.col = "black",    # Text label color
         number.cex = 1.2)
dev.off()
# Plot anomalies across years ####
library(esquisse)
esquisser(swtmNAO)
# tailwind 1000
png("AnomaliesTailwind1000.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
AnomaliesTailwind1000 <- swtmNAO$anom_tailwind1000
plot(years, AnomaliesTailwind1000, type = "b",
     xlab = "Years", ylab = "Anomalies of tailwind at 1000 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
# temperature 1000
png("AnomaliesTemperature1000.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
AnomaliesTemperature1000 <- swtmNAO$anom_temperature1000
plot(years, AnomaliesTemperature1000, type = "b",
     xlab = "Years", ylab = "Anomalies of temperature at 1000 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
# tailwind 925
png("AnomaliesTailwind925.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
AnomaliesTailwind925 <- swtmNAO$anom_tailwind925
plot(years, AnomaliesTailwind925, type = "b",
     xlab = "Years", ylab = "Anomalies of tailwind at 925 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
# temperature 925
png("AnomaliesTemperature925.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(7, 6, 6, 2) + 0.1)
years <- 2007:2024
AnomaliesTemperature925 <- swtmNAO$anom_temperature925
plot(years, AnomaliesTemperature925, type = "b",
     xlab = "Years", ylab = "Anomalies of temperature at 925 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
# tailwind 850
png("AnomaliesTailwind850.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 6, 2) + 0.1)
years <- 2007:2024
AnomaliesTailwind850 <- swtmNAO$anom_tailwind850
plot(years, AnomaliesTailwind850, type = "b",
     xlab = "Years", ylab = "Anomalies of tailwind at 850 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
# temperature 850
png("AnomaliesTemperature850.png", width = 3000, height = 2000, units = "px", res = 300)
par(mar = c(6, 6, 4, 2) + 0.1)
years <- 2007:2024
AnomaliesTemperature850 <- swtmNAO$anom_temperature850
plot(years, AnomaliesTemperature850, type = "b",
     xlab = "Years", ylab = "Anomalies of temperature at 850 mb",
     cex.axis = 1.3,     
     cex.lab = 1.83,     
     cex.main = 2.5,
     pch = 19,
     xlim = c(2007, 2024),
     xaxt = "n")    
axis(1, at = years, labels = years, cex.axis = 1.3)
dev.off()
