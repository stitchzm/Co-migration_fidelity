# GAM WITH WEATHER + NAO
# import data
load("/swtmNAO2024.rds")
load("/ponza_abundance_list.rds")
load("/Modularity_sma.rds")

library(dplyr)
library(gratia)
library(performance)
library(mgcv)

# Create one dataset
DataGAM = as.data.frame(cbind(Modularity.sma$Q,swtmNAO))
colnames(DataGAM)[1] <- "Q"

# GAM ####
set.seed(123)
gam_model1000 <- gam(Q ~ s(anom_temperature1000, k=3) + 
                       s(anom_tailwind1000, k=3) + 
                       s(seasonal_nao, k=3) +
                       ti(anom_temperature1000, seasonal_nao, k=3) + 
                       ti(anom_tailwind1000, seasonal_nao, k = 3),  
                     family = betar(),  
                     data = DataGAM)
set.seed(123)
gam_model925 <- gam(Q ~ s(anom_temperature925, k=3) + 
                      s(anom_tailwind925, k=3) + 
                      s(seasonal_nao, k=3) +
                      ti(anom_temperature925, seasonal_nao, k=3) + 
                      ti(anom_tailwind925, seasonal_nao, k = 3),  
                    family = betar(),  
                    data = DataGAM)
set.seed(123)
gam_model850 <- gam(Q ~ s(anom_temperature850, k=3) + 
                      s(anom_tailwind850, k=3) + 
                      s(seasonal_nao, k=3) +
                      ti(anom_temperature850, seasonal_nao, k=3) +  
                      ti(anom_tailwind850, seasonal_nao, k = 3), 
                    family =  betar(),  
                    data = DataGAM)

summary(gam_model1000)
summary(gam_model925)
summary(gam_model850)
check_collinearity(gam_model1000)
check_collinearity(gam_model925)
check_collinearity(gam_model850)
model_performance(gam_model1000)
model_performance(gam_model925)
model_performance(gam_model850)

# VISUALIZE GAM OUTPUTS ####
vis.gam(gam_model1000, view = c("anom_temperature1000", "seasonal_nao"), plot.type = "contour",
        main = "Interaction of temperature and NAO", color = "heat",
        xlab = "Anomalies of temperature °C around 100 m", ylab = "Seasonal NAO index",
        cex.main = 1.5,cex.lab = 1.2,cex.axis = 1.0)
vis.gam(gam_model925, view = c("anom_tailwind925", "seasonal_nao"), plot.type = "contour",
        main = "Interaction of tailwind and NAO", color = "cm",
        xlab = "Tailwind m/s around 700 m", ylab = "Seasonal NAO index",
        cex.main = 1.5,cex.lab = 1.2,cex.axis = 1.0)
vis.gam(gam_model850, view = c("anom_tailwind850", "seasonal_nao"), plot.type = "contour",
        main = "Interaction of tailwind and NAO", color = "cm",
        xlab = "Tailwind m/s around 1500 m", ylab = "Seasonal NAO index",
        cex.main = 1.5,cex.lab = 1.2,cex.axis = 1.0)

# plots per variable and then combine 
library(gridExtra)
library(ggplot2)
# temperature
temp1 <- draw(gam_model1000, residual = TRUE, select = 1)
temp2 <- draw(gam_model925, residual = TRUE, select = 1)
temp3 <- draw(gam_model850, residual = TRUE, select = 1)
# tailwind 
tail1 <- draw(gam_model1000, residual = TRUE, select = 2)
tail2 <- draw(gam_model925, residual = TRUE, select = 2)
tail3 <- draw(gam_model850, residual = TRUE, select = 2)
# NAO 
NAO1 <- draw(gam_model1000, residual = TRUE, select = 3)
NAO2 <- draw(gam_model925, residual = TRUE, select = 3)
NAO3 <- draw(gam_model850, residual = TRUE, select = 3)
# Use grid.arrange to combine them
temp <- grid.arrange(temp1, temp2, temp3,ncol = 3, nrow = 1)
tail <- grid.arrange(tail1, tail2, tail3, ncol = 3, nrow = 1)
NAO <- grid.arrange(NAO1, NAO2, NAO3, ncol = 3, nrow = 1)
# Save and export
ggsave("tempGAM24.png", temp, width = 3000, height = 2000, units = "px", 
       pointsize = 20,bg = "white",dpi = 300)
ggsave("tailGAM24.png", tail, width = 3000, height = 2000, units = "px", 
       pointsize = 20,bg = "white",dpi = 300)
ggsave("NAOGAM24.png", NAO, width = 3000, height = 2000, units = "px", 
       pointsize = 20,bg = "white",dpi = 300)
