library(terra)
library(sf)
library(randomForest)
library(raster)
library(sp)
library(glue)
library(tidyverse)
library(Metrics)

options("install.lock"=FALSE)

s2_imgs <- rast("E:/Bachelorarbeit/Daten/Sentinel_Tiff/Raster_Stack_Geb.tif")

# Namen anpassen

names(s2_imgs) <- paste0("B", seq_len(nlyr(s2_imgs)))

# Überpfrüfen

print(head(s2_imgs))
str(s2_imgs)
sum(nrow(vec))

vec <- terra::vect("E:/Bachelorarbeit/Daten/polygone_test/10x10_polys_Auswahl.gpkg")

# TLS-Daten anpaasen

vec_clean <- vec[complete.cases(as.data.frame(vec)),]

vec_clean2 <- vec_clean[,c(8:12)]

# Sentinel-2-Daten anpassen

preds_s2 <- extract(s2_imgs, vec_clean2, ID=F)

names(preds_s2) <- names((s2_imgs))

# Vektor als Dataframe

vec_df <- as.data.frame(vec_clean2)


# Random Forest anwenden

### Krautige Pflanzen (Herbs)

set.seed(3)
model_n_s2H <- randomForest(preds_s2, vec_df$Herbs, ntree = 500, mtry=sqrt(ncol(preds_s2)), na.action = omit)
model_n_s2H

# Scatterplot erstellen
plot(model_n_s2H$predicted, vec_df$Herbs,
     main = "Krautige Pflanzen",  
     xlab = "Vorhergesagte Werte (%)", 
     ylab = "Tatsächliche Werte (%)", 
     pch = 16, col = "blue")  

abline(0, 1, col = "red", lwd = 2)  

# Vorhersage auf Waldflächen im Gebiet übertragen

pred_n_s2H <- predict(s2_imgs, model_n_s2H, type="response", na.rm=TRUE) 
plot(pred_n_s2H, main="Verteilung der Krautigen Pflanzen im Untersuchungsgebiet", zlim=c(1,5))
plot()

# Tatsächliche vs. vorhergesagte Werte
obsH <- model_n_s2H$y
predH <- model_n_s2H$predicted

# Statistische Fehler
rmse_valH <- rmse(obsH, predH)
mae_valH <- mae(obsH, predH)
r2_valH <- cor(obsH, predH)^2

cat("RMSE:", rmse_valH, "\nMAE:", mae_valH, "\nR²:", r2_valH, "\n")

#### Gras (Grass)

set.seed(3)
model_n_s2G <- randomForest(preds_s2, vec_df$Grass, ntree = 500, mtry=sqrt(ncol(preds_s2)), na.action = omit)
model_n_s2G

plot(model_n_s2G$predicted, vec_df$Grass,
     main = "Gras", 
     xlab = "Vorhergesagte Werte (%)", 
     ylab = "Tatsächliche Werte (%)",  
     pch = 16, col = "blue") 

abline(0, 1, col = "red", lwd = 2)  

pred_n_s2G <- predict(s2_imgs, model_n_s2G, type="response", na.rm=TRUE) 
plot(pred_n_s2G, main="Verteilung der Gräser im Untersuchungsgebiet", zlim=c(1,5))
plot()

obsG <- model_n_s2G$y
predG <- model_n_s2G$predicted

rmse_valG <- rmse(obsG, predG)
mae_valG <- mae(obsG, predG)
r2_valG <- cor(obsG, predG)^2

cat("RMSE:", rmse_valG, "\nMAE:", mae_valG, "\nR²:", r2_valG, "\n")


### Sträucher (shrub)

set.seed(3)
model_n_s2S <- randomForest(preds_s2, vec_df$Shrub, ntree = 500, mtry=sqrt(ncol(preds_s2)), na.action = omit)
model_n_s2S

plot(model_n_s2H$predicted, vec_df$Shrub,
     main = "Sträucher", 
     xlab = "Vorhergesagte Werte (%)",  
     ylab = "Tatsächliche Werte (%)", 
     pch = 16, col = "blue")  

abline(0, 1, col = "red", lwd = 2)  

pred_n_s2S <- predict(s2_imgs, model_n_s2S, type="response", na.rm=TRUE) 
plot(pred_n_s2S, main="Verteilung der Sträucher im Untersuchungsgebiet", zlim=c(1,5))
plot()

obsS <- model_n_s2S$y
predS <- model_n_s2S$predicted

rmse_valS <- rmse(obsS, predS)
mae_valS <- mae(obsS, predS)
r2_valS <- cor(obsS, predS)^2

cat("RMSE:", rmse_valS, "\nMAE:", mae_valS, "\nR²:", r2_valS, "\n")
