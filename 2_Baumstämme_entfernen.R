library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)
library(fs)
library(RANN)

options("install.lock"=FALSE)
install.packages("RANN")

wolke_Plot06 <- "D:/Daten_Ba/res01_normalisiert/res01_normalisiertcloud_ES_plot06_res01m_50x50m.laz"
stems_Plot06 <- "D:/CloudCompare/3DFin/Plot_01/Stems_in_stripe/Stems_Plot06.las"
output_Plot06 <- "D:/RStudio/Output_Stems_removed/Plot06_oStems_neu.laz"

  # Punktwolken einlesen
  wolke <- readLAS(wolke_Plot06)
  stems <- readLAS(stems_Plot06)
  
  
  # Koordinaten der Punktwolken in Matrix überführen
  pc1_coords <- as.matrix(wolke@data[, 1:3])
  pc2_coords <- as.matrix(stems@data[, 1:3])
  
  # nearest neighbor Analyse
  nn_results <- nn2(pc2_coords, pc1_coords, k = 1)
  
  # Distanzen extrahieren
  distances <- nn_results$nn.dists
  
  # Distanz Threshold festlegen, zum Punkte matchen
  threshold <- 0.1
  
  # Filter points that are farther than the threshold distance
  filtered_indices <- which(distances > threshold)
  filtered_points <- wolke@data[filtered_indices, ]
  
  # Neues LAS-Objekt
  pointcloud_difference <- LAS(filtered_points)
  
  writeLAS(pointcloud_difference, output_Plot06)
plot(pointcloud_difference)  
