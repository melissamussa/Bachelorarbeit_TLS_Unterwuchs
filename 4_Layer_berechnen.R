library(lidR)
library(sp)
library(terra)
library(sf)
library(rgdal)
library(RCSF)
library(fasterize)
library(openxlsx)
library(raster)
library(fs)

Wolke_folder <- "E:/Bachelorarbeit/Daten/lidar_10x10_Plots"
output_folder <- "E:/Bachelorarbeit/Daten/Layer_Prozente"

las_files <- fs::dir_ls(Wolke_folder, regexp = "\\.laz$")

polygons <- st_read(gpkg_file)

polygons$Plot_nb <- as.character(polygons$Plot_nb)

results <- data.frame(Plot = character(), Grass = numeric(), Herbs = numeric(), Shrub = numeric())

for (las_file in las_files) {
  base <- fs::path_ext_remove(fs::path_file(las_file))
  
  Testwolke_nof <- readLAS(las_file)
  
  # Punktwolken nach Schichten filtern
  Testwolke_grd <- filter_poi(Testwolke_nof, Classification == 2)
  Testwolke_hrb <- filter_poi(Testwolke_nof, Classification != 2, Classification != 7, Z <= 0.25)
  Testwolke_grs <- filter_poi(Testwolke_nof, Classification != 7, Z >= 0.25, Z <= 0.5)
  Testwolke_shr <- filter_poi(Testwolke_nof, Classification != 7, Z >= 0.5, Z <= 2)
  Testwolke_tre <- filter_poi(Testwolke_nof, Classification != 7, Z >= 2)
  
  # Raster erstellen
  
  # Krautige Pflanzen
  raster8h <- grid_metrics(Testwolke_hrb, ~mean(Z), 0.1)
  raster8h[is.na(raster8h)] <- 0      # leere Zellen auf 0
  raster8h[raster8h > 0] <- 2         # gefüllte Zellen auf 2
  
  
  # Gras
  raster8g <- grid_metrics(Testwolke_grs, ~mean(Z), 0.1)
  raster8g[is.na(raster8g)] <- 0   
  raster8g[raster8g >= 0.25] <- 2  
  
  
  # Sträucher
  raster8s <- grid_metrics(plot8_shrL, ~mean(Z), 0.1)
  raster8s[is.na(raster8s)] <- 0   
  raster8s[raster8s >= 0.5] <- 2      
  
  
  # Prozente brechnen
  
  # Krautige Pflanzen
  h0 <- length(raster8h[raster8h == 0])
  h2 <- length(raster8h[raster8h == 2])
  h_total <- h0 + h2
  h0_perc <- (h0 / h_total) * 100
  h2_perc <- (h2 / h_total) * 100
  
  # Gras
  g0 <- length(raster8g[raster8g == 0])
  g2 <- length(raster8g[raster8g == 2])
  g_total <- g0 + g2
  g0_perc <- (g0 / g_total) * 100
  g2_perc <- (g2 / g_total) * 100
  
  # Sträucher
  s0 <- length(raster8s[raster8s == 0])
  s2 <- length(raster8s[raster8s == 2])
  s_total <- s0 + s2
  s0_perc <- (s0 / s_total) * 100
  s2_perc <- (s2 / s_total) * 100
  
  
  results <- rbind(results, data.frame(Plot = base, Grass = g2_perc, Herbs = h2_perc, Shrub = s2_perc))
}

write.csv(results, file.path(output_folder, "Results_Percentages_neu.csv"), row.names = FALSE)
