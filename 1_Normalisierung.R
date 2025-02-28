library(terra)
library(lidR)


input_file <- "G:/Daten_Ba/res01m/cloud_ES_plot01_res01m_50x50m.laz"
output_file <- "G:/RStudio/Output_Normalisiert/Plot01_norm.laz"

lidar_Plot01 <- readLAS(input_file)

summary(lidar_Plot01)

# Bodenpunkte klassifizieren und DTM erstellen

Boden_Plot01 <- classify_ground(lidar_Plot01, algorithm = csf())

DTM_Plot01 <- rasterize_terrain(Boden_Plot01, res = 1, algorithm = tin())

# Normalisierung

normalized_Plot01 <- normalize_height(Boden_Plot01, DTM_Plot01)

plot(normalized_Plot01, color = "Z", main = "Normalisierte Punktwolke")

writeLAS(normalized_Plot01, output_file)
 