library(terra)
library(raster)
library(sp)
library(grid)
library(ggspatial) 

#1. Bilder der verschiedenen Daten zu einem gemeinsamen Stack hinzufügen

raster_files <- list.files("H:/Bachelorarbeit/Daten/Sentinel_Tiff", pattern = "\\.tif$", full.names = TRUE)

# Stack erstellen

raster_stack <- terra::rast(raster_files)

# Überprüfen

print(raster_stack)

writeRaster(raster_stack, "H:/Bachelorarbeit/Daten/Sentinel_Tiff/Raster_Stack.tif", overwrite = TRUE)


#2. Raster Stack auf die Forstflächen zuschneiden

s2_imgs <- rast("E:/Bachelorarbeit/Daten/Sentinel_Tiff/Raster_Stack.tif")
crop <- vect("E:/Bachelorarbeit/Daten/polygone_test/Ourense_nurWald29N.gpkg")

#Raster Daten auf Polygone zuschneiden

s2_imgs_crop <- crop(s2_imgs, crop)
s2_imgs_mask <- mask(s2_imgs_crop, crop)

#Überprüfen

plotRGB(s2_imgs_mask, r=4, g=3, b=2, stretch="lin")

#zugeschnittenen Raster abspeichern
writeRaster(s2_imgs_mask, "E:/Bachelorarbeit/Daten/Sentinel_Tiff/Raster_Stack_Cropped.tif", overwrite = TRUE)

s2_imgs_test <- rast("E:/Bachelorarbeit/Daten/Sentinel_Tiff/Raster_Stack_Geb.tif")
plotRGB(s2_imgs_test, r=26, g=25, b=24, stretch="lin", axes = TRUE)
