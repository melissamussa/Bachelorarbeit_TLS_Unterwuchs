library(lidR)
library(sf)

laz_dir <- "F:/RStudio/Output_Stems_removed"
gpkg_file <- "E:/Bachelorarbeit/Daten/polygone_test/10x10_Polygone.gpkg"
output_dir <- "E:/Bachelorarbeit/Daten/lidar_10x10_Plots"

polygons <- st_read(gpkg_file)

laz_files <- list.files(laz_dir, pattern = "\\.laz$", full.names = TRUE)

# LAZ-Dateien öffnen und Koordinatensystem anpassen
for (laz_file in laz_files) {
  cat("Bearbeite Datei:", laz_file, "\n")
  
  las <- readLAS(laz_file)
  
  st_crs(las) <- 32629

  if (is.null(las)) {
    warning("Fehler bei: ", laz_file)
    next
  }
  
  # LAZ-Dateien anhand Polygone clippen
  for (i in seq_len(nrow(polygons))) {
    polygon <- polygons[i, ]  
    polygon_name <- as.character(polygon$Plot_nb) 
    
    clipped <- clip_roi(las, polygon)
    
    if (!is.null(clipped)) {
      output_file <- file.path(output_dir,
                               paste0(tools::file_path_sans_ext(basename(laz_file)), "_",
                                      polygon_name, ".laz"))
      writeLAS(clipped, output_file)
      cat("Gespeichert:", output_file, "\n")
    } else {
      warning("Kein Ergebnis für: ", polygon_name)
    }
  }
}
