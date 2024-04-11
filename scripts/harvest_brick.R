library(terra)
######################## set up######################## 
years <- 1990:2021 #study years
harvest_directory <- file.path("../GIS_Files/harvest_rasters")
dir.create(paste0(harvest_directory, "/harv_layers"))
harv_records <- vect("../GIS_Files/albers_combined_harv_records.gpkg")
#need study area bounds
harv_boundaries <- vect("../GIS_Files/combined_tract_boundaries.gpkg")

crs <- labrador.client::get_cafri_crs()


######################## rasterize harvests  ######################## 
harv_records$value <- 1 #dummy value for creating harvest raster

#use your LT output as reference raster here, want harvest layer to have same extent
ref_raster <- rast("rasters/combo12_all_losses2.tif")[[1]]
values(ref_raster) <- NA

lapply(1:length(years), \(i) {
  message(paste0("Working on iteration ", i))
  
  harv_subset <- terra::subset(harv_records,
                               (years[i] >= harv_records$start_date & years[i] <= harv_records$end_date)
                               | as.integer(harv_records$start_date) + 1 == years[i]
                               | as.integer(harv_records$end_date) + 1== years[i]
                               | as.integer(harv_records$start_date) - 1 == years [i]
                               | as.integer(harv_records$end_date) - 1 == years[i] )
  
  harv_file <- file.path(paste0(harvest_directory, "/harv_layers/harv_layer_", years[i], ".tif"))
  harv_exists <- file.exists(harv_file)
  
  if (!harv_exists & nrow(harv_subset) != 0){
    terra::rasterize(harv_subset, ref_raster, field = "value", filename = harv_file)
  } else if (!harv_exists){
    writeRaster(ref_raster, filename = harv_file)
  }
})

######################## combine havrvest rasters into single harvest brick layer ######################## 
rasters <- list.files(paste0(harvest_directory, "/harv_layers/"), full.names = TRUE)
harv_raster <- rast(rasters)
names(harv_raster) <- years
harv_raster[is.na(harv_raster)] <- 0
writeRaster(harv_raster, paste0(harvest_directory,"/harvest_brick.tif"), overwrite = TRUE)
harv_raster <- rast(paste0(harvest_directory,"/harvest_brick.tif"))

######################## mask harvest brick with study area boundaries ######################## 
#terra throws a fit if the crs info doesn't match exactly, just making the labels match here, its the same crs
crs(harvest_rast) <- crs

#dummy value for making raster
harv_boundaries$rast_value <- 1
harv_boundaries <- rasterize(harv_boundaries, harv_raster, field = "rast_value")
#areas outside harvests set to NA
harv_raster <- mask(harv_raster, harv_boundaries) 

writeRaster(harv_raster, paste0(harvest_directory,"/harvest_brick.tif"), overwrite = TRUE)
rast