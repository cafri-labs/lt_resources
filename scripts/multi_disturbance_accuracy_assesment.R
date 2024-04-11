library(terra)
library(exactextractr)
library(dplyr)
library(future)
library(future.apply)
library(progressr)


############################################# set up #############################################
#this accuracy assessment process is designed to match a an annual disturbance raster stack to polygon reference data
#replace reference data with your own polygon data for best results
#required data: LT output, target event polygons, raster of event polygons, polygons of study area bounds (tracts)

#adjusts allowable ram usaage
terra::terraOptions(memfrac=0.85)

study_begin <- 1990
study_end <- 2021
crs <- labrador.client::get_cafri_crs()

#load data
crs <-  labrador.client::get_cafri_crs()
tracts <- vect("../GIS_Files/combined_tract_boundaries.gpkg")
harvests <- vect("../GIS_Files/albers_combined_harv_records.gpkg") |> terra::as.data.frame(geom = "WKT")
harvest_rast <- rast("../GIS_Files/harvest_rasters/harvest_brick.tif")
losses <- rast("rasters/combo12_all_losses2.tif")
#terra throws a fit if the crs info doesn't match exactly, just making the labels match here, its the same crs 
crs(losses) <- crs

#data names
lt_output_name <- "combo_12"

############################################# functions ###########################################

#harvest accuracy assessment function
#this function assesses the area within harvests
#assesses accuracy for each harvest event

polygon_acc_assesment <- function(harvest, loss_layer){
  
  # Assumes loss_layer is passed as a filename - important for parallelization with terra
  loss_layer <- terra::rast(loss_layer) |> terra::crop(harvest)
  start_year <- harvest$start_date
  end_year <- harvest$end_date
  
  #assigning buffer years, each harvest gets +- 1 year buffer window, unless they align with the end of the study period
  if (end_year == 2021) {
    harv_years <- c((as.numeric(start_year)-1):(as.numeric(end_year)))
  } else if (start_year == 1990) {
    harv_years <- c((as.numeric(start_year)):(as.numeric(end_year) + 1))
  } else {
    harv_years <- c((as.numeric(start_year)-1):(as.numeric(end_year) +1))
  }
  
  #subset loss layer so that only the layers for the harvest years remain
  harv_year_losses <- subset(loss_layer, as.character(harv_years))
  # exact extract wont count NA values, so this makes counting TPs easy
  harv_year_losses[harv_year_losses < 0] <- NA 
  
  TP <- exact_extract(harv_year_losses, harvest, 'count')
  #add up TP for all years of the harvest
  TP <- sum(TP)
  #FN = total pixels in the harvest - TPs
  #the number of pixels in the harvest is constant so loss_layer[[1]] calls the first layer of the loss object to count them
  FN <- exact_extract(loss_layer[[1]], harvest, 'count')
  FN <- FN - TP
  
  results <- as.data.frame(TP) |> cbind(FN)
  
  #this method essentially takes an overhead view of the harvest 
  #instead of treating each year individually, we look at the harvest as a whole
  #to get the right number of pixels for the multi year stack, multiply by the number of years in the harvest
  results <- results * as.numeric(length(harv_years)) 
  return(results)
}

# tract accuracy assessment function
#this function looks at areas outside your harvests but within your study area boundaries
#assess accuracy on an annual basis

tract_acc_assesment <- function(tract_losses_file_path, i, tract){
  #going one year at a time
  # Assumes loss file  is passed as a filename - important for parallelization with terra
  year_layer <- terra::rast(tract_losses_file_path, lyrs=i) |>
    terra::crop(tract)
  
  #FP = total number of disturbances detected, since outside reference data area any detection are automatic FP
  #TN = total number of pixels - FPS
  #however to count this easily first need total number of pixels
  total_pixels <- exact_extract(year_layer, tract, 'count')
  
  #set all the 0 pixels to NA now
  #couldn't have done this before now without making it difficult to calculate the total number of pixels inside the tracts
  year_layer[year_layer <= 0] <- NA
  
  #count FP
  FP <- exact_extract(year_layer, tract, 'count')
  
  #count TN
  TN <- total_pixels - FP
  
  results <- as.data.frame(sum(TN)) |> cbind(sum(FP))
  return(results)
}


####################################### reference data formatting ####################################### 

#match reference data dates to LT output dates, check validity of harvest dates
harvests <- harvests %>% filter(!(is.na(start_date)| is.na(end_date))) %>% 
  filter(!(end_date < start_date)) %>% 
  filter(!(start_date < study_begin | end_date > study_end | start_date > study_end))
harvests <- vect(harvests, geom = "geometry")
#terra also looses the crs information when moving from a vector -> dataframe -> vector so need to reassign it here
crs(harvests) <- crs


####################################### harvest polygon accuracy assessment  ####################################### 
# tallying TP and FN

#create new loss layer that only has values for areas within harvests
#duplicate loss layer so don't overwrite original 
polygon_losses <- losses
#set all NA to -1, use -1 not zero in case there were any very small disturbance magnitudes
polygon_losses[is.na(polygon_losses)] <- -1 
#mask so all areas outside harvest polygons are NA, areas inside harvest are now either disturbance magnitude or -1
polygon_losses <- mask(polygon_losses, harvests) 
#need to write new losses file for polygon acc assessment function
writeRaster(polygon_losses, paste0(lt_output_name, "_polygon_assesment_loss_layer.tif"), overwrite = TRUE) 
#read file path back
loss_layer <- file.path(paste0(lt_output_name, "_polygon_assesment_loss_layer.tif"))

#harvests have to be sf object to work with parallelization
harvests <- sf::st_as_sf(harvests) 

#make empty data frame for results
polygon_results <- data.frame(TP=0, FN = 0)
names(polygon_results) <- c("TP", "FN")

#run polygon assessment function
future::plan('multisession')

progressr::with_progress({
  p <- progressr::progressor(along = 1:nrow(harvests))
  polygon_results <- future.apply::future_lapply(seq_len(nrow(harvests)), \(i) {
    p(message = paste0("Processing harvest: ", (i)))
    harvest <- harvests[i,]
    polygon_acc_assesment(harvest, loss_layer)
  })
})
  
polygon_results <- dplyr::bind_rows(polygon_results)

#write.csv(polygon_results, paste0(lt_output_name, "_multidisturbance_polygon_accuracy.csv"), row.names = FALSE)


####################################### asses area outside harvest polygons #######################################
#tallies FP and TN


##### make tract brick #######
#can use any raster with same extent as template
tract_rast <- fasterize::fasterize(sf::st_as_sf(tracts), raster::raster(losses)) 
#convert to spatRaster
tract_rast <- rast(tract_rast)

###### recrop loss layer #####
tract_losses <- crop(losses, tract_rast) 
#focused on losses outside harvest areas now
#set areas outside of the tracts to -1, -1 just dummy value
tract_losses <- mask(tract_losses, tract_rast, updatevalue = -1) 
#set any NA pixels inside tracts to 0
tract_losses[is.na(tract_losses)] <- 0
#set outside values back to NA, now areas outside tracts are NA and areas inside are either the loss magnitude or 0
tract_losses[tract_losses == -1] <- NA
#remove harvested areas from loss layer
#the combination of mask_values = 0 and inverse = true means we are keeping the areas outside the harvests
tract_losses <- mask(tract_losses, harvest_rast, maskvalues = 0, inverse = TRUE)
crs(harvest_rast) <- crs
#write tract loss layer for tract accuracy assessment function
writeRaster(swiss_cheese, paste0(lt_output_name, "_tract_loss_layer.tif"), overwrite = TRUE)

#tract layer has to be sf object for exact extract functions
tracts <- sf::st_as_sf(tracts)

#create blank df for results
tract_results <- data.frame(TN=0, FP = 0)
names(tract_results) <- c("TN", "FP")

#load tract losses file path, has to be file path for function
tract_losses_file_path <- file.path(paste0(lt_output_name, "_tract_loss_layer.tif"))

years <- names(rast(tract_losses))


#run tract accuracy assesment function
future::plan('multisession')

progressr::with_progress({
  p <- progressr::progressor(along = 1:length(names))
  tract_results <- future.apply::future_lapply(seq_len(length(names)), \(i) {
    p(message = paste0("Processing layer: ", (i)))
    tract_acc_assesment(tract_losses, i, tracts)
  })
})

tract_results <- dplyr::bind_rows(tract_results)
#write.csv(tract_results, paste0(lt_output_name, "_multidisturbance_tract_accuracy.csv"), row.names = FALSE)


####################### combine results ##########################

full_results <- cbind(sum(polygon_results[['TP']]), sum(polygon_results[['FP']]), sum(tract_results[['sum(TN)']]), sum(tract_results[['sum(FP)']]))
full_results <- as.data.frame(full_results)
names(full_results) <- c("TP", "FN", "TN", "FP")

#write.csv(full_results, paste0(lt_output_name, "_multidisturbance__accuracy.csv"), row.names = FALSE)

precision <- full_results$TP / (full_results$TP + full_results$FP)
recall <- full_results$TP / (full_results$TP + full_results$FN)
f1 <- 2 * (recall * precision)/(recall + precision)
accuracy <- (full_results$TP + full_results$TN)/(full_results$TN + full_results$FN + full_results$TP + full_results$FP)
