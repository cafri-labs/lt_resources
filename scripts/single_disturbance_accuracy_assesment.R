library(terra)
library(dplyr)

#this accuracy assessment process is designed to match a disturbance raster to polygon reference data, replace reference data with your own polygon data for best results
#required data: LT output, target event polygons, polygons of study area bounds (tracts)

################################## set beginning and end of study window ##################################
#our reference data has different time ranges so we need to handle them separately for whole script, if you have just one area could simplify
uhw_study_begin <- 2010
uhw_study_end <- 2019
hwf_study_begin <- 1990
hwf_study_end <- 2019

################################## load reference data ##################################
harvests <- vect("../GIS_Files/combined_harvest_records.gpkg") #%>% project(labrador.client::get_cafri_crs()) #match reference data to disturbance raster crs
#remove harvests that start or end after the end of lt output
harvests <- harvests[harvests$start_date <= uhw_study_end & harvests$end_date < uhw_study_end] 
harvests$start_date <- as.numeric(harvests$start_date)
harvests$end_date <- as.numeric(harvests$end_date)

#out reference data has different ranges of harvest dates, have to be handled separately
HWF_harvests <- harvests[harvests$Property == "HWF"]
UHW_harvests <- harvests[harvests$Property == "UHW"]

#tract data is outer bounds of study area, for us here it is the property boundaries for our harvest record data
tracts <- vect("../GIS_Files/combined_tract_boundaries.gpkg")
tracts <- project(tracts, "EPSG:26918")
uhw_tracts <- tracts[tracts$Ownership == "UHW" ]
hwf_tracts <- tracts[tracts$Ownership == "HWF" ]

################################## load and adjust disturbance raster ##################################
disturbances <- rast("rasters/tuning_combos/lt_gee_nbr_greatest_tuning_recoverythreshold_75.tif")

#remove disturbances that occur after end of reference data, +1 is becuase we use +- 1 year buffer window for accuracy assesment below
disturbances[disturbances > (uhw_study_end +1)] <- 0 
#clip/mask rasters to extent of harvest polygons
uhw_disturbances <- crop(disturbances, uhw_tracts)
uhw_disturbances <- mask(uhw_disturbances, uhw_tracts)
hwf_disturbances <- crop(disturbances, hwf_tracts)
hwf_disturbances <- mask(hwf_disturbances, hwf_tracts)
#adjust start dates of disturbance rasters to match reference data
uhw_disturbances[uhw_disturbances < uhw_study_begin] <- 0
hwf_disturbances[hwf_disturbances < hwf_study_begin] <- 0

#create harvest start and end date rasters 
uhw_start_date <- rasterize(UHW_harvests, uhw_disturbances, field = 'start_date', background = NA)
uhw_end_date <- rasterize(UHW_harvests, uhw_disturbances, field = 'end_date', background = NA)

hwf_start_date <- rasterize(HWF_harvests, hwf_disturbances, field = 'start_date', background = NA)
hwf_end_date <- rasterize(HWF_harvests, hwf_disturbances, field = 'end_date', background = NA)

#set no data values to zero
uhw_disturbances[is.na(uhw_disturbances)] <- 0
uhw_start_date[is.na(uhw_start_date)] <- 0
uhw_end_date[is.na(uhw_end_date)] <- 0

hwf_disturbances[is.na(hwf_disturbances)] <- 0
hwf_start_date[is.na(hwf_start_date)] <- 0
hwf_end_date[is.na(hwf_end_date)] <- 0

################################## accuracy assesments ##################################

#UHW accuracy assessment calculations, note +/- 1 year buffer window
#create blank raster with same extent to map accuracy assesment values onto
uhw_accuracy_assessment <- uhw_disturbances
uhw_accuracy_assessment[!is.na(uhw_accuracy_assessment)] <- NA
#accuracy assessment
uhw_accuracy_assessment[uhw_disturbances == 0 & uhw_start_date == 0] <- 1 #no start date, no harvest, TN
uhw_accuracy_assessment[uhw_disturbances == 0 & uhw_start_date > 1] <- 2 #FN
uhw_accuracy_assessment[uhw_disturbances != 0 & (uhw_disturbances >= (uhw_start_date - 1)) & (uhw_disturbances <= (uhw_end_date + 1))] <- 3 #TP
uhw_accuracy_assessment[uhw_disturbances > 0 & (uhw_disturbances <= (uhw_start_date - 1) | uhw_disturbances >= (uhw_end_date + 1))] <- 4 #FP

uhw_accuracy_assessment <- crop(uhw_accuracy_assessment, uhw_tracts)
uhw_accuracy_assessment <- mask(uhw_accuracy_assessment, uhw_tracts)

#HWF accuracy assessment, note +/- 1 year buffer window
#create blank raster with same extent to map accuracy assesment values onto
hwf_accuracy_assessment <- hwf_disturbances
hwf_accuracy_assessment[!is.na(hwf_accuracy_assessment)] <- NA
#accuracy assessment
hwf_accuracy_assessment[hwf_disturbances == 0 & hwf_start_date == 0] <- 1
hwf_accuracy_assessment[hwf_disturbances == 0 & hwf_start_date > 1] <- 2
hwf_accuracy_assessment[hwf_disturbances != 0 & (hwf_disturbances >= (hwf_start_date - 1)) & (hwf_disturbances <= (hwf_end_date + 1))] <- 3
hwf_accuracy_assessment[hwf_disturbances > 0 & (hwf_disturbances <= (hwf_start_date - 1) | hwf_disturbances >= (hwf_end_date + 1))] <- 4

hwf_accuracy_assessment <- crop(hwf_accuracy_assessment, hwf_tracts)
hwf_accuracy_assessment <- mask(hwf_accuracy_assessment, hwf_tracts)

################################## calculate accuracy metrics ##################################
#accuracy metrics
freq_hwf <- freq(hwf_accuracy_assessment)
freq_uhw <- freq(uhw_accuracy_assessment)

TN <- freq_hwf[1,3] + freq_uhw[1,3]
FN <- freq_hwf[2,3] + freq_uhw[2,3]
TP <- freq_hwf[3,3] + freq_uhw[3,3]
FP <- freq_hwf[4,3] + freq_uhw[4,3]
precision <- TP/(TP + FP)
recall <- TP/(TP + FN)
F1 <- 2 * (recall * precision)/(recall + precision)
accuracy <- (TP + TN)/(TN + FN + TP + FP)
