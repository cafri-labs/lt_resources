library(terra)
library(dplyr)
library(tidyr)


######################### data setup ######################### 
lt_data_dir <- file.path("rasters/lt_nbr_ftv_1985_2021_combo6")
lt_name <- "lt_nbr_1985_2021_combo6" 
ftv_file_name <- "lt_nbr_ftv_1985_2021_combo6" 
vertex_file_name <- "lt_nbr_is_vertex_1985_2021_combo6" 
dir.create(paste0(lt_data_dir, "/ftv_tiles"))
dir.create(paste0(lt_data_dir, "/vertex_tiles"))
dir.create(paste0(lt_data_dir, "/masked_ftv"))
dir.create(paste0(lt_data_dir, "/losses"))

######################### merge tiles function ######################### 
#ftv stacks from LT are often very large, and as such are split into several tiles. This function will stitch those tiles back together
#if you have a single ftv file, you can skip this step
#requires gdal installation
merge_tiles <- function(source_dir, dest_file, pattern = ".*tif") {
  vrt_file <- gsub("(tiff|tif)", "vrt", dest_file)
  
  files <- list.files(source_dir, full.names = TRUE, pattern = pattern)
  index_file <- file.path(source_dir, "files.txt")
  writeLines(files, index_file)
  system(
    glue::glue("gdalbuildvrt {vrt_file} -input_file_list {index_file}")
  )
  system(
    glue::glue(
      paste(
        "gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=3 -co TILED=YES",
        "-co BIGTIFF=YES {vrt_file} {dest_file}"
      )
    )
  )
  unlink(index_file)
  unlink(vrt_file)
  return(dest_file)
}

merge_tiles(lt_data_dir, paste0(lt_data_dir, "/", ftv_file_name, "_all.tif"), pattern = "lt_nbr_ftv.*tif" )


######################### retile ftv and vertex layers ######################### 
# load merged ftv and vertex layers
ftv <- terra::rast(paste0(lt_data_dir, "/", ftv_file_name, "_all.tif"))
names(ftv) <- 1985:2022
is_vertex <- terra::rast(paste0(lt_data_dir, "/", vertex_file_name, ".tif"))
names(is_vertex) <- 1985:2022


#analysis requires much smaller tiles, can adjust size of tiles based on processing ability of your computer
tile_grid <- terra::rast(
  extent = terra::ext(ftv),
  ncols = 30, 
  nrows = 30
)

terra::makeTiles(
  ftv,
  tile_grid,
  path.expand(paste0(lt_data_dir, '/ftv_tiles/ftv_', ".tif")),
  overwrite = TRUE
)

terra::makeTiles(
  is_vertex,
  tile_grid,
  path.expand(paste0(lt_data_dir, '/vertex_tiles/is_vertex_', ".tif")),
  overwrite = TRUE
)

######################### mask smoothed ftv traj with vertices  ######################### 
#removing pixels that are not breakpoints in spectral trajectory
vertex_files <- list.files(paste0(lt_data_dir,"/vertex_tiles/"), pattern = "is_vertex_.*.tif", full.names = TRUE)
ftv_files <- list.files(paste0(lt_data_dir,"/ftv_tiles/"), pattern = "ftv_.*.tif", full.names = TRUE)

lapply(1:length(ftv_files), \(i) {
  message(paste0("Working on iteration ", i))
  vertex_file <- vertex_files[i]
  ftv_file <- ftv_files[i]
  index <- stringr::str_split(gsub(pattern = "\\.tif", "", basename(vertex_file)), "_")[[1]] |> tail(1)
  masked_ftv <- file.path(paste0(lt_data_dir,"/masked_ftv/masked_ftv_", index, ".tif"))
  
  if (!file.exists(masked_ftv)){
    terra::mask(terra::rast(ftv_file), terra::rast(vertex_file), maskvalues = 0, filename = masked_ftv)
  } 
})

######################### create all loss layer  ######################### 
#using masked ftv to identify decreasing segments
#value of previous vertex greater than value of current vertex

detect_loss <- function(model, data, ...){
  # Collect these for later
  years <- names(data)
  
  # Build a place holder data.frame
  results_container <- data.frame(
    matrix(nrow = nrow(data), ncol = length(years))
  )
  names(results_container) <- years
  
  # Compute magnitudes for loss vertices, with > 1 segment
  data <- data |> 
    mutate(id = 1:nrow(data)) |> # keep track of pixel order
    pivot_longer(-id, names_to = "year") |>
    mutate(year = as.numeric(year) + 1) |> # LT defaults adds a year to the vertex date to assign year of disturbance
    group_by(id) |>
    filter(!is.na(value)) |>
    filter(n() > 2) |>
    mutate(change = value - lead(value)) |>
    filter(change > 0) |> 
    select(-value) |>
    ungroup() |>
    pivot_wider(names_from = "year", values_from = "change")
  
  # Pad the results, filling in the years that don't have disturbances
  fill_years <- setdiff(years, names(data))
  fill_cols <- lapply(fill_years, \(fy) {
    return(rep(NA, nrow(data)))
  })
  names(fill_cols) <- fill_years
  data <- bind_cols(data, fill_cols) |>
    dplyr::relocate(all_of(years))
  
  # Insert results in the correct location
  results_container[data$id, ] <- data |> select(-id)
  return(results_container)
}

#use loss funtction to create loss tiles
masked_ftvs <- list.files(paste0(lt_data_dir, "/masked_ftv/"), full.names = TRUE)
lapply(masked_ftvs, \(masked_file) {
  index <- stringr::str_split(gsub(pattern = "\\.tif", "", basename(masked_file)), "_")[[1]] |> tail(1)
  losses <- file.path(paste0(lt_data_dir,"/losses/losses", index , ".tif")) 
  
  if (!file.exists(losses)){
    terra::predict(terra::rast(masked_file), list(), detect_loss, filename = losses)
  } 
})

#merge loss tiles into new loss.tif 
merge_tiles(paste0(lt_data_dir, "/losses"), paste0(lt_data_dir,"/losses/", lt_name,"_all_losses.tif"), pattern = "*.tif" )
all_losses <- rast("rasters/lt_nbr_rt1_1985-2022/losses/rt1_1985_2022_all_losses.tif")
names(all_losses) <- 1985:2022
all_losses <- subset(all_losses, 6:38) #remove pre 1990 years, optional step
writeRaster(all_losses, paste0(lt_data_dir,"/losses/", lt_name,"_all_losses.tif"), overwrite = TRUE)
