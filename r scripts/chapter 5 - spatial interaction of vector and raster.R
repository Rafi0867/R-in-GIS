# install required packages----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  tidyterra, # handle and visualize raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  ggplot2
)


# cropping to the area of interest----
  #--- set the path to the folder to which you save the downloaded PRISM data ---#
  # This code sets the current working directory as the designated folder
  options(prism.path = "Data/Chapter 5")
  #--- download PRISM precipitation data ---#
  prism::get_prism_dailys(
    type = "tmax",
    date = "2018-07-01",
    keepZip = FALSE
  )
  #--- the file name of the PRISM data just downloaded ---#
  prism_file <- "Data/Chapter 5/PRISM_tmax_stable_4kmD2_20180701_bil/PRISM_tmax_stable_4kmD2_20180701_bil.bil"
  #--- read in the prism data ---#
  prism_tmax_0701_sr <- terra::rast(prism_file)
  
    # visualize the data
    ggplot() +
      geom_spatraster(data = prism_tmax_0701_sr) +
      scale_fill_whitebox_c(
        name = "tmax",
        palette = "muted",
        labels = scales::label_number(suffix = "º"),
        n.breaks = 12,
        guide = guide_legend(reverse = TRUE)
      ) +
      theme_void()
  
  
  #--- Kansas boundary (sf) ---#
  KS_county_sf <-
    #--- get Kansas county boundary ---
    tigris::counties(state = "Kansas", cb = TRUE) %>%
    #--- sp to sf ---#
    sf::st_as_sf() %>%
    #--- transform using the CRS of the PRISM tmax data  ---#
    sf::st_transform(terra::crs(prism_tmax_0701_sr))
      #--- gen map ---#
      ggplot() +
        geom_sf(data = KS_county_sf, fill = NA, color = "blue") +
        theme_void()
      
    #--- crop the entire PRISM to its KS portion---#
    prism_tmax_0701_KS_sr <-
      terra::crop(
        prism_tmax_0701_sr,
        KS_county_sf
      )
  
    # project the cropped kansas tmax data
        ggplot() +
          geom_spatraster(data = prism_tmax_0701_KS_sr) +
          geom_sf(data = KS_county_sf, fill = NA, color = "blue") +
          scale_fill_whitebox_c(
            name = "tmax",
            palette = "muted",
            labels = scales::label_number(suffix = "º"),
            n.breaks = 12,
            guide = guide_legend(reverse = TRUE)
          ) +
          theme_void()

# raster to data frame
  prism_data <- as.data.frame(prism_tmax_0701_KS_sr, xy = TRUE)
  KS_data <- as.data.frame(KS_county_sf, xy = TRUE)
  left_join(prism_data, KS_data)
  
#======================================================================================== 
# Prism data extraction and visualization ----
  set.seed(378533)
  
  #--- create polygons ---#
  polygon <-
    sf::st_polygon(list(
      rbind(c(0, 0), c(8, 0), c(8, 8), c(0, 8), c(0, 0))
    ))
  
  raster_like_cells <-
    sf::st_make_grid(polygon, n = c(8, 8)) %>%
    sf::st_as_sf() %>%
    mutate(value = sample(1:64, 64))
  
  stars_cells <-
    stars::st_rasterize(raster_like_cells, nx = 8, ny = 8)
  
  cell_centroids <-
    sf::st_centroid(raster_like_cells) %>%
    sf::st_as_sf()
  
  #--------------------------
  # Create points for which values are extracted
  #--------------------------
  #--- points ---#
  point_1 <- sf::st_point(c(2.4, 2.2))
  point_2 <- sf::st_point(c(6.7, 1.8))
  point_3 <- sf::st_point(c(4.2, 7.1))
  
  #--- combine the points to make a single  sf of points ---#
  points <- list(point_1, point_2, point_3) %>%
    sf::st_sfc() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(point_name = c("Point 1", "Point 2", "Point 3"))
  
  #--------------------------
  # Create maps
  #--------------------------
  ggplot() +
    #geom_stars(data = stars_cells, alpha = 0.5) +
    scale_fill_distiller(name = "Value", palette = "Spectral") +
    geom_sf_text(data = raster_like_cells, aes(label = value)) +
    geom_sf(data = points, aes(shape = point_name), size = 2) +
    scale_shape(name = "Points") +
    theme_void() 
  
  
  
# 3xtracting values within a polygon
  #--------------------------
  # Create a polygon for which values are extracted
  #--------------------------
  polygon_extract <-
    sf::st_polygon(list(
      rbind(c(1.5, 2), c(6, 2.3), c(7, 6.5), c(2, 5), c(1.5, 2))
    ))
  
  polygons_extract_viz <-
    ggplot() +
    #geom_stars(data = stars_cells, alpha = 0.5) +
    scale_fill_distiller(name = "Value", palette = "Spectral") +
    geom_sf(data = polygon_extract, fill = "gray", alpha = 0.5) +
    geom_sf(data = cell_centroids, color = "black", size = 0.8) +
    geom_sf_text(
      data = raster_like_cells, 
      aes(label = value),
      nudge_x = -0.25,
      nudge_y = 0.25
    ) +
    theme_void() 
  
  polygons_extract_viz
  
  
  
#================================================================================
  #--- download PRISM precipitation data ---#
  prism::get_prism_dailys(
    type = "ppt",
    date = "2018-07-02",
    keepZip = FALSE
  )
  
  #--- the file name of the PRISM data just downloaded ---#
  prism_file <- "Data/Chapter 5/PRISM_tmax_stable_4kmD2_20180702_bil/PRISM_tmax_stable_4kmD2_20180702_bil.bil"
  
  #--- read in the prism data and crop it to Kansas state border ---#
  prism_tmax_0702_KS_sr <-
    terra::rast(prism_file) %>%
    terra::crop(KS_county_sf)
trial_data <- left_join(prism_data, trial_data) 
