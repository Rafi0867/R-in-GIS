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
  ggplot2,
  exactextractr
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
      crop(
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

# # raster to data frame
#   prism_data <- as.data.frame(prism_tmax_0701_KS_sr, xy = TRUE)
#   KS_data <- as.data.frame(KS_county_sf, xy = TRUE)
#   left_join(prism_data, KS_data)
#   
# #======================================================================================== 
# # Prism data extraction and visualization ----
#   set.seed(378533)
#   
#   #--- create polygons ---#
#   polygon <-
#     sf::st_polygon(list(
#       rbind(c(0, 0), c(8, 0), c(8, 8), c(0, 8), c(0, 0))
#     ))
#   
#   raster_like_cells <-
#     sf::st_make_grid(polygon, n = c(8, 8)) %>%
#     sf::st_as_sf() %>%
#     mutate(value = sample(1:64, 64))
#   
#   stars_cells <-
#     stars::st_rasterize(raster_like_cells, nx = 8, ny = 8)
#   
#   cell_centroids <-
#     sf::st_centroid(raster_like_cells) %>%
#     sf::st_as_sf()
#   
#   #--------------------------
#   # Create points for which values are extracted
#   #--------------------------
#   #--- points ---#
#   point_1 <- sf::st_point(c(2.4, 2.2))
#   point_2 <- sf::st_point(c(6.7, 1.8))
#   point_3 <- sf::st_point(c(4.2, 7.1))
#   
#   #--- combine the points to make a single  sf of points ---#
#   points <- list(point_1, point_2, point_3) %>%
#     sf::st_sfc() %>%
#     sf::st_as_sf() %>%
#     dplyr::mutate(point_name = c("Point 1", "Point 2", "Point 3"))
#   
#   #--------------------------
#   # Create maps
#   #--------------------------
#   ggplot() +
#     #geom_stars(data = stars_cells, alpha = 0.5) +
#     scale_fill_distiller(name = "Value", palette = "Spectral") +
#     geom_sf_text(data = raster_like_cells, aes(label = value)) +
#     geom_sf(data = points, aes(shape = point_name), size = 2) +
#     scale_shape(name = "Points") +
#     theme_void() 
#   
#   
#   
# # 3xtracting values within a polygon
#   #--------------------------
#   # Create a polygon for which values are extracted
#   #--------------------------
#   polygon_extract <-
#     sf::st_polygon(list(
#       rbind(c(1.5, 2), c(6, 2.3), c(7, 6.5), c(2, 5), c(1.5, 2))
#     ))
#   
#   polygons_extract_viz <-
#     ggplot() +
#     #geom_stars(data = stars_cells, alpha = 0.5) +
#     scale_fill_distiller(name = "Value", palette = "Spectral") +
#     geom_sf(data = polygon_extract, fill = "gray", alpha = 0.5) +
#     geom_sf(data = cell_centroids, color = "black", size = 0.8) +
#     geom_sf_text(
#       data = raster_like_cells, 
#       aes(label = value),
#       nudge_x = -0.25,
#       nudge_y = 0.25
#     ) +
#     theme_void() 
#   
#   polygons_extract_viz
#   
#   
  
#================================================================================
  # creating another reaster layer for further practice of multiple layer----
      ##--- download PRISM precipitation data ----
      prism::get_prism_dailys(
        type = "tmax",
        date = "2018-07-02",
        keepZip = FALSE
      )
      
      ##--- the file name of the PRISM data just downloaded ----
      prism_file <- "Data/Chapter 5/PRISM_tmax_stable_4kmD2_20180702_bil/PRISM_tmax_stable_4kmD2_20180702_bil.bil"
      
      ##--- read in the prism data and crop it to Kansas state border ----
      prism_tmax_0702_KS_sr <-
        terra::rast(prism_file) %>%
        terra::crop(KS_county_sf)
      
      ### visualize the data----
      ggplot() +
        geom_spatraster(data = prism_tmax_0702_KS_sr) +
        geom_sf(data = KS_county_sf, fill = NA, color = "blue") +
        scale_fill_whitebox_c(
          name = "tmax",
          palette = "muted",
          n.breaks = 12,
          guide = guide_legend(reverse = TRUE)
        ) +
        theme_void()
      
      ## adding the kansas irrigation data----
      KS_wells <- readRDS("Data/Chapter 5/Chap_5_wells_KS.rds")
      ### visualize the data----
      ggplot() +
        geom_spatraster(data = prism_tmax_0702_KS_sr) +
        geom_sf(data = KS_county_sf, fill = NA, color = "blue") +
        geom_sf(data = KS_wells, color = "black", fill = NA, size = 0.5) +
        scale_fill_whitebox_c(
          name = "tmax",
          palette = "muted",
          n.breaks = 10,
          guide = guide_legend(reverse = TRUE)
        ) +
        theme_void()+
        labs(
          title = "Map of Kansas county boundaries, irrigation wells and PRISM max temp"
        )+
        theme(
          plot.title = element_text(hjust = 0.5)
        )
  
  # extract tmax values from prism layer by points----
  tmax_from_prism <- terra::extract(prism_tmax_0701_KS_sr, KS_wells)
  head(tmax_from_prism)
    #here each id is a well
  
  
  
  
  # extract tmax values from prism layer by polygons (single layer)----
  tmax_by_county <- terra::extract(prism_tmax_0701_KS_sr, KS_county_sf)
  head(tmax_by_county)  
    # here each id is a county.
    list(KS_county_sf$NAME) #there are 105 counties in Kansas
    # lets see the tail of the data to varify if we get the data by county
    tail(tmax_by_county)
    
    # now calculate the mean of tmax by county to have only 105 observation
    mean_tmax<- tmax_by_county %>%
      group_by(ID) %>%
      summarise(tmean = mean(PRISM_tmax_stable_4kmD2_20180701_bil))
    
    ## appending the tmax data with county sf data----
      KS_county_sf <-
        #--- back to sf ---#
        KS_county_sf %>%
        #--- define ID ---#
        mutate(ID := seq_len(nrow(.))) %>%
        #--- merge by ID ---#
        left_join(., mean_tmax, by = "ID") 
      
    
  # extracting single layer data with FUN function----
    tmax_by_county_2 <- terra::extract(prism_tmax_0701_KS_sr, KS_county_sf,
                                       fun = min) %>%
      rename(tmin = PRISM_tmax_stable_4kmD2_20180701_bil)
      #now append the data with KS county sf data
      KS_county_sf_2 <- KS_county_sf %>%
        mutate(ID := seq_len(nrow(.))) %>%
        left_join(., tmax_by_county_2, by = "ID")
      
      
      
  # extract from multi-layer SpatRaster----
      ## creating a multi layer raster 
      prism_tmax_stack <- c(prism_tmax_0701_KS_sr, prism_tmax_0702_KS_sr)
      
      #--- extract tmax values ---#
      tmax_from_prism_stack <- terra::extract(prism_tmax_stack, KS_wells)
      
      
      ## extracting from a multi-layer Raster object----
      tmax_by_county_from_stack <- terra::extract(prism_tmax_stack, KS_county_sf)
      head(tmax_by_county_from_stack)   
      # this is simply extracting data from the raster. since the raster contains
      # multiple data layer, so we are getting the data of all the layers as a
      # new column of the data set. If we have four layers in our raster data
      # we will then have four separate columns of values each with the name of
      # the layer. here each ID is a well in Kansas,
      
      
      ## extracting from multi-layer Raster object  with add-ons ----
      tmax_by_county_from_stack <- terra::extract(prism_tmax_stack, 
                                                  KS_county_sf,
                                                  exact = TRUE)
      head(tmax_by_county_from_stack)
      # now you have fraction column to the resulting data.frame and you can find 
      # area-weighted summary of the extracted values
      
      
      # calculating area weighted summary of the extracted values----
      area_summary_tmax_by_county <- tmax_by_county_from_stack %>%
        group_by(ID) %>%
        summarize(
          tmax_0701 = sum(fraction * PRISM_tmax_stable_4kmD2_20180701_bil) / sum(fraction),
          tmax_0702 = sum(fraction * PRISM_tmax_stable_4kmD2_20180702_bil) / sum(fraction)
        )
      head(area_summary_tmax_by_county)      
      # now we have a area weighted tmax summary by county data   
      
      
      
  # # NOT WORKING FOR ME alternative to terra::exact() function ----
  #     #--- syntax (this does not run) ---#
  #     # exactextractr::exact_extract(raster, polygons sf, include_cols = list of vars)
  #     
  #     # Convert SpatRaster to RasterLayer
  #     prism_tmax_0701_KS_raster <- raster::raster(prism_tmax_0701_KS_sr)
  #     
  #     # extracting value from single layer raster
  #     tmax_by_county <-
  #       exactextractr::exact_extract(
  #         prism_tmax_0701_KS_sr,
  #         KS_county_sf,
  #         #--- inherit COUNTYFP from KS_county_sf ---#
  #         include_cols = "COUNTYFP",
  #         #--- this is for not displaying progress bar ---#
  #         progress = TRUE
  #       )
  #     
  #     extacted_mean <- 
  #       exactextractr::exact_extract(
  #         prism_tmax_stack, 
  #         KS_county_sf, 
  #         "mean", 
  #         append_cols = "COUNTYFP", 
  #         progress = FALSE
  #       )
  #     
  #     head(extacted_mean)
      
      
      
      
  # Execution speed checking ----
      ## Data Preparation ----
        ### creating a SpatVector data ----
        KS_wells_sv <- terra::vect(KS_wells)
        KS_county_sv <- terra::vect(KS_county_sf)  
        
        
        ### disaggregatibng tmax data ----
        prism_tmax_0701_KS_sr_10 <- terra::disagg(prism_tmax_0701_KS_sr, fact = 10)
        
        #--- original ---#
        dim(prism_tmax_0701_KS_sr)
        #--- disaggregated ---#
        dim(prism_tmax_0701_KS_sr_10)
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        