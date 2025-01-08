#===============================================================================
#--- Initialization ---#
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2,
  dplyr,
  tidyverse,
  data.table,
  sf,
  tmap,
  rmapshaper,
  mapview
)
#check documentation
?geos_binary_pred


#===============================================================================
#---- Data preparation ----
  #--- create points ---#
  point_1 <- sf::st_point(c(2, 2))
  point_2 <- sf::st_point(c(1, 1))
  point_3 <- sf::st_point(c(1, 3))
  
  #--- combine the points to make a single  sf of points ---#
  (
    points <- 
      list(point_1, point_2, point_3) %>% 
      sf::st_sfc() %>% 
      sf::st_as_sf() %>% 
      dplyr::mutate(point_name = c("point 1", "point 2", "point 3"))
  )
  
  #--- plotting the points ---#
  plot(points)
  
  #--- create points for lines ---#
  line_1 <- sf::st_linestring(rbind(c(0, 0), c(2.5, 0.5)))
  line_2 <- sf::st_linestring(rbind(c(1.5, 0.5), c(2.5, 2)))
  
  #--- combine the points to make a single  sf of points ---#
  (
    lines <- 
      list(line_1, line_2) %>% 
      sf::st_sfc() %>% 
      sf::st_as_sf() %>% 
      dplyr::mutate(line_name = c("line 1", "line 2"))
  )
  
  #--- plotting the lines ---#
  plot(lines)
  
  
  #--- create polygons ---#
  polygon_1 <- sf::st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)) 
  ))
  
  polygon_2 <- sf::st_polygon(list(
    rbind(c(0.5, 1.5), c(0.5, 3.5), c(2.5, 3.5), c(2.5, 1.5), c(0.5, 1.5)) 
  ))
  
  polygon_3 <- sf::st_polygon(list(
    rbind(c(0.5, 2.5), c(0.5, 3.2), c(2.3, 3.2), c(2, 2), c(0.5, 2.5)) 
  ))
  
  #--- combine the polygons to make an sf of polygons ---#
  (
    polygons <- 
      list(polygon_1, polygon_2, polygon_3) %>% 
      #creates a list of items 
      sf::st_sfc() %>% 
      #convert the list into simple feature collection
      sf::st_as_sf() %>% 
      #convert the sfc into sf object
      dplyr::mutate(polygon_name = c("polygon 1", "polygon 2", "polygon 3"))
      #rename the objects for clean view in map
  )

  #--- plotting the polygon ---#
  plot(polygons)

  #--- plot everything together ---#
  ggplot()+
    geom_sf(data = polygons, aes(fill = polygon_name), alpha = 0.2)+
    geom_sf(data = lines, aes(color = line_name))+
    geom_sf(data = points, aes(shape = point_name), size = 4)+
    theme_void()+
    labs(
      fill = "Polygons",
      color = "Lines",
      shape = "Points",
      title = "Combining All Simple Features" 
    )+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )



  #===============================================================================
  #---- st_intersects ----
    #--- interscting points and polygons ---#
    st_intersects(points, polygons)
    #--- intersection analysis in matrix form ---#
    st_intersects(points, polygons, sparse = FALSE)
      #point is on row and polygon is on column
    
    #--- intersection detection for line and points ---#
    st_intersects(points, lines, sparse = FALSE)
    #--- intersection detection for line and polygons ---#
    st_intersects(polygons, lines, sparse = FALSE)
  


#===============================================================================
#---- st_is_with_distance ----
#this function will identify if a point is within the specified distance or not
#we will start with an arbitrary points
    
    #setting seed for exact replication
    set.seed(38424738)
    # point set 1
    points_set_1 <-
      lapply(1:5, function(x) sf::st_point(runif(2))) %>% 
      sf::st_sfc() %>% sf::st_as_sf() %>% 
      dplyr::mutate(id = 1:nrow(.))
    #point set 2
    points_set_2 <-
      lapply(1:5, function(x) sf::st_point(runif(2))) %>% 
      sf::st_sfc() %>% sf::st_as_sf() %>% 
      dplyr::mutate(id = 1:nrow(.))    
    
    #--- plotting these two point sets ---#
    ggplot()+
      geom_sf_text(data = points_set_1, aes(label = id), color = "blue")+
      geom_sf_text(data = points_set_2, aes(label = id), color = "red")+
      theme_void()
    
    #We want to know which of the blue points (points_set_2) are located within 
    #0.2 from each of the red points (points_set_1).
    
    #--- create a buffer at 0.2 of point set 2 ---#
    buffer_2 <- st_buffer(points_set_2, dist = 0.2)

    #--- plot everything in one graph ---#
    ggplot()+
      geom_sf_text(data = points_set_1, aes(label = id), color = "blue")+
      geom_sf_text(data = points_set_2, aes(label = id), color = "red")+
      geom_sf(data = buffer_2, fill = NA, color = "red")+theme_bw()
    
    #--- Confirming visual result in matrix ---#
    st_is_within_distance(points_set_1, points_set_2, dist = 0.1)


    
#===============================================================================
#--- Spatial Cropping ----
    #--- data preparation ---#
    #--- Kansas county borders ---#
    KS_counties <-
      readRDS("Data/Chapter 3/KS_county_borders.rds") %>%
      sf::st_transform(32614)
    
    #--- High-Plains Aquifer boundary ---#
    hpa <- 
      sf::st_read("Data/Chapter 3/hp_bound2010.shp") %>%
      .[1, ] %>%
      sf::st_transform(st_crs(KS_counties))
    
    #--- all the irrigation wells in KS ---#
    KS_wells <- 
      readRDS("Data/Chapter 3/Kansas_wells.rds") %>%
      sf::st_transform(st_crs(KS_counties))
    
    #--- US railroads in the Mid West region ---#
    rail_roads_mw <- sf::st_read("Data/Chapter 3/tl_2015_us_rails.prj")
    
    
#===============================================================================
#--- Bounding box ----
    #--- get the bounding box of KS_wells ---#
    (
      bbox_KS_wells <- sf::st_bbox(KS_wells)  
    )
    #--- check the class ---#
    class(bbox_KS_wells)
    # converting the bbox into sfc ---#
    KS_wells_bbox_sfc <- sf::st_as_sfc(bbox_KS_wells) 
    #--- plotting the bbox and wells data ---#
    ggplot()+
      geom_sf(data = KS_wells_bbox_sfc, color = "red", fill = NA, 
              linewidth = 0.5 )+
      geom_sf(data = KS_wells, size = 0.5)+
      theme_void()

#--- Cropping data using st_crop function ----
    #--- crop hpa data to ks_wells data ---#
    hpa_cropped_wells <- st_crop(hpa, KS_wells)
    #plot the cropped data
    plot(hpa_cropped_wells)
    
    #--- plot everything again ---#
    ggplot()+
      geom_sf(data = hpa_cropped_wells, fill = "blue", alpha = 0.3)+
      geom_sf(data = KS_wells_bbox_sfc, color = "red", linewidth = 0.5, 
              fill = NA)+
      geom_sf(data = KS_wells, size = 0.8, color = "black")+
      theme_void()+
      labs(
        caption = "The bounding box of the irrigation wells in Kansas that overlie HPA"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )


    
#===============================================================================
#--- Spatial Sub-setting ----    
    #--- plot KS county, cropped HPA in one graph ---#
    ggplot()+
      geom_sf(data = KS_counties)+
      geom_sf(data = hpa_cropped_wells, fill = "blue", alpha = 0.2)+
      theme_void()+
      labs(
        caption = "Kansas portion of High-Plains Aquifer and Kansas counties"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic",size = 12)
      )
    
    
    
    #--- pull the hpa intersection KS counties only ---#
    hpa_intersecting_county <- KS_counties[hpa_cropped_wells,]
    #--- plot the hpa intersecting counties ---#
    ggplot()+
      geom_sf(data = hpa_intersecting_county, fill = NA)+
      geom_sf(data = hpa_cropped_wells, fill = "blue", alpha = 0.2)+
      theme_void()+
      labs(
        caption = "The results of spatially subsetting Kansas counties based on HPA boundary"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face= "italic", size = 12)
      )


    
    
    #--- identify complete covered KS counties ---#
    counties_within_hpa <- KS_counties[hpa_cropped_wells, op = st_within]
    #--- plot complete counties only ---#
    ggplot()+
      geom_sf(data = hpa_cropped_wells, fill = "blue", alpha = 0.1)+
      geom_sf(data = counties_within_hpa, color = "black", fill = NA)+
      theme_void()+
      labs(
        caption = "Counties completely within the HPA Area"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )


    #--- points(source) vs polygon(target) ---#
    # crop KS_wells by hpa_cropped_wells
    KS_wells_in_hpa <- KS_wells[hpa_cropped_wells,]
    ggplot()+
      geom_sf(data = hpa_cropped_wells, fill = "blue", alpha = 0.2)+
      geom_sf(data = KS_wells_in_hpa, size = 0.8)+
      theme_void()+
      labs(
        title = "A map of Kansas irrigation wells and HPA"
      )+
      theme(
        plot.title = element_text(hjust = 0.5, face= "bold", size = 12)
      )
      









dsfsdf







































































