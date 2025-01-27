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
    rail_roads_mw <- sf::st_read("Data/Chapter 3/mw_railroads.geojson")
    plot(rail_roads_mw)
    
    
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
      

    #--- line source and polygon target ---#
    #first plot the rain roads map
    ggplot()+
      geom_sf(data = rail_roads_mw)+
      theme_void()
    #now plot the Kansas map on the above rail map
    ggplot()+
      geom_sf(data = KS_counties, color = "red", fill = NA, size = 1)+
      geom_sf(data = rail_roads_mw, color = "blue", fill = NA)+
      theme_void()+
      labs(
        title = "U.S. railroads and Kansas county boundaries"
      )+
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
      )
    #now crop the rail roads only for Kansas counties and drop othes
    KS_rail <- rail_roads_mw[KS_counties, ]
    #now plot like the last one
    ggplot()+
      geom_sf(data = KS_counties, color = "black", fill = NA, linewidth = 0.5)+
      geom_sf(data = KS_rail, color = "blue", fill = NA, linewidth = 1)+
      theme_void()+
      labs(
        title = "U.S. railroads and Kansas county boundaries"
      )+
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
      )
    
    
    #--- Polygon source vs point target ---#
    KS_counties_intersected <- KS_counties[KS_wells_in_hpa, ]  
    # plot the resulted counties
    ggplot()+
      geom_sf(data = KS_counties, fill = NA, color = "black")+
      geom_sf(data = KS_counties_intersected, fill = "blue", alpha = 0.3)+
      geom_sf(data = KS_wells_in_hpa, size = 1)+
      theme_void()+
      labs(
        caption = "Counties that have at least one well"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )

    
#===============================================================================
#--- Different Types of Spatial Join ----
      
    #---Case 1: points (target) vs polygons (source)---#
    #here we actually want to project fake corn price data to original wells data
    #lets create a fake county level corn price data
    KS_corn_price <-
      KS_counties %>%
      dplyr::mutate(corn_price = seq(3.2, 3.9, length = nrow(.))) %>%
      dplyr::select(COUNTYFP, corn_price)
    #plot the data
    ggplot()+
      geom_sf(data = KS_corn_price, aes(fill = corn_price))+
      scale_fill_viridis_c() +
      theme_void()
    # now we need to join wells data with price data
    KS_wells_price <- st_join(KS_wells, KS_corn_price)
    #now plot the final thing again
    ggplot()+
      geom_sf(data = KS_counties)+
      geom_sf(data = KS_wells_price, aes(color = corn_price))+
      scale_fill_viridis_c()+
      theme_void()+
      labs(
        caption = "Map of wells color-differentiated by corn price",
        color = "Corn Price"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )

    
    #---Case 2: polygons (target) vs points (source)---#
    #--- spatial join ---#
    KS_County_wells <- sf::st_join(KS_counties, KS_wells)
    plot(KS_County_wells)
    #--- take a look ---#
    dplyr::select(KS_County_wells, COUNTYFP, site, af_used)
    #summarizing the data
    KS_County_wells %>% 
      dplyr::group_by(COUNTYFP) %>% 
      dplyr::summarize(af_used = sum(af_used, na.rm = TRUE)) 
    #plot the data
    ggplot() +
      geom_sf(data = KS_counties) +
      geom_sf(data = KS_wells, aes(color = af_used), size = 0.2) +
      scale_color_viridis_c(name = "Groundwater Pumping (acre-feet)") +
      theme_void() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))+
      labs(
        title = "Map of wells color-differentiated by corn price"
      )
    
    
    #---Case 3: polygons (target) vs polygons (source)---#
    #load Iowa corn data
    IA_corn <- readRDS("Data/Chapter 3/IA_corn.rds")
    IA_corn
    #make a plot of counties color based on corn acres
    ggplot(IA_corn)+
      geom_sf(aes(fill = acres))+
      scale_fill_viridis_c(name = "Corn Acreage")+
      theme_void()+
      labs(
        caption = "Map of Iowa counties color-differentiated by corn planted acreage"
      )+
      theme(
        plot.caption  = element_text(hjust = 0.7, face = "italic", size = 12)
      )
    #import HUC units
    HUC_IA <- 
      sf::st_read("Data/Chapter 3/huc250k.shp") %>% 
      dplyr::select(HUC_CODE) %>% 
      #--- reproject to the CRS of IA ---#
      sf::st_transform(st_crs(IA_corn)) %>% 
      #--- select HUC units that overlaps with IA ---#
      .[IA_corn, ]
    #simply plot the huc values
    ggplot(HUC_IA) + 
      geom_sf() +
      theme_void()
    #plot the corn data on huc map
    ggplot()+
      geom_sf(data = HUC_IA)+
      geom_sf(data = IA_corn, aes(fill = acres), alpha = 0.4)+
      scale_fill_viridis_c()+
      theme_void()+
      labs(
        caption = "Map of HUC units superimposed on the counties in Iowas"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )
    
    #now join the two data sets
    IA_huc_corn <- st_join(HUC_IA, IA_corn)
    ggplot()+
      geom_sf(data = IA_huc_corn, aes(fill = acres))+
      scale_fill_viridis_c(name = "Acres")+
      theme_void()
    #filer one region
    #--- get the HUC unit with `HUC_CODE ==10170203`  ---#
    (
      temp_HUC_county <- filter(IA_huc_corn, HUC_CODE == 10170203)
    )
    #plot the region
    temp_HUC_county %>%
      dplyr::mutate(county_text = paste0("County Code: ", county_code)) %>%
      ggplot(.) +
      geom_sf() +
      facet_wrap(county_text ~ ., nrow = 2) +
      theme_void()

    
    #more advanced way to summarize data by county
    #--- sum ---#
    data <- aggregate(dplyr::select(KS_wells, af_used), KS_counties, FUN = sum)
    ggplot()+
      geom_sf(data = KS_counties)+
      geom_sf(data = data, aes(fill = af_used))+
      theme_void()
    
#===============================================================================
#--- Other topological relationships ----
    set.seed(29841)
    
    points_set_1 <-
      lapply(1:5, function(x) sf::st_point(runif(2))) %>%
      sf::st_sfc() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(id_1 = 1:nrow(.))
    
    points_set_2 <-
      lapply(1:5, function(x) sf::st_point(runif(2))) %>%
      sf::st_sfc() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(id_2 = 1:nrow(.))

    #--- plot point sets ---#
    ggplot() +
      geom_sf(data = sf::st_buffer(points_set_1, dist = 0.2), color = "red", 
              fill = NA) +
      geom_sf_text(data = points_set_1, aes(label = id_1), color = "red") +
      geom_sf_text(data = points_set_2, aes(label = id_2), color = "blue")+
      theme_minimal()
    
    #--- join point set 1 and 2 ---#
    sf::st_join(points_set_1, points_set_2, join = \(x, y) 
                st_is_within_distance(x, y, dist = 0.2))


#================================================================================
#--- Spatial intersection (cropping join) ----
    #--- we will use the previous toy example ---#
    ggplot() +
      geom_sf(data = polygons, aes(fill = polygon_name), alpha = 0.3) +
      scale_fill_discrete(name = "Polygons") +
      geom_sf(data = lines, aes(color = line_name)) +
      scale_color_discrete(name = "Lines") +
      theme_void()
    #--- finding the intersection of the lines and polygons ---#
    (
      intersections_lp <- 
        sf::st_intersection(lines, polygons) %>% 
        dplyr::mutate(int_name = paste0(line_name, "-", polygon_name))
    )
    #---plotting the intersection data ---#
    plot(intersections_lp)
      #seems like we found the intersections only in this data set
    ggplot() +
      #--- here are all the original polygons  ---#
      geom_sf(data = polygons, aes(fill = polygon_name), alpha = 0.3) +
      #--- here is what is returned after st_intersection ---#
      geom_sf(data = intersections_lp, aes(color = int_name), size = 1.5)+
      theme_void()
    
    #--- finding intersections of polygons ---#
    (
      intersections_pp <- 
        sf::st_intersection(polygons[c(1,3), ], polygons[2, ]) %>% 
        dplyr::mutate(int_name = paste0(polygon_name, "-", polygon_name.1))
    )
    #---plot the data ---#
    ggplot()+
      geom_sf(data = polygons, aes(fill = polygon_name), alpha = 0.2)+
      geom_sf(data = intersections_pp, aes(color = int_name), linewidth = 1)+
      theme_void()+
      labs(
        caption = "The outcome of the intersections of polygon 2 and polygons 1 and 3"
      )+
      theme(
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
      )
    
    
 #==============================================================================
#--- Calculating Area weighted Average ----
    (
      HUC_intersections <- 
        sf::st_intersection(HUC_IA, IA_corn) %>% 
        dplyr::mutate(huc_county = paste0(HUC_CODE, "-", county_code))
    )
    #--- inital plotting what kind of data we are going to get ---#
    ggplot()+
      geom_sf(data = HUC_intersections)
    
    #--- Now plot formally ---#
      ggplot()+
      geom_sf(data = HUC_intersections %>% filter(HUC_CODE == 10170203),
              aes(fill = huc_county))+
        theme_void()+
        labs(
          caption = "Intersections of a HUC unit and Iowa counties",
          fill = "HUC County Code"
        )+
        theme(
          plot.caption = element_text(hjust = 0.5, face = "italic", size = 12)
        )+
        scale_fill_viridis_d()
      
      #--- Now calculate the area weighted average ---#
      (
        HUC_aw_acres <- 
          HUC_intersections %>% 
          #--- get area ---#
          dplyr::mutate(area = as.numeric(st_area(.))) %>% 
          #--- get area-weight by HUC unit ---#
          dplyr::group_by(HUC_CODE) %>% 
          dplyr::mutate(weight = area / sum(area)) %>% 
          #--- calculate area-weighted corn acreage by HUC unit ---#
          dplyr::summarize(aw_acres = sum(weight * acres))
      )


















































