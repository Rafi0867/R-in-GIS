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
      shape = "Points"
    )























































































































