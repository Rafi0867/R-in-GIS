if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  dplyr, # data wrangling
  data.table, # data wrangling
  tmap, # make maps
  mapview, # create an interactive map
  patchwork, # arranging maps
  ggplot2,
  rmapshaper
)
library(ggplot2)


#--- a dataset that comes with the sf package ---#
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

#--- plotting the nc region in map ---#
ggplot(nc) +
  geom_sf() +
  theme_void()

plot(nc%>%select(AREA))

#--- checking the data class ---#
class(nc)

#--- take a look at the data ---#
head(nc)


#--- take a look at the sfg package ---#
sf::st_geometry(nc[1, ])[[1]][[1]]
plot(sf::st_geometry(nc[15, ]))

#--- displyaing the simple feature geometry list-column ---#
dplyr::select(nc, geometry)



#===============================================================================
# Here we will learn about types of simple feature geometry with sf function.
# different types of geometry includes Line, polygon, multipolygon, point etc


#--- POINT ---#
a_point <- st_point(c(4,1))
  # The st_point() function creates a POINT object when supplied with a vector 
  # of two numeric values
  #--- check the class of the newly created item ---#
  class(a_point)
  plot(a_point) # plotting the point that we created.

#--- LINE STRING ---#
var<- rbind(c(2, 3), c(3, 4), c(3, 5), c(1, 5))
  #--- create a "LINESTRING" ---#
  a_linestring <- st_linestring(var)
  #--- check the class ---#
  class(a_linestring)
  plot(a_linestring)

  
#--- POLYGON ---#  
  #--- collection of points in a matrix form ---#
  p1 <- rbind(c(0, 0), c(0, 3), c(0, 4), c(4, 4), c(4, 0), c(0, 0))

  #creating the polygon
  a_polygon <- st_polygon(list(p1)) # the only exception is I need to specify the word list
  plot(a_polygon)  
  
  
#--- MULTI-POLYGON ---#
p2 <- rbind(c(4, 0), c(5, 0), c(5, 3), c(4, 2), c(4, 0))
  # create a multi-polygon
  multipolygon <- st_multipolygon(list(list(p1), list(p2)))
  # all the polygons should be in separate list
  # another final list is for the multipolygon
  plot(multipolygon)



#===============================================================================
# now we will; start learning about simple feature geometry list-column(sfc)

  #--- create an sfc ---#
  sfc_ex <- sf::st_sfc(list(a_point, a_linestring, a_polygon, multipolygon))
  plot(sfc_ex)

# to create an sf object, first we need to add a column to the data frame
  df_ex <- data.frame(name = c("A" , "B", "C", "D"))
# add the sfc object as a column
  df_ex$geometry <- sfc_ex
# look at the data
  df_ex
  #--- see what it looks like (this is not an sf object yet) ---#
  class(df_ex)
  # this one is still a data frame. not the sf function. we need to convert it
  # as a sf object
  #--- let R recognize the data frame as sf ---#
  sf_ex <- st_as_sf(df_ex)
  
  #--- see what it looks like ---#
  sf_ex
  class(sf_ex)
  plot(sf_ex)


#===============================================================================
#----     READING AND WRITING VECTOR DATA ----
  #--- Reading shape files ---#
  nc_loaded <- st_read("Data/Chapter 2/nc.shp")
  plot(nc_loaded)
  
  #--- writing to a shape file ---#
  # this is actually converting the above sf objects into a shape file
  st_write(
    nc_loaded,
    dsn = "Data/Chapter 2/trial1.shp",
    driver = "ESRI Shapefile",
    append = FALSE
  )
  #after running this code chunk, you will see there are 4 different files that 
  #has been created with the following extension ".dbf, .prj, .shp, .shx".
  #shp: This is the main file of a shape file, storing the geometric definitions 
  #     of features like points, lines, and polygons.
  #.shx: This is the index file for the shape file, providing quick access to the 
  #     geometric records within the .shp file.
  #.prj: This is a projection file that defines the coordinate system used 
  #     by the shape file.
  #.dbf: This is a dBASE database file that stores the attribute data associated 
  #     with the features in the shape file.
  
      #--- Now create a polygon and a line as st file and make a shape file ---#
      # creating lines
      line_points <- rbind(c(2,4), c(2,6))
      line_1 <- st_linestring(line_points)
      
      # creating polygons
      polygons_points <- rbind(c(0,0), c(0,4), c(3,6), c(4,0), c(0,0))
      polygons_1 <- st_polygon(list(polygons_points))
      
      # write as a shape file
      st_write(
        polygons_1,
        dsn = "Data/Chapter 2/trial2.shp",
        driver = "ESRI Shapefile",
        append = FALSE
      )
      

#===============================================================================
#---- working with alternative to shape file ----
      #--- write as a gpkg file ---#
      st_write(nc, dsn = "Data/nc.geojson", append = FALSE)
      
      #--- read a geojson file ---#
      nc <- sf::st_read("Data/nc.geojson")
      plot(nc$AREA)
      ggplot(nc)+
        geom_sf()+
        theme_void()
      
      #--- write as a gpkg file ---#
      sf::st_write(nc, dsn = "Data/nc.gpkg", append = FALSE)
      
      #--- read a gpkg file ---#
      nc <- sf::st_read("Data/nc.gpkg")
      
      ggplot(nc)+
        geom_sf()+
        theme_void()

      
#===============================================================================
#--- Projection of different coordinate system ----
      st_crs(nc)
#--- transforming from nad27 to wsg84 ---#
      nc_wgs84 <- st_transform(nc, 4326)
      # new object <- transform(old object, new object's EPSG number) 
      
      # printing the crs of the newly transformed object
      st_crs(nc_wsg84)
      
      ggplot(nc_wsg84)+
        geom_sf()+
        theme_void()
      
#--- convert again in NAD83 ---#
      nc_nad83 <- st_transform(nc, 26917)
      # see the structure
      st_crs(nc_nad83)
      ggplot(nc_nad83)+geom_sf()+theme_void()
      
      
#--- transform of multiple objects ---#
      nc_utm17N <- sf::st_transform(nc_wgs84, 26917)
      
#--- check if the transformation was successful ---#
      sf::st_crs(nc_utm17N)
#--- transform ---#
      nc_utm17N_2 <- sf::st_transform(nc_wgs84, sf::st_crs(nc_utm17N))
      
#--- check if the transformation was successful ---#
      st_crs(nc_utm17N_2)



#===============================================================================
#---- quick and interactive view of sf objects ----
      
      #--- quick view 
      plot(nc)
      #--- quickly plot only one variable from nc object
      plot(nc$AREA) # it is not the map. to get this info in map use the following
      plot(nc %>% select(AREA))
      
      
      #--- interactive view
      #--- DO NOT RUN (for polygons) ---#
      tm_shape(sf) +
        tm_polygons()
      #--- DO NOT RUN (for points) ---#
      tm_shape(sf) +
        tm_symbols()
      
      # creates a static map of NC where county boundaries are drawn
      (
        tm_nc_polygons <- tm_shape(nc) + tm_polygons()
      )
      # created an interactive map
      tmap_leaflet(tm_nc_polygons)


      
#===============================================================================
#---- Turning data frame with points into simple feature object ----
      # load irrigation wells registration data
      wells <- readRDS("Data/Chapter 2/well_registration.rds")
      # see data class
      class(wells)
      # see data structure
      str(wells)
      
      
      # convert data frame variable into sf object
      wells_sf <- st_as_sf(wells, coords = c("longdd", "latdd"))
      # see the data class again
      class(wells_sf)
      str(wells_sf)            
      # check the crs
      st_crs(wells_sf) #see the CRS is NA, we need to put the CRS
      # put the crs 4269
      st_crs(wells_sf) <- 4269
      # check it again
      st_crs(wells_sf)
      table(wells$nrdname)
      plot(wells_sf %>% select(acres) %>% 
             filter(wells_sf$nrdname == "Central Platte") %>%
             filter(acres >400))

      
#===============================================================================
#---- Conversion to and from sp objects ----
    # convert into sp object
      wells_sp <- as(wells_sf, "Spatial")
    # check the class
      class(wells_sp)

    
    # convert the sp object back to sf again
      wells_sf_2 <- st_as_sf(wells_sp)
    # check the class
      class(wells_sf_2)

      
#===============================================================================
#--- Non-spatial transformation of sf ----
   # using dplyr in the sf object to show the similarities
      #--- here is what the data looks like ---#
      dplyr::select(wells_sf, wellid, nrdname, acres, regdate, nrdname)
      
    #--- do some transformations ---#
      wells_sf %>%
        #--- select variables (geometry will always remain after select) ---#
        dplyr::select(wellid, nrdname, acres, regdate, nrdname) %>%
        #--- removes observations with acre < 30  ---#
        dplyr::filter(acres > 30) %>%
        #--- hectare instead of acre ---#
        dplyr::mutate(hectare = acres * 0.404686)     
  
      wells_sf%>%
        select(wellid, acres, nrdname, regdate)%>%
        filter(acres >30)%>%
        mutate(hectare = acres * 0.404)
      #--- summary by group ---#
      wells_by_nrd <-
        wells_sf[1:100, ] %>%
        #--- group by nrdname ---#
        dplyr::group_by(nrdname) %>%
        #--- summarize ---#
        dplyr::summarize(tot_acres = sum(acres, na.rm = TRUE))
      
      #--- take a look ---#
      wells_by_nrd      
      #--- remove geometry ---#
      wells_no_longer_sf <- sf::st_drop_geometry(wells_sf)
      
      #--- take a look ---#
      head(wells_no_longer_sf)      
      
      wells_no_longer_sf %>%
        #--- group by nrdname ---#
        dplyr::group_by(nrdname) %>%
        #--- summarize ---#
        dplyr::summarize(tot_acres = sum(acres, na.rm = TRUE))      

      
#===============================================================================
#---- using data table ----
      #--- convert an sf to data.table ---#
        wells_dt <- data.table::data.table(wells_sf)
      class(wells_dt)      
      
      #--- work on the first 10 ---#
      wells_dt[1:10, ]$geometry %>%
        sf::st_buffer(dist = 2) %>%
        head()
      
#===============================================================================
#--- Non interactive Geometrical Operation ----
     #--- creating buffer around points ---#
      #--- read wells location data ---#
      urnrd_wells_sf <-
        readRDS("Data/Chapter 2/urnrd_wells.rds") %>%
        #--- project to UTM 14N WGS 84 ---#
        st_transform(32614)
      
      #--- plot the points ---#
      ggplot(urnrd_wells_sf) +
        geom_sf(size = 0.4, color = "red") +
        theme_void()      
      #--- now lets create the buffer at "dist" m around the points---#
      wells_buffer <- st_buffer(urnrd_wells_sf, dist = 1500)
      
      #--- Plot the buffer ---#
      ggplot() +
        geom_sf(data = urnrd_wells_sf , size = 1.1, 
                color = "red") +
        geom_sf(data = wells_buffer %>% filter(acres>200), color = "darkgreen", 
                fill = "green", alpha = 0.2) +
        theme_void()

#===============================================================================
#--- Creating buffer around the polygons ----
  #--- load the data and select 3 counties ---#
      county_boundary <- readRDS("Data/Chapter 2/NE_county_borders.rds") %>%
        filter(NAME %in% c("Chase", "Dundy", "Perkins")) %>%
        st_transform(32614)
      
  #--- plot the county boundery polygons with previous well data ---#
      ggplot(county_boundary)+
        geom_sf()+
        geom_sf(data = county_boundary, aes(fill = NAME)) +
        #scale_fill_brewer(name = "County", palette = "RdYlGn") +
        geom_sf(data = urnrd_wells_sf, color = "red", size = 0.2)+
        theme_void()

  #--- creating buffer at 'dist" meter arounf the counties ---#
      NE_buffer <- st_buffer(county_boundary, dist = 2000)
      
      ggplot() +
        geom_sf(data = NE_buffer, fill = "blue", alpha = 0.3) +
        geom_sf(data = county_boundary, aes(fill = NAME)) +
        scale_fill_brewer(name = "County", palette = "RdYlGn") +
        theme_void()      
      
#--- calculating the area of a polygon ----
      #--- generate area by polygon ---#
      (
        NE_counties <- dplyr::mutate(county_boundary, 
                                     area = st_area(county_boundary))
      )
      # checking the class
      class(NE_counties$area)
      # we can not do any operation with this form. we need to transform
      # converting the area into a numeric format
        NE_counties <- dplyr::mutate(NE_counties, area = as.numeric(area))
      #chekc the class again
        class(NE_counties$area)

#--- finding the centeroid of the polygon ----
        #--- create centroids ---#
          NE_centroids <- st_centroid(NE_counties)
        # plot the centeroid with the polygon
        ggplot()+
          geom_sf(data = county_boundary)+
          geom_sf(data = NE_centroids)+
          theme_void()
      
      #----------------------     TRIAL     -----------------------------
        #create a buffer around the ceteroid and plot the wells within the buffer
        buffer <- st_buffer(NE_centroids, dist = 5000)  
        #plot everything
        ggplot()+
          geom_sf(data = county_boundary)+
          geom_sf(data = urnrd_wells_sf, color = "red")+
          geom_sf(data = buffer, color = "black", fill = NA)+
          theme_void()
      #-------------------------------------------------------------------
        

#--- calculating linear distance between points ---#
        st_distance(urnrd_wells_sf[1:5,], urnrd_wells_sf[1:5,])

        
#--- Combining features without resolving boundaries ---#
        NE_county_combine <- st_combine(county_boundary)
        # plot the data
        ggplot(NE_county_combine)+
          geom_sf()+
          theme_void()
        
#--- Combining features with resolving boundaries ---#
        NE_country_union <- st_union(county_boundary)
        # plot the data
        ggplot(NE_country_union)+
          geom_sf()+
          theme_void()
        
#--- simplifying map object for faster rendering ---#
        # load data
        IL_county <- st_read("Data/Chapter 2/IL_county_detailed.geojson")
        #plot the map data (only cook county)
        ggplot()+
          geom_sf(data = IL_county %>% filter(NAME %in% "Cook County"))+
          theme_void()
          #since the north east coener has some non linear parts, so lets
          #simplify this
        
        # create a data set that contains only Cook County
        Cook_county_IL <- IL_county %>%
                          filter(NAME == "Cook County")
        # simplify the border
        Cook_simple <- st_simplify(Cook_county_IL, dTolerance = 5000)
          #dTolerance is the level of simplification
        ggplot(Cook_simple)+
          geom_sf(color = "red", fill = NA)+
          theme_void()        
          
        
        #----------------------     TRIAL     -----------------------------
        # see how different tolerance works and looks like
        Cook1 <- st_simplify(Cook_county_IL, dTolerance = 1000)
        Cook2 <- st_simplify(Cook_county_IL, dTolerance = 2000)
        Cook3 <- st_simplify(Cook_county_IL, dTolerance = 3000)
        Cook4 <- st_simplify(Cook_county_IL, dTolerance = 4000)
        Cook5 <- st_simplify(Cook_county_IL, dTolerance = 5000)
        #Plot everything together
        ggplot()+
          geom_sf(data = Cook1, color = "red", fill = NA)+
          geom_sf(data = Cook2, color = "green", fill = NA)+
          geom_sf(data = Cook3, color = "blue", fill = NA)+
          geom_sf(data = Cook4, color = "black", fill = NA)+
          geom_sf(data = Cook5, color = "purple", fill = NA)+
          theme_void()
        
        ggplot(Cook_county_IL)+geom_sf()+theme_void() | ggplot(Cook1)+geom_sf()+theme_void() | 
        ggplot(Cook2)+geom_sf()+theme_void() | ggplot(Cook3)+geom_sf()+theme_void() | 
        ggplot(Cook4)+geom_sf()+theme_void() | ggplot(Cook5)+geom_sf()+theme_void()
        #-------------------------------------------------------------------
        
      # simplify illinois border
        IL_simplified <- st_simplify(IL_county, dTolerance = 2000)
      # plot everything together
        ggplot(IL_county)+geom_sf(fill = "blue", alpha = 0.2)+theme_void() | 
          ggplot(IL_simplified)+geom_sf(fill = "blue", alpha = 0.2)+theme_void()
        
      # using rmapshaper simplify the map
        IL_rmap <- ms_simplify(IL_county, keep = 0.005)
        #the lower the keep, the more simplified the map.
      #Plot everything again
        (
            ggplot(IL_county)+geom_sf(fill = "blue", alpha = 0.2)+theme_void()+
              labs(title = "Raw")+
              theme(plot.title = element_text(hjust = 0.6)) | 
              
            ggplot(IL_simplified)+geom_sf(fill = "blue", alpha = 0.2)+theme_void()+
              labs(title = "Simplified")+
              theme(plot.title = element_text(hjust = 0.6)) |
              
            ggplot(IL_rmap)+geom_sf(fill = "blue", alpha = 0.2)+theme_void()+
              labs(title = "RmapShaper")+
              theme(plot.title = element_text(hjust = 0.6))
        )
        
 #==============================================================================
        
        
# book chapter link: https://tmieno2.github.io/R-as-GIS-for-Economists-Quarto/chapters/02-VectorDataBasics.html#non-interactive-geometrical-operations
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

      
      
      
      
      
      
      
      