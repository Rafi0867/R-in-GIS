#                             Owner's Note

# This chapter introduces raster data handling using the raster and terra 
# packages in R.

# Emphasizes that terra is the modern replacement for the older raster package.

# Explains that many spatial packages still depend on raster object classes.

# Covers how to convert between raster and terra objects.

# Highlights similarities and differences in function names between the two 
# packages.

# Focuses on the common task of extracting raster values using vector data 
# (covered in detail in Chapter 5).

# Mentions the stars package as useful for handling raster data with temporal 
# dimensions (e.g., PRISM, Daymet).

# Notes that stars supports dplyr-style data manipulation for spatio temporal 
# raster data (covered in Chapter 6).

# https://tmieno2.github.io/R-as-GIS-for-Economists-Quarto/chapters/04-RasterDataBasics.html

#  ==========================    PREAAMBLE    ==========================
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  ggplot2,
  terra,
  raster,
  mapview,
  dplyr,
  sf,
  lubridate
)


# load Iowa 2015 tiff file data
IA_cdl_2015 <- raster("Data/Chapter 4/IA_cdl_2015.tif")
plot(IA_cdl_2015)
IA_cdl_2016 <- raster("Data/Chapter 4/IA_cdl_2016.tif")

#--- stack the two ---#
(
  IA_cdl_stack <- stack(IA_cdl_2015, IA_cdl_2016)
)
plot(IA_cdl_stack)
class(IA_cdl_stack)

#--- putting two raster together ---#
IA_cdl_brick <- brick(IA_cdl_2015, IA_cdl_2016)
plot(IA_cdl_brick)


# Terra packages: Spatraster----------------------
# converting raster into terra package suitable SpatRaster format
IAA_cdl_2015_sr <- rast(IA_cdl_2015)
print(IAA_cdl_2015_sr)

#inspecting the new raster file
IAA_cdl_2015_sr
#this raster has only one layer since it is converted from the raw raster
#now lets convert the stacka nd bricked raster into Spatraster

IAA_cdl_2015_stack_sr <- rast(IA_cdl_stack)
IAA_cdl_2015_stack_sr
#now it has two layers since the input was a stacked raster of 2 objects
# similarly we can convert the brick into Spatraster

IAA_cdl_2015_brick_sr <- rast(IA_cdl_brick)
IAA_cdl_2015_brick_sr


  ## reverting the Spatraster into Raster -------------------------------
  IAA_cdl_2015_stack_sr %>%
    raster() %>%
    plot()
  #keep in mind that, even though SpatRaster has multiple layers, the resulting 
  #RasterLayer object has only the first of the multiple layers.
      
      #### another easier way to revert the SPatraster into raster--------------
      IA_sptoraster <- as(IAA_cdl_2015_stack_sr, "Raster")
      IA_sptoraster
      # This works for any Raster object and you do not have to pick the right 
      # function like above.




# Terra packages: SpatVector------------------------
#--- Illinois county boundary ---#
(
IL_county <- tigris::counties(
            state = "Illinois", 
            progress_bar = TRUE) %>%
          dplyr::select(STATEFP, COUNTYFP, ALAND)
)


# converting the sf object into spatVector 
IL_county_sv <- vect(IL_county)
IL_county_sv



# Reading different raster data files---------------------------
(
  IA_cdl_2015_sr <- rast("Data/Chapter 4/IA_cdl_2015.tif")
)

#--- the list of path to the files ---#
files_list <- c("Data/Chapter 4/IA_cdl_2015.tif", "Data/Chapter 4/IA_cdl_2016.tif")

#--- read the two at the same time ---#
(
  multi_layer_sr <- terra::rast(files_list)
)



# Writing raster data files ----------------------------------------
# saving the previously converted SpatRaster into a Geotiff file
writeRaster(IA_cdl_2015_sr, "Data/Chapter 4/IA_cdl_stack.tif", 
                   filetype = "GTiff", overwrite = TRUE)

# There are several options to export the raster data into different format
# The most popular ones are as follows:
#   NetCDF --> "*.nc" --> filetype = "CDF"
# for more details: https://www.rdocumentation.org/packages/raster/versions/3.0-12/topics/writeRaster


# saving multilayer object as Geotiff file
writeRaster(IA_cdl_stack, "Data/Chapter 4/IA_cdl_stack_2015.tif",
            filetype = "GTiff", overwrite = TRUE)





# Extracting information from Raster Data object -------------------------
crs(IA_cdl_2015_sr)
# subsetting ther raster
#--- index ---#
IAA_cdl_2015_stack_sr[[2]] # (originally IA_cdl_2016.tif)


# extracting cell values
values_from_rs <- values(IAA_cdl_2015_stack_sr)
values_from_rs[1:20,]




# Converting raster into data frame
IA_cdl_df <- as.data.frame(IA_sptoraster, xy = TRUE)
































































