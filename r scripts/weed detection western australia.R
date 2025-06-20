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


# Load packages
library(terra)
library(readxl)
library(tidyverse)
library(lubridate)
library(patchwork)
library(viridis)

# Load raster
r <- rast("Data/trial/canola_plot.tif")
names(r) <- c("NDVI", "RGVI", "NDWI", "MSI", "GNDVI", "SAVI")
plot(r)

# Convert raster to dataframe for plotting
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)

# Load Excel time series
excel_path <- "Data/trial/canola_plot1.xlsx"  # <-- update this
df_excel <- read_excel(excel_path) %>%
  mutate(date = ymd(date)) %>%
  dplyr::select(date, NDVI, RGVI, NDWI, MSI, GNDVI, SAVI)

# Index names to loop over
index_names <- c("NDVI", "RGVI", "NDWI", "MSI", "GNDVI", "SAVI")


# Define the folder to save images
save_folder <- "Data/trial"

# Create the folder if it doesn't exist
if (!dir.exists(save_folder)) {
  dir.create(save_folder, recursive = TRUE)
}

# Loop through indices
for (index in index_names) {
  
  # --- A: Raster Map ---
  p_map <- ggplot(r_df, aes(x = x, y = y, fill = .data[[index]])) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(option = "D", name = index) +
    theme_void() +
    ggtitle(paste(index, "Spatial Map"))
  
  # --- B: Time Series ---
  p_ts <- ggplot(df_excel, aes(x = date, y = .data[[index]])) +
    geom_line(color = "steelblue") +
    geom_point() +
    labs(title = paste(index, "Time Series"), x = "Date", y = index) +
    theme_minimal()
  
  # --- C: Combine ---
  combined_plot <- p_map + p_ts + plot_layout(ncol = 2)
  print(combined_plot)
  
  # --- D: Save plot to folder ---
  ggsave(
    filename = file.path(save_folder, paste0(index, "_combined_plot.png")),
    plot = combined_plot,
    width = 10,
    height = 5
  )
}





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df <- as.data.frame(df_excel)
# Ensure the date column is in Date format
df$date <- as.Date(df$date)

# Reshape the data from wide to long format
# This is crucial for ggplot2 to plot multiple lines based on a 'parameter' column
df_long <- df %>%
  pivot_longer(
    cols = -date, # all columns except 'date'
    names_to = "parameter",
    values_to = "value"
  )

# Create the time series line chart
ggplot(df_long%>%filter(parameter != "MSI"), 
       aes(x = date, y = value, color = parameter)) +
  geom_line(size = 1) + # Draw lines
  geom_point() + # Add points for each data point
  labs(
    title = "Time Series of Remote Sensing Parameters",
    x = "Date",
    y = "Value",
    color = "Parameter"
  ) +
  theme_minimal() + # A clean theme
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    legend.position = "right" # Position the legend
  )




#================================================================================
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer, if you start from df_excel directly

# --- Assuming df_excel is already loaded and processed as in your GEE code workflow ---
# If you are running this code independently, you'll need to load df_excel first:
# excel_path <- "Data/trial/canola_plot1.xlsx"
# df_excel <- readxl::read_excel(excel_path) %>%
#   mutate(date = as.Date(date)) %>% # Ensure date is in Date format
#   dplyr::select(date, NDVI, RGVI, NDWI, MSI, GNDVI) # Make sure NDVI is selected

# 1. Filter for NDVI data only
df_ndvi <- df_excel %>%
  dplyr::select(date, NDVI, RGVI, NDWI, SAVI, MSI) %>% # Select only date and NDVI
  mutate(
    year = as.factor(format(date, "%Y")), # Extract year as a factor
    day_of_year = as.Date(format(date, "2000-%m-%d")) # Create a "day of year" for alignment
    # We use 2000 as a placeholder year for plotting
  )

# 2. Filter for 2023 and 2024 data (assuming your data spans these years)
# If your df_excel contains only 2023 data, you'll need to adjust or extend it.
# For demonstration, let's assume df_excel has data for both years.
# If it only has 2023, you might not see two lines unless you simulate 2024 data.

# Here's the plot
ggplot(df_ndvi, aes(x = day_of_year, y = NDVI, color = year)) +
  geom_line(size = 1.2) + # Increase line width for better visibility
  geom_point(size = 2) +  # Add points for clarity
  labs(
    title = "NDVI Time Series: 2023 vs. 2024",
    x = "Date (Day of Year)",
    y = "NDVI Value",
    color = "Year" # Legend title
  ) +
  scale_x_date(date_labels = "%b-%d") + # Format x-axis to show Month-Day
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )









#-------------------------------------------------------------------------------
# classification of weed using points

library(tidyverse)
library(sf)
library(geojsonsf)
library(lubridate)

# STEP 1: Load CSV and convert geometry
df <- read_csv("Data/trial/canola_points_area2.csv")
df_sf <- geojson_sf(df$.geo) %>%
  bind_cols(df %>% dplyr::select(-.geo))%>%
  mutate(month = month(date))

# STEP 2: Clean and classify
df_sf <- df_sf %>%
  mutate(date = ymd(date),
         month = format(date, "%Y-%m"),
         Class = case_when(
           NDVI < 0.1 ~ "Soil",
           RedEdge >= 0.45 & RedEdge <= 0.55 ~ "Weed",
           RedEdge > 0.55 ~ "Canola"
         ))

# STEP 3: Define manual colors
class_colors <- c(
  "Soil" = "yellow",
  "Weed" = "red",
  "Canola" = "blue"
)

# STEP 4: Plot classified map
ggplot(df_sf) +
  geom_sf(aes(color = Class), size = 2) +
  scale_color_manual(values = class_colors) +
  facet_wrap(~month) +
  labs(color = "Class") +
  theme_void()



# STEP 5: Plot
# Check and set CRS if missing
if (is.na(crs(r))) {
  crs(r) <- "EPSG:4326"  # Or use the correct CRS you know from GEE
}
r_boundary <- st_as_sf(as.polygons(ext(r), crs = crs(r))) # Convert extent to sf polygon


ggplot() +
  geom_sf(data = df_sf, aes(color = Class), size = 2) +
  geom_sf(data = r_boundary, fill = NA, color = "black", linewidth = 1) +
  scale_color_manual(values = class_colors) +
  facet_wrap(~month) +
  labs(title = "Monthly Crop Classification (NDVI + RedEdge)",
       color = "Class") +
  theme_void()




# another way 
points <- read_sf("Data/trial/points/canola_points.shp")
area <- read_sf("Data/trial/area/Rectangle_Shape_Export.shp")

ggplot() +
  geom_sf(data = df_sf, aes(color = Class), size = 2) +
  geom_sf(data = area, fill = NA, color = "black", linewidth = 1) +
  scale_color_manual(values = class_colors) +
  facet_wrap(~month) +
  labs(title = "Monthly Crop Classification (NDVI + RedEdge)",
       color = "Class") +
  theme_void()









# creating NDVI map for area2 using tif file
area2 <- rast("Data/trial/area2.tif")
plot(area2)
# STEP 2: Convert to data frame for ggplot (e.g., for NDVI layer only)
df_area2 <- as.data.frame(area2, xy = TRUE)


# STEP 2.1: Plot with ggplot
ggplot(df_area2, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_viridis_c(name = "NDVI") +
  coord_equal() +
  theme_void() +
  labs(title = "NDVI Map - Area 2")


# STEP 2.2: Plot with ggplot
ggplot(df_area2, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_viridis_c( option = "D",  #from A to E
    name = "NDVI",
    breaks = seq(0.39, 0.60, by = 0.04) # Adjust start, end, and 'by' value as needed
  ) +
  coord_equal() +
  theme_void() +
  labs(title = "NDVI Map - Area 2")

# Option 2.3: Simple two-color gradient (e.g., from red to green)
ggplot(df_area2, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_gradient(
    low = "red",     # Color for low NDVI values
    high = "darkgreen", # Color for high NDVI values
    name = "NDVI",
    breaks = seq(0.40, 0.60, by = 0.02)
  ) +
  coord_equal() +
  theme_void() +
  labs(title = "NDVI Map - Area 2")

# Option 2.4: Multi-color gradient (e.g., often used for spectral indices)
# You might need to install and load the 'RColorBrewer' package for some palettes
# install.packages("RColorBrewer")
# library(RColorBrewer)

ggplot(df_area2, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_gradientn(
    colors = c("darkred", "red", "yellow", "green", "darkgreen"), # Custom color sequence
    # Or use a palette from RColorBrewer: colors = brewer.pal(9, "YlGn") (Yellow-Green)
    # colors = brewer.pal(9, "RdYlGn") (Red-Yellow-Green, good for diverging)
    name = "NDVI",
    breaks = seq(0.40, 0.60, by = 0.05)
  ) +
  coord_equal() +
  theme_void() +
  labs(title = "NDVI Map - Area 2")









# sugar beets in USA Mapping ===================================================
# creating NDVI map for area2 using tif file
area2 <- rast("Data/trial/beat_23_area.tif")
plot(area2)
# STEP 2: Convert to data frame for ggplot (e.g., for NDVI layer only)
df_area2 <- as.data.frame(area2, xy = TRUE)


# STEP 2.1: Plot with ggplot
ggplot(df_area2, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_viridis_c(name = "NDVI") +
  coord_equal() +
  theme_void() +
  labs(title = "NDVI Map - Area 2")


# plotting NDVI
ggplot(df_area2) +
  geom_raster(aes(x = x, y = y, fill = NDVI)) +
  scale_fill_gradientn(
    colors = c("darkred", "red", "yellow", "green", "darkgreen"),
    name = "NDVI",
    breaks = seq(0.2668, 0.3716, by = 0.025),  # Adjust to your actual NDVI range
    limits = c(0.2668, 0.3716),                # Optional: force range
    guide = guide_colorbar(
      barwidth = 0.5, barheight = 10,        # Size of the legend bar
      ticks = TRUE,
      frame.colour = "black",
      title.position = "top"
    )
  ) +
  coord_equal() +
  theme_minimal() +
  labs(title = "NDVI Map - Area 2") 
  




# plotting red edge
ggplot(df_area2, aes(x = x, y = y, fill = RedEdge)) +
  geom_raster() +
  scale_fill_gradientn(
    colors = c("darkred", "red", "yellow", "green", "darkgreen"),
    name = "NDVI",
    breaks = seq(0.24, 0.35, by = 0.025),  # Adjust to your actual NDVI range
    limits = c(0.24, 0.34),                # Optional: force range
    guide = guide_colorbar(
      barwidth = 0.5, barheight = 10,        # Size of the legend bar
      ticks = TRUE,
      frame.colour = "black",
      title.position = "top"
    )
  ) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Red Edge Map - Area 2") 



  # we need to classify the red edge in three classes
    library(tidyverse)
    library(classInt)  # optional for Jenks breaks
    
    # STEP 1: Assume df_area2 has a column called RedEdge
    # Remove NA and scale RedEdge for clustering
    rededge_vals <- df_area2 %>%
      filter(!is.na(RedEdge)) %>%
      pull(RedEdge)
    
    # STEP 2: Use k-means to classify into 3 clusters
    set.seed(123)  # for reproducibility
    kmeans_result <- kmeans(rededge_vals, centers = 2)
    
    # STEP 3: Add cluster back to the data frame
    df_area2_clean <- df_area2 %>%
      filter(!is.na(RedEdge)) %>%
      mutate(RedEdge_Class = factor(kmeans_result$cluster))
    
    # STEP 4: Plot the classified map
    ggplot(df_area2_clean, aes(x = x, y = y, fill = RedEdge_Class)) +
      geom_raster() +
      scale_fill_manual(
        values = c("darkgreen", "orange", "red"),
        name = "RedEdge Class",
        labels = c("Low", "Medium", "High")
      ) +
      coord_equal() +
      theme_void() +
      labs(title = "Red Edge Classification via K-Means")

    
    
    
    
    
# creating a new classified data set and mapping the data
    # STEP 1: Create classified data frame
    classified_data <- df_area2 %>%
      mutate(
        Class = case_when(
          NDVI <= 0.31 ~ "Soil",
          RedEdge > 0.2753 ~ "Crop",
          RedEdge > 0.24 & RedEdge <= 0.2753 ~ "Weed",
          TRUE ~ NA_character_
        )
      )
    
    # STEP 2: Plot classified map
    ggplot(classified_data, aes(x = x, y = y, fill = Class)) +
      geom_raster() +
      scale_fill_manual(values = c("Soil" = "yellow", "Weed" = "red", "Crop" = "darkgreen")) +
      coord_equal() +
      theme_void() +
      labs(title = "Classified Map (Soil, Weed, Crop)", fill = "Class")
    
    
    
# combining all maps together
    # STEP 1: NDVI Map
    p_ndvi <- ggplot(classified_data, aes(x = x, y = y, fill = NDVI)) +
      geom_raster() +
      scale_fill_gradientn(
        colors = c("darkred", "red", "yellow", "green", "darkgreen"),
        name = "NDVI"
      ) +
      coord_equal() +
      theme_void() +
      labs(title = "NDVI Map")+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold")
      )
    
    # STEP 2: RedEdge Map
    p_re <- ggplot(classified_data, aes(x = x, y = y, fill = RedEdge)) +
      geom_raster() +
      scale_fill_gradientn(
        colors = c("darkred", "red", "yellow", "green", "darkgreen"),
        name = "RedEdge"
      ) +
      coord_equal() +
      theme_void() +
      labs(title = "Red Edge Map")+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold")
      )
    
    
    # STEP 3: Classified Map
    p_class <- ggplot(classified_data, aes(x = x, y = y, fill = Class)) +
      geom_raster() +
      scale_fill_manual(
        values = c("Soil" = "yellow", "Weed" = "red", "Crop" = "darkgreen"),
        name = "Class"
      ) +
      coord_equal() +
      theme_void() +
      labs(title = "Classified Map (Soil, Weed, Crop)")+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold")
      )
    
    
    # STEP 4: Combine with patchwork
    wrap_plots(p_ndvi, p_re, p_class, ncol = 2)
