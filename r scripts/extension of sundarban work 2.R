# --- 0. Setup: Install and Load Packages ---

# Install pacman if not already installed, then load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra,       # Handle raster data (modern alternative to 'raster')
  tidyverse,   # Data wrangling (dplyr, tidyr) and visualization (ggplot2)
  patchwork,   # Combine ggplot objects
  viridis      # Color palettes for plots
)

# --- 1. Configuration ---

# Define file paths for your LULC TIFFs
# IMPORTANT: Ensure these paths are correct relative to your R script's working directory.
# Example: If your script is in 'project_folder/' and tifs are in 'project_folder/Data/sundarban tifs/',
# then 'Data/sundarban tifs/Sundarban_rf_00.tif' is correct.
lulc_files <- list(
  "2000" = "Data/sundarban tifs/Sundarban_rf_00.tif",
  "2005" = "Data/sundarban tifs/Sundarban_rf_05.tif",
  "2010" = "Data/sundarban tifs/Sundarban_rf_10.tif",
  "2015" = "Data/sundarban tifs/Sundarban_rf_15.tif",
  "2020" = "Data/sundarban tifs/Sundarban_rf_21.tif" # Used as the base for prediction
)

# Define your LULC classes with dissolved categories:
# Original: 0=Water, 1=Forest, 2=Built-up, 3=Aquaculture, 4=Others
# New Classification:
# 0 = Water
# 1 = Forest
# 2 = Built-up & Others (combines original classes 2 and 4)
# 3 = Aquaculture
lulc_class_names <- c("Water", "Forest", "Built-up & Others", "Aquaculture")
num_lulc_classes <- length(lulc_class_names) # Now 4 classes

# Output file prefixes for predicted TIFFs
output_prefix <- "predicted_lulc_reclassified_"

# Define a consistent color palette for all plots with the new categories
custom_colors <- c(
  "Water" = "lightblue",
  "Forest" = "darkgreen",
  "Built-up & Others" = "orange", # New color for the combined category
  "Aquaculture" = "red"
)

# --- 2. Functions ---

#' Reclassify LULC Raster Categories
#'
#' Combines 'Built-up' (original class 2) and 'Others' (original class 4)
#' into a single new category, assigned value 2.
#' Original classes 0, 1, 3 remain unchanged.
#'
#' @param raster_obj A SpatRaster object with original LULC classes (0-4).
#' @return A SpatRaster object with reclassified LULC categories (0-3).
reclassify_lulc <- function(raster_obj) {
  # Define the reclassification matrix:
  # column 1: old value, column 2: new value
  reclass_matrix_combined <- matrix(c(
    0, 0, # Water remains Water
    1, 1, # Forest remains Forest
    2, 2, # Built-up becomes Built-up & Others
    3, 3, # Aquaculture remains Aquaculture
    4, 2  # Others becomes Built-up & Others (combined with original 2)
  ), ncol = 2, byrow = TRUE)
  
  # Apply reclassification.
  # 'right = TRUE' is standard. 'others = NA' handles any values not in matrix.
  reclassified_raster <- terra::classify(raster_obj, reclass_matrix_combined, right = TRUE, others = NA)
  return(reclassified_raster)
}


#' Calculate Transition Matrix
#'
#' Calculates the transition matrix between two LULC raster layers.
#'
#' @param lulc_prev A SpatRaster object of the previous LULC.
#' @param lulc_curr A SpatRaster object of the current LULC.
#' @param num_classes The total number of LULC classes (0-indexed).
#' @return A matrix representing the transition probabilities.
#'         Rows are 'from' classes, columns are 'to' classes.
calculate_transition_matrix <- function(lulc_prev, lulc_curr, num_classes) {
  # Get values as vectors, efficiently handling NAs by filtering
  # It's assumed lulc_prev and lulc_curr are already aligned.
  vals_prev <- terra::values(lulc_prev)
  vals_curr <- terra::values(lulc_curr)
  
  # Identify and filter out NA values from both vectors simultaneously
  valid_indices <- which(!is.na(vals_prev) & !is.na(vals_curr))
  vals_prev_filtered <- vals_prev[valid_indices]
  vals_curr_filtered <- vals_curr[valid_indices]
  
  # Convert to factors with all possible levels to ensure the table includes
  # rows/columns for classes even if they don't appear in the current transition.
  # This makes the table dimensions consistent (num_classes x num_classes).
  from_factor <- factor(vals_prev_filtered, levels = 0:(num_classes - 1))
  to_factor <- factor(vals_curr_filtered, levels = 0:(num_classes - 1))
  
  # Create a contingency table (counts of transitions)
  transition_counts <- table(from_factor, to_factor)
  
  # Convert counts to probabilities, where rows sum to 1 (from_class to to_class)
  transition_matrix <- prop.table(transition_counts, margin = 1)
  
  # Handle 'from' classes that did not appear in the historical data (rows summing to zero)
  # `prop.table` will produce NaNs for such rows. Assume they remain unchanged.
  nan_rows <- which(apply(transition_matrix, 1, function(x) any(is.nan(x))))
  if (length(nan_rows) > 0) {
    for (i in nan_rows) {
      transition_matrix[i, ] <- 0 # Reset row to zeros
      transition_matrix[i, i] <- 1 # Set diagonal to 1 (class stays the same)
    }
  }
  
  # Add descriptive row and column names
  rownames(transition_matrix) <- paste0("From_", lulc_class_names)
  colnames(transition_matrix) <- paste0("To_", lulc_class_names)
  
  return(transition_matrix)
}


#' Predict LULC using Markov Chain
#'
#' Predicts the next LULC map using a Markov chain transition matrix.
#' This function applies a deterministic prediction based on the highest probability.
#'
#' @param current_lulc_raster A SpatRaster object of the current LULC.
#' @param transition_matrix The 2D transition probability matrix (from_class, to_class).
#' @param num_classes The total number of LULC classes.
#' @return A new SpatRaster object with the predicted LULC classes.
predict_lulc_markov <- function(current_lulc_raster, transition_matrix, num_classes) {
  # Create a reclassification matrix required by terra::classify
  # This matrix maps 'from' values to 'to' values based on the highest probability
  reclass_matrix <- matrix(NA, nrow = num_classes, ncol = 2)
  
  # Populate the reclassification matrix
  for (i in 0:(num_classes - 1)) {
    # Get the row of probabilities for the current 'from' class (R is 1-indexed)
    probabilities <- transition_matrix[i + 1, ]
    
    # Determine the 'to' class with the highest probability
    # `which.max` returns the index of the first maximum value
    predicted_class <- which.max(probabilities) - 1 # Convert back to 0-indexed
    
    # Store the mapping: old_value -> new_value
    reclass_matrix[i + 1, 1] <- i
    reclass_matrix[i + 1, 2] <- predicted_class
  }
  
  # Apply the reclassification to the current LULC raster
  # `right = TRUE` handles bin edges for ranges (not strictly needed for exact matches but good practice)
  # `others = NA` ensures values not in `reclass_matrix` (e.g., existing NAs) remain NA.
  predicted_lulc_raster <- terra::classify(current_lulc_raster, reclass_matrix, right = TRUE, others = NA)
  
  return(predicted_lulc_raster)
}

#' Prepare Raster for ggplot
#'
#' Converts a SpatRaster object into a data frame suitable for ggplot2,
#' with LULC classes as factors and an added year label.
#'
#' @param raster_obj A SpatRaster object.
#' @param year_label A string representing the year for the data.
#' @return A data frame with x, y, LULC_Class, and Year columns.
prepare_for_ggplot <- function(raster_obj, year_label) {
  # Convert raster to data frame, including coordinates and removing NAs
  df <- as.data.frame(raster_obj, xy = TRUE, na.rm = TRUE)
  colnames(df)[3] <- "LULC_Class" # Rename the LULC column
  
  # Convert numeric LULC classes to named factors for better plotting
  df$LULC_Class <- factor(df$LULC_Class,
                          levels = 0:(num_lulc_classes - 1),
                          labels = lulc_class_names)
  df$Year <- factor(year_label) # Add year as a factor for consistent labels
  return(df)
}

#' Create LULC Map Plot
#'
#' Generates a ggplot object for a given LULC data frame.
#'
#' @param df_lulc A data frame prepared by `prepare_for_ggplot`.
#' @param plot_title A string for the plot's title.
#' @param show_legend A boolean, TRUE to show the legend, FALSE otherwise.
#' @return A ggplot object.
create_lulc_plot <- function(df_lulc, plot_title, show_legend = TRUE) {
  p <- ggplot() +
    geom_raster(data = df_lulc, aes(x = x, y = y, fill = LULC_Class)) +
    scale_fill_manual(values = custom_colors, name = "LULC Class") +
    labs(title = plot_title, x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5), # Center title
      axis.text = element_blank(),           # Remove axis text
      axis.ticks = element_blank(),          # Remove axis ticks
      axis.title = element_blank()           # Remove axis titles
    ) +
    coord_sf(expand = FALSE) # Important for spatial plots to remove extra whitespace
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}


# --- 3. Main Script: LULC Prediction Workflow ---

# Load LULC raster data, and reclassify immediately
cat("Loading and reclassifying LULC data...\n")
lulc_rasters <- list()
for (year_str in names(lulc_files)) {
  filepath <- lulc_files[[year_str]]
  cat(paste0("  Loading LULC for ", year_str, " from ", filepath, "...\n"))
  temp_raster <- terra::rast(filepath)
  
  # Apply reclassification to combine categories
  reclassified_temp_raster <- reclassify_lulc(temp_raster)
  
  # Ensure integer values and clip to the valid class range (0 to num_lulc_classes - 1)
  lulc_rasters[[year_str]] <- terra::clamp(round(reclassified_temp_raster),
                                           lower = 0, upper = num_lulc_classes - 1,
                                           values = TRUE)
}
cat("LULC data loaded and reclassified.\n")

# Calculate average transition matrix from historical data
cat("\nCalculating average transition matrix from historical data...\n")
all_transition_matrices <- list()
years_sorted <- sort(as.numeric(names(lulc_rasters)))

for (i in 1:(length(years_sorted) - 1)) {
  prev_year_str <- as.character(years_sorted[i])
  curr_year_str <- as.character(years_sorted[i+1])
  
  cat(paste0("  Calculating transition for ", prev_year_str, " to ", curr_year_str, "...\n"))
  
  # Retrieve rasters for current period
  lulc_prev_raster <- lulc_rasters[[prev_year_str]]
  lulc_curr_raster <- lulc_rasters[[curr_year_str]]
  
  # --- FIX START ---
  # Ensure rasters are spatially aligned (CRS, extent, resolution)
  # Replaced terra::compareGeom with individual checks to avoid previous error.
  if (!terra::same.crs(lulc_prev_raster, lulc_curr_raster)) {
    warning(paste0("  CRSs mismatch for ", prev_year_str, " and ", curr_year_str, ". Re-projecting ", curr_year_str, " to match."))
    lulc_curr_raster <- terra::project(lulc_curr_raster, lulc_prev_raster)
  }
  if (!all(terra::ext(lulc_prev_raster) == terra::ext(lulc_curr_raster))) {
    warning(paste0("  Extents mismatch for ", prev_year_str, " and ", curr_year_str, ". Cropping/extending ", curr_year_str, " to match."))
    # Extend/crop to match extent. Order matters: crop then extend.
    lulc_curr_raster <- terra::extend(terra::crop(lulc_curr_raster, lulc_prev_raster), lulc_prev_raster)
  }
  if (!all(terra::res(lulc_prev_raster) == terra::res(lulc_curr_raster))) {
    warning(paste0("  Resolutions mismatch for ", prev_year_str, " and ", curr_year_str, ". Resampling ", curr_year_str, " to match."))
    lulc_curr_raster <- terra::resample(lulc_curr_raster, lulc_prev_raster, method = "near")
  }
  # --- FIX END ---
  
  # Calculate and store the transition matrix for this period
  matrix_i <- calculate_transition_matrix(
    lulc_prev = lulc_prev_raster,
    lulc_curr = lulc_curr_raster,
    num_lulc_classes
  )
  all_transition_matrices[[paste0(prev_year_str, "_", curr_year_str)]] <- matrix_i
}

# Average all calculated transition matrices
if (length(all_transition_matrices) > 0) {
  # `Reduce("+", ...)` sums all matrices in the list
  avg_transition_matrix <- Reduce("+", all_transition_matrices) / length(all_transition_matrices)
} else {
  stop("Error: No historical transitions to average. Check your LULC files and years configuration.")
}

cat("\nAverage Transition Matrix (Reclassified Categories):\n")
print(round(avg_transition_matrix, 3)) # Print rounded for readability

# --- 4. LULC Prediction ---

cat("\nInitiating LULC predictions for future years (using reclassified categories)...\n")

# Start prediction from the last observed LULC map (2020) after reclassification
current_lulc_for_prediction <- lulc_rasters[["2020"]]
predicted_rasters <- list()

# Define years for prediction
prediction_years <- c("2025", "2030", "2035")

for (year in prediction_years) {
  cat(paste0("  Predicting LULC for ", year, "...\n"))
  predicted_lulc_year <- predict_lulc_markov(
    current_lulc_for_prediction,
    avg_transition_matrix,
    num_lulc_classes
  )
  predicted_rasters[[year]] <- predicted_lulc_year
  
  # Save the predicted raster to a TIFF file
  output_filename <- paste0(output_prefix, year, ".tif")
  terra::writeRaster(predicted_lulc_year, filename = output_filename,
                     overwrite = TRUE, datatype = "INT1U") # INT1U for 0-3 classes
  cat(paste0("  ", year, " LULC predicted and saved to '", output_filename, "'\n"))
  
  # Update current LULC for the next prediction iteration
  current_lulc_for_prediction <- predicted_lulc_year
}

cat("\nAll LULC predictions complete with reclassified categories.\n")


# --- 5. Visualization ---

cat("\nGenerating LULC visualization maps (with reclassified categories)...\n")

# Prepare data frames for observed 2020 (used as 2021 in original plot) and predicted years
lulc_df_2020_observed <- prepare_for_ggplot(lulc_rasters[["2020"]], "2020 (Observed)")
lulc_df_2025_predicted <- prepare_for_ggplot(predicted_rasters[["2025"]], "2025 (Predicted)")
lulc_df_2030_predicted <- prepare_for_ggplot(predicted_rasters[["2030"]], "2030 (Predicted)")
lulc_df_2035_predicted <- prepare_for_ggplot(predicted_rasters[["2035"]], "2035 (Predicted)")

# Create individual ggplot objects using the helper function
p_2020_obs <- create_lulc_plot(lulc_df_2020_observed, "Observed LULC: 2020", show_legend = TRUE)
p_2025_pred <- create_lulc_plot(lulc_df_2025_predicted, "Predicted LULC: 2025", show_legend = FALSE)
p_2030_pred <- create_lulc_plot(lulc_df_2030_predicted, "Predicted LULC: 2030", show_legend = FALSE)
p_2035_pred <- create_lulc_plot(lulc_df_2035_predicted, "Predicted LULC: 2035", show_legend = FALSE)


# Arrange plots side-by-side using patchwork
# Use '+' for side-by-side arrangement.
# `plot_layout(guides = "collect")` consolidates all legends into one.
combined_plot <- p_2020_obs + p_2025_pred  +
  plot_layout(guides = "collect") # Arrange in 2 rows for better fit

# Display the combined plot
print(combined_plot)


# Save the combined plot to a high-resolution PNG file
output_plot_filename <- "predicted_lulc_all_years_reclassified_combined.png"
ggsave(output_plot_filename,
       plot = combined_plot,
       width = 18, # Increased width for 4 plots
       height = 10, # Adjusted height
       dpi = 300
)
cat(paste0("Combined LULC plot (reclassified) saved to '", output_plot_filename, "'\n"))

cat("\nScript execution complete.\n")