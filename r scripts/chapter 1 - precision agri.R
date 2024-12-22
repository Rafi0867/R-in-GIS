# setup -----
# loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  dplyr, # data wrangling
  ggplot2, # for map creation
  fixest, # OLS regression
  patchwork # arrange multiple plots
)

#defining the theme of the map we will create further
theme_for_map <-
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_line(color = "transparent"),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )


# project demonstration----
#--- read the trial design data ---#
trial_design_16 <- readRDS("Data/chapter 1/trial_design.rds")

#--- map of trial design ---#
ggplot(data = trial_design_16) +
  geom_sf(aes(fill = factor(NRATE))) +
  scale_fill_brewer(name = "N", #define the legend title
                    palette = "OrRd", #define color pallet("Blues", "BuGn", 
                    #"BuPu", "GnBu", "Greens", "Greys"",PuBu", "PuBuGn", "PuRd", "Purples")
                    direction = 1) + #defines the direction of legend, assesnding/discending
  theme_for_map

#--- read yield data (sf data saved as rds) ---#
yield <- readRDS("Data/chapter 1/yield.rds")

#--- read NH3 data (GeoPackage data) ---#
NH3_data <- sf::st_read("Data/chapter 1/NH3.gpkg")

#--- read ec data (shape file) ---#
ec <- sf::st_read(dsn = "Data/chapter 1", "ec")



#--- yield map ---#
g_yield <-
  ggplot() +
  geom_sf(data = trial_design_16) +
  geom_sf(data = yield, aes(color = yield), size = 0.5) +
  scale_color_distiller(name = "Yield", palette = "Greens", direction = 1) +
  theme_for_map

#--- NH3 map ---#
g_NH3 <-
  ggplot() +
  geom_sf(data = trial_design_16) +
  geom_sf(data = NH3_data, aes(color = aa_NH3), size = 0.5) +
  scale_color_distiller(name = "NH3", palette = "BuPu", direction = 1) +
  theme_for_map

#--- NH3 map ---#
g_ec <-
  ggplot() +
  geom_sf(data = trial_design_16) +
  geom_sf(data = ec, aes(color = ec), size = 0.5) +
  scale_color_distiller(name = "EC", palette = "OrRd", direction = 1) +
  theme_for_map

#--- stack the figures vertically and display (enabled by the patchwork package) ---#
g_yield / g_NH3 / g_ec
