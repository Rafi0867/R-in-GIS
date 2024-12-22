# to learn more about the project description visit:
# https://tmieno2.github.io/R-as-GIS-for-Economists-Quarto/chapters/01-Demonstration.html


# installing packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  dplyr, # data wrangling
  dataRetrieval, # download USGS NWIS data
  lubridate, # Date object handling
  lfe, # fast regression with many fixed effects
  tmap, # mapping
  ggplot2 # for plotting
)

#--- first loading the shape files of the locations ---#
three_counties <-
  sf::st_read(dsn = "Data/chapter 1", layer = "urnrd") %>%
  #--- project to WGS84/UTM 14N ---#
  sf::st_transform(32614)
    # here, dsn is the data set location.

#--- importing ground water data ---#
#--- groundwater pumping data ---#
(
  urnrd_gw <- readRDS("Data/chapter 1/urnrd_gw_pumping.rds")
)


#--- converting data co-ordinates ---#
urnrd_gw_sf <-
  urnrd_gw %>%
  #--- convert to sf ---#
  st_as_sf(coords = c("lon", "lat")) %>%
  #--- set CRS WGS UTM 14 (you need to know the CRS of the coordinates to do this) ---#
  st_set_crs(32614)

#--- now sf ---#
urnrd_gw_sf


#--- map the three counties ---#
ggplot() +
  geom_sf(data = three_counties) +
  geom_sf(data = three_counties, fill = "blue", alpha = 0.3) +
  theme_void()

#--- download groundwater level data ---#
NE_gwl <-
  lapply(
    1990:1995,
    \(year) {
      dataRetrieval::readNWISdata(
        stateCd = "Nebraska",
        startDate = paste0(year, "-01-01"),
        endDate = paste0(year + 1, "-01-01"),
        service = "gwlevels"
      )
    }
  ) %>%
  dplyr::bind_rows() %>%
  dplyr::select(site_no, lev_dt, lev_va) %>%
  dplyr::rename(date = lev_dt, dwt = lev_va) %>%
  dplyr::filter(!is.na(dwt))

#--- take a look ---#
head(NE_gwl, 10)


#--- Average depth to water table in March ---#
NE_gwl_march <-
  NE_gwl %>%
  dplyr::mutate(
    date = as.Date(date),  #it actually format the existing date variable 
    month = lubridate::month(date), # extracts the month from new date variable
    year = lubridate::year(date), #extracts month in the same way
  ) %>%
  #--- select observation in March ---#
  dplyr::filter(year <= 1995, month == 3) %>% #filter out observations outside the range
  #--- gwl average in March ---#
  dplyr::group_by(site_no, year) %>% #create a temp group by which it will summarize
  dplyr::summarize(dwt = mean(dwt)) #summarized the grouped data

#--- take a look ---#
head(NE_gwl_march, 10)



#--- get the list of site ids ---#
NE_site_ls <- NE_gwl$site_no %>% unique()
  #only captures unique values for site no. and make a list

#--- get the locations of the site ids ---#
sites_info <-
  readNWISsite(siteNumbers = NE_site_ls) %>% #using the above list, collect info from web
  dplyr::select(site_no, dec_lat_va, dec_long_va) %>%
  #--- turn the data into an sf object ---#
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>%
  #--- NAD 83 ---#
  sf::st_set_crs(4269) %>%
  #--- project to WGS UTM 14 ---#
  sf::st_transform(32614) %>%
  #--- keep only those located inside the three counties ---#
  .[three_counties, ]


buffers <- sf::st_buffer(sites_info, dist = 2 * 1609.34) # in meter

#plotting wells and monitoring site with 2 miles redius
ggplot() +
  geom_sf(data = three_counties) +
  geom_sf(data = sites_info, size = 0.5) +
  geom_sf(data = buffers, fill = NA, col = "red") +
  theme_void()

#--- find irrigation wells inside the buffer and calculate total pumping  ---#
pumping_nearby <- sf::st_join(buffers, urnrd_gw_sf)


(
  total_pumping_nearby <-
    pumping_nearby %>%
    sf::st_drop_geometry() %>%
    #--- calculate total pumping by monitoring well ---#
    dplyr::group_by(site_no, year) %>%
    dplyr::summarize(nearby_pumping = sum(vol_af, na.rm = TRUE)) %>%
    #--- NA means 0 pumping ---#
    dplyr::mutate(
      nearby_pumping = ifelse(is.na(nearby_pumping), 0, nearby_pumping)
    )
)

#--- regression-ready data ---#
reg_data <-
  NE_gwl_march %>%
  #--- pick monitoring wells that are inside the three counties ---#
  dplyr::filter(site_no %in% unique(sites_info$site_no)) %>%
  #--- merge with the nearby pumping data ---#
  dplyr::left_join(., total_pumping_nearby, by = c("site_no", "year")) %>%
  #--- lead depth to water table ---#
  dplyr::arrange(site_no, year) %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(
    #--- lead depth ---#
    dwt_lead1 = dplyr::lead(dwt, n = 1, default = NA, order_by = year),
    #--- first order difference in dwt  ---#
    dwt_dif = dwt_lead1 - dwt
  )

#--- take a look ---#
dplyr::select(reg_data, site_no, year, dwt_dif, nearby_pumping)


#--- OLS with site_no and year FEs (error clustered by site_no) ---#
(
  reg_dwt <-
    fixest::feols(
      dwt_dif ~ nearby_pumping | site_no + year,
      cluster = "site_no",
      data = reg_data
    )
)
