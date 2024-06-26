# add_rst_to_basemaps.R 

library(sf)
library(osmdata) # OpenStreetMap
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
# library(rnaturalearthdata)
library(ggspatial)
library(readxl)

basemap <- readRDS(file = "basemap_noRST_20240620.Rds")

rst_raw <- read_xlsx("Summary of available RST data.xlsx", 
    sheet = "Trap Locations") %>%
    # The "notes" column has no header, fix
    rename(Notes = `...12`)
# 144 entries

# Filter to those with locations
rst_filtered <- rst_raw %>%
  # Either lat and lon have to be filled in, or UTM
  filter(
    (!is.na(Lat) & !is.na(Lon)) | 
    !is.na(`UTM (NAD 83)`) & 
    # UBC also has to have the data
    `Data with UBC` == "Yes"
    )

# And the complement:
rst_removed <- rst_raw %>%
  filter((is.na(Lat) | is.na(Lon)) & is.na(`UTM (NAD 83)`) |
    `Data with UBC` != "Yes")

### Note: based on a first plotting of these data, the Cougar Tailrace
###  lat-lon do not match up with the UTM, and the map does not line up with
###  the lat-lon positions. 
### According to UTM locations, the correct lat lon should be:
##    44.1326
##    -122.2439
### But the lat-lon in the record is:
##    44.7559 (off by ~0.6 degrees!)
##    -122.2358

# Lat-lon positions
rst_latlon <- rst_filtered %>%
  filter(!is.na(Lat) & !is.na(Lon)) %>%
  # Note: the lat-lon data for Cougar tailrace traps was not matching up with 
  # UTM, and not lining up on the map. Use UTM instead of lat-lon for CGR TR
  filter(Location != "Cougar tailrace")

rst_utm_cgrtr <- rst_filtered %>%
  filter(
    Location == "Cougar tailrace") %>% # ,
    # !is.na(`UTM (NAD 83)`)) %>%
  rowwise() %>%
  mutate(
    utm_e = as.numeric(
        strsplit(`UTM (NAD 83)`, fixed = TRUE, split = " ")[[1]][1]),
    utm_n = as.numeric(
        strsplit(`UTM (NAD 83)`, fixed = TRUE, split = " ")[[1]][2]),
    crs_orig = "UTM"
  ) %>%
  ungroup()

rst_utm_cgrtr$utm_n[which(is.na(rst_utm_cgrtr$`UTM (NAD 83)`))] <- 
  rst_utm_cgrtr$utm_n[1]
rst_utm_cgrtr$utm_e[which(is.na(rst_utm_cgrtr$`UTM (NAD 83)`))] <- 
  rst_utm_cgrtr$utm_e[1]

rst_utm_cgrtr <- rst_utm_cgrtr %>%
  st_as_sf(., coords = c(x = "utm_e", y = "utm_n"), remove = FALSE,
  crs = "+proj=utm +zone=10") %>%
  # Transform to the appropriate lat-lon CRS
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  ungroup() %>%
  mutate(
    "Lon" = sf::st_coordinates(.)[,1],
    "Lat" = sf::st_coordinates(.)[,2]
  )

# Filter to sites that use UTM positions, except CGR TR
rst_utm <- rst_filtered %>%
  filter((is.na(Lat) | is.na(Lon)) & !is.na(`UTM (NAD 83)`)) %>%
  filter(Location != "Cougar tailrace") %>%
  rowwise() %>%
  mutate(
    utm_e = as.numeric(
        strsplit(`UTM (NAD 83)`, fixed = TRUE, split = " ")[[1]][1]),
    utm_n = as.numeric(
        strsplit(`UTM (NAD 83)`, fixed = TRUE, split = " ")[[1]][2]),
    crs_orig = "UTM"
  )

# Add lat-lon geography to the UTM-projected spatial object
rst_utm_sf <- st_as_sf(rst_utm,
  coords = c(x = "utm_e", y = "utm_n"), remove = FALSE,
  crs = "+proj=utm +zone=10") %>%
  # Transform to the appropriate lat-lon CRS
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  ungroup() %>%
  mutate(
    "Lon" = sf::st_coordinates(.)[,1],
    "Lat" = sf::st_coordinates(.)[,2]
  )

rst_utm_latlon <- rst_utm_sf %>%
  bind_rows(
    st_as_sf((rst_latlon %>%
      mutate(crs_orig = "longlat")), 
    coords = c(x = "Lon", y = "Lat"), remove = FALSE,
    crs = "+proj=longlat +datum=WGS84")) %>%
  bind_rows(rst_utm_cgrtr)
# Correct number of rows is good

#### Summarize data according to years active, etc. ----------------------------
# ggplotly(basemap +
# # ggplot() + 
#   geom_sf(data = rst_utm_latlon, # %>% filter(crs_orig == "UTM"), 
#   inherit.aes = FALSE, 
#     aes(color = crs_orig, text = paste0("Name: ", Location, 
#         "\nInj.? ", `Injunction period`))))

# ggplotly(
#     ggplot() + geom_sf(data = rst_utm_sf, inherit.aes = FALSE, 
#         aes(color = crs_orig))
# )

#### Summarize data according to years active, etc.
rst_prepost_yrs <- rst_utm_latlon %>%
  group_by(Location, `Injunction period`, Trap, Operator) %>% 
#   rowwise() %>%
  summarize(
    unique_yrs_pre = paste(sort(unique(Year[`Injunction period` == "Pre"])),
      collapse = ", "),
    unique_ops_pre = paste(sort(unique(Operator[`Injunction period` == "Pre"])),
      collapse = ", "),
    unique_traps_pre = paste(sort(unique(Trap[`Injunction period` == "Pre"])),
      collapse = ", "),
    ## POST
    unique_yrs_post = paste(sort(unique(Year[`Injunction period` == "Post"])),
      collapse = ", "),
    unique_ops_post = paste(sort(unique(Operator[`Injunction period` ==
      "Post"])), collapse = ", "),
    unique_traps_post = paste(sort(unique(Trap[`Injunction period` == "Post"])),
      collapse = ", ")) %>%
# rst_prepost_yrs <- rst_utm_latlon %>%
#   group_by(Location, `Injunction period`, Trap, Operator) %>%
# #   rowwise() %>%
#   summarize(
#     min_yr = min(Year), 
#     max_yr = max(Year)) %>%
  ungroup() %>%
  group_by(Location) %>%
  mutate(
    prepost_composite = paste(sort(unique(`Injunction period`), decreasing = T),
      collapse = " & ")) %>%
  ungroup() %>%
  group_by(Location, prepost_composite) %>%
  summarize(
    yrs_pre = paste(unique(unique_yrs_pre), collapse = ""),
    ops_pre = paste(unique(unique_ops_pre), collapse = ""),
    traps_pre = paste(unique(unique_traps_pre), collapse = ""),
    yrs_post = paste(unique(unique_yrs_post), collapse = ""),
    ops_post = paste(unique(unique_ops_post), collapse = ""),
    traps_post = paste(unique(unique_traps_post), collapse = ""),

  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    # `Years active` = paste0(min_yr, "-", max_yr),
    plotly_map_text = paste0(
        "<b>", Location, "</b> (", tolower(prepost_composite), ")", 
        ifelse(prepost_composite == "Pre", paste(
          "\nYear(s): ", yrs_pre,
          "\nTrap type(s): ", traps_pre,
          "\nOperator(s): ", ops_pre, collapse = " "), 
        ifelse(prepost_composite == "Post", paste(
          "\nYear(s): ", yrs_post,
          "\nTrap type(s): ", traps_post,
          "\nOperator(s): ", ops_post, collapse = " "), 
          paste("\n<b>Pre-injunction</b>:",
            "\nYear(s): ", yrs_pre,
            "\nTrap type(s): ", traps_pre,
            "\nOperator(s): ", ops_pre,
            "\n<b>Post-injunction</b>:",
          "\nYear(s): ", yrs_pre,
          "\nTrap type(s): ", traps_pre,
          "\nOperator(s): ", ops_pre, collapse = " ") 
        )
        )
    )
  ) %>%
  ungroup() %>%
  mutate(prepost_composite = factor(prepost_composite,
    levels = c("Pre", "Post", "Pre & Post")))

# New weird error??
rst_prepost_yrs <- sf::st_cast(rst_prepost_yrs, "MULTIPOINT")
# WTF it worked????????

overview_ggplotly <- ggplotly(basemap +
# ggplot() + 
    geom_sf(data = rst_prepost_yrs, inherit.aes = FALSE, 
      alpha = 0.8,
      aes(
        color = prepost_composite,
        shape = prepost_composite,
        text = plotly_map_text), size = 2) +
    labs(
      color = "<b>Injunction period</b>\n(click a category to hide it\non the map, double-click to\nhide all others)",
      shape = "<b>Injunction period</b>\n(click a category to hide it\non the map, double-click to\nhide all others)") + 
    # Limit to just the study region
    coord_sf(xlim = c(-123.2336, -121.9263), ylim = c(43.3971, 44.9601)), # + 
    # theme(legend.position = "bottom"),
  tooltip = "text")

saveRDS(overview_ggplotly, file = "2024_06_26_rst_map_compiled.Rds")
# matrix(c(-123.2336, -121.9263, 43.3971, 45.6601),
#   byrow = T, ncol = 2)

# # Subbasin maps ----------------------------------------------------------------
# #### Summarize data according to years active, etc.
# nsantiam_zoommap <- basemap +
# # ggplot() +
#   geom_sf(data = rst_prepost_yrs,
#     # %>% filter(crs_orig == "UTM"),
#     alpha = 0.8,
#     aes(
#       color = inj_factor,
#       shape = inj_factor,
#       text = plotly_map_text), size = 1.5) +
#   coord_sf(xlim = c(-122.39244, -122.00830), ylim = c(44.69349, 44.72592)) +
#   scale_shape_manual(values = c(16, 18)) +
#   labs(
#     color = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)",
#     shape = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)")
# ggplotly(nsantiam_zoommap, tooltip = "text")

# # northsantiam_bb <- matrix(c(-123.19244, -121.69830, 44.34349, 44.92592),
# #   byrow = T, ncol = 2)

# ssantiam_zoommap <- basemap +
# # ggplot() +
#   geom_sf(data = rst_prepost_yrs, 
#     # %>% filter(crs_orig == "UTM"), 
#     alpha = 0.8,
#     aes(
#       color = `Injunction period`,
#       shape = `Injunction period`,
#       text = plotly_map_text), size = 1.5) +
#   coord_sf(xlim = c(-122.7, -122.375), ylim = c(44.3, 44.56)) + 
#   # coord_sf(xlim = c(-122.39244, -122.00830), ylim = c(44.69349, 44.72592)) + 
#   scale_shape_manual(values = c(16, 18)) + 
#   labs(
#     color = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)",
#     shape = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)")
# ggplotly(ssantiam_zoommap, tooltip = "text")

# mfork_zoommap <- basemap +
#   geom_sf(data = rst_prepost_yrs, # %>% filter(crs_orig == "UTM"), 
#   inherit.aes = FALSE,
#   size = 2,
#     aes(
#       shape = `Injunction period`,
#       color = Operator,
#       text = paste0(
#         Location, 
#         "\nMin-max operating years: ", `Year range`,
#         # "\nInj: ", `Injunction period`,
#         "\nTrap: ", Trap,
#         "\nOperator: ", Operator))) +
#   coord_sf(xlim = c(-123.0, -122.1), ylim = c(43.4, 44.03)) +
#   labs(shape = "", color = "Injunction period,Operator\n(click to hide categories)")

# #         min      max
# # x -123.02455 -122.122
# # y   43.41334   44.029
# ggplotly(mfork_zoommap, tooltip = "text")


# # Assign colnames that play nicely with osmdata package
# colnames(basin_bb) <- colnames(northsantiam_bb) <- c('min','max')
# rownames(basin_bb) <- rownames(northsantiam_bb) <- c('x','y')

# # Search for these using OSM
# southsantiam_bb <- getbb("South Santiam River")
# middle_fork_bb <- getbb("Middle Fork Willamette River")