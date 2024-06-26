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

basemap <- readRDS(file = "basemap_noRST_20240626.Rds")

rst_raw <- read_xlsx(
  path = "Summary of available RST data_estimatesForBlanks_20240626.xlsx", 
  sheet = "Trap Locations") %>%
  # The "notes" column has no header, fix
  rename(Notes = `...12`)
# 144 entries

# Filter to those with locations
rst_filtered <- rst_raw %>%
  # Either lat and lon have to be filled in, or UTM
  filter(
    (!is.na(Lat) & !is.na(Lon)) | 
    !is.na(`UTM (NAD 83)`) 
    # UBC also has to have the data
    # `Data with UBC` == "Yes"
    )

# And the complement:
rst_removed <- rst_raw %>%
  filter((is.na(Lat) | is.na(Lon)) & is.na(`UTM (NAD 83)`) 
    # `Data with UBC` != "Yes"
    )

# 0 here, nice!

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

#### Summarize data according to years active, etc.
rst_prepost_yrs <- rst_utm_latlon %>%
  group_by(Location) %>%
  arrange(Year) %>%
  group_by(Location, `Injunction period`) %>% # , Trap, Operator) %>% 
#   rowwise() %>%
  summarize(
    unique_ubchasdata_pre = paste(
      sort(unique(Year[
        `Injunction period` == "Pre" & `Data with UBC` == "Yes"])),
      collapse = ", "),
    unique_yrs_pre = paste(sort(unique(Year[`Injunction period` == "Pre"])),
      collapse = ", "),
    unique_ops_pre = paste(unique(Operator[`Injunction period` == "Pre"]),
      collapse = ", "),
    unique_traps_pre = paste(unique(Trap[`Injunction period` == "Pre"]),
      collapse = ", "),
    ## POST
    unique_ubchasdata_post = paste(
      sort(unique(Year[
        `Injunction period` == "Post" & `Data with UBC` == "Yes"])),
      collapse = ", "),
    unique_yrs_post = paste(sort(unique(Year[`Injunction period` == "Post"])),
      collapse = ", "),
    unique_ops_post = paste(unique(Operator[`Injunction period` ==
      "Post"]), collapse = ", "),
    unique_traps_post = paste(unique(Trap[`Injunction period` == "Post"]),
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
    data_pre = paste(sort(unique_ubchasdata_pre[unique_ubchasdata_pre != ""]), collapse = ", "), 
    yrs_pre = paste(sort(unique_yrs_pre[unique_yrs_pre != ""]), collapse = ", "),
    ops_pre = paste(unique_ops_pre[unique_ops_pre != ""], collapse = ", "),
    traps_pre = paste(unique_traps_pre[unique_traps_pre != ""],
      collapse = ", "),
    data_post = paste(sort(unique_ubchasdata_post[unique_ubchasdata_post != ""]), collapse = ", "), 
    yrs_post = paste(sort(unique_yrs_post[unique_yrs_post != ""]), collapse = ", "),
    ops_post = paste(unique_ops_post[unique_ops_post != ""], collapse = ", "),
    traps_post = paste(unique_traps_post[unique_traps_post != ""],
      collapse = ", ")
  ) %>%
  ungroup()
  
# YEAR RANGES ARE TOO MESSY!!! Esp. at LOP!!
# https://stackoverflow.com/questions/16911773/
#   collapse-runs-of-consecutive-numbers-to-ranges

findIntRuns <- function(run) {
  if (identical(numeric(0), run)) return("")
  rundiff <- c(1, diff(run))
  difflist <- split(run, cumsum(rundiff != 1))
  unlist(lapply(difflist, function(x) {
    # if(length(x) %in% 1:2) as.character(x) else paste0(x[1], "-", x[length(x)])
    if(length(x) == 1) as.character(x) else paste0(x[1], "-", x[length(x)])
  }), use.names = FALSE)
}

### Example use: 
# s <- "1,2,3,4,8,9,14,15,16,19"
# s2 <- as.numeric(unlist(strsplit(s, ",")))

# paste0(findIntRuns(s2), collapse=",")
# [1] "1-4,8,9,14-16,19"

rst_prepost_yrs <- rst_prepost_yrs %>%
  rowwise() %>%
  mutate(
    yrs_pre_ranges = paste(findIntRuns(
      as.numeric(unlist(strsplit(yrs_pre, ", ")))),
      collapse=","),
    yrs_post_ranges = paste(findIntRuns(
      as.numeric(unlist(strsplit(yrs_post, ", ")))),
      collapse=","),
    dat_pre_ranges = paste(findIntRuns(
      as.numeric(unlist(strsplit(data_pre, ", ")))),
      collapse=","),
    dat_post_ranges = paste(findIntRuns(
      as.numeric(unlist(strsplit(data_post, ", ")))),
      collapse=",")) %>%
  ungroup()

rst_plotting_df <- rst_prepost_yrs %>% 
  # Add plotly hover text 
  mutate(
    # `Years active` = paste0(min_yr, "-", max_yr),
    plotly_map_text = case_when(
      prepost_composite == "Pre" ~ paste0("<b>", Location, "</b> (",
        tolower(prepost_composite), "-injunction)",
        "\n<i>Year(s):</i> ", yrs_pre_ranges,
        "\n<i>Trap type(s):</i> ", traps_pre,
        "\n<i>Operator(s):</i> ", ops_pre,
        "\n<i>UBC has data for:</i> ", dat_pre_ranges),
      prepost_composite == "Post" ~ paste0("<b>", Location, "</b> (",
        tolower(prepost_composite), "-injunction)",
          "\n<i>Year(s):</i> ", yrs_post_ranges,
          "\n<i>Trap type(s):</i> ", traps_post,
          "\n<i>Operator(s):</i> ", ops_post,
          "\n<i>UBC has data for:</i> ", dat_post_ranges),
      prepost_composite == "Pre & Post" ~ paste0("<b>", Location, "</b> (",
        tolower(prepost_composite), "-injunction)",
        "\n<b>Pre-injunction</b>:",
        "\n<i>Year(s):</i> ", yrs_pre_ranges,
        "\n<i>Trap type(s):</i> ", traps_pre,
        "\n<i>Operator(s):</i> ", ops_pre,
        "\n<i>UBC has data for:</i> ", dat_pre_ranges,
        "\n<b>Post-injunction</b>:",
        "\n<i>Year(s):</i> ", yrs_post_ranges,
        "\n<i>Trap type(s):</i> ", traps_post,
        "\n<i>Operator(s):</i> ", ops_post,
        "\n<i>UBC has data for:</i> ", dat_post_ranges)
    )
  ) %>%
  mutate(prepost_composite = factor(prepost_composite,
    levels = c("Pre", "Post", "Pre & Post")))

    
    # paste0(
    #     "<b>", Location, "</b> (", tolower(prepost_composite), "-injunction)", 
    #     ifelse(prepost_composite == "Pre", paste(
    #       "\nYear(s): ", yrs_pre_ranges,
    #       "\nTrap type(s): ", traps_pre,
    #       "\nOperator(s): ", ops_pre,  
    #       "\nUBC has data: ", data_pre, collapse = ""),
    #     ifelse(prepost_composite == "Post", paste(
    #       "\nYear(s): ", yrs_post_ranges,
    #       "\nTrap type(s): ", traps_post,
    #       "\nOperator(s): ", ops_post,
    #       "\nUBC has data: ", data_post, collapse = ""),
    #       paste("\n<b>Pre-injunction</b>:",
    #         "\nYear(s): ", yrs_pre_ranges,
    #         "\nTrap type(s): ", traps_pre,
    #         "\nOperator(s): ", ops_pre,
    #         "\nUBC has data?: ", data_pre,
    #         "\n<b>Post-injunction</b>:",
    #         "\nYear(s): ", yrs_post_ranges,
    #         "\nTrap type(s): ", traps_post,
    #         "\nOperator(s): ", ops_post,
    #         "\nUBC has data: ", data_post, collapse = ""),
    #     )
    #   )
    # )
  # ) %>%
  # ungroup() %>%
  # mutate(prepost_composite = factor(prepost_composite,
  #   levels = c("Pre", "Post", "Pre & Post")))

# New weird error??
rst_plotting_df <- sf::st_cast(rst_plotting_df, "MULTIPOINT")
# WTF it worked????????

overview_ggplotly <- ggplotly(basemap +
# ggplot() + 
    geom_sf(data = rst_plotting_df, inherit.aes = FALSE, 
      alpha = 0.8,
      aes(
        color = prepost_composite,
        shape = prepost_composite,
        text = plotly_map_text), size = 2) +
    labs(
      color = "<b>Injunction period</b>\n(click a category to hide it\non the map, double-click to\nhide all others)",
      shape = "<b>Injunction period</b>\n(click a category to hide it\non the map, double-click to\nhide all others)") + 
    # Limit to just the study region
    scale_color_brewer(palette = "Dark2") + 
    coord_sf(xlim = c(-123.2336, -121.9263), ylim = c(43.3971, 44.9601)), # + 
    # theme(legend.position = "bottom"),
  tooltip = "text")

saveRDS(overview_ggplotly, file = "2024_06_26_v2_rst_map_compiled_estimatedlocs.Rds")
# matrix(c(-123.2336, -121.9263, 43.3971, 45.6601),
#   byrow = T, ncol = 2)

message("Written overview ggplotly object; subbasin specific commands below have not been run")
break

# # Subbasin maps ----------------------------------------------------------------
#### Summarize data according to years active, etc.
nsantiam_zoommap <- basemap +
# ggplot() +
  geom_sf(data = rst_prepost_yrs,
    # %>% filter(crs_orig == "UTM"),
    alpha = 0.8,
    aes(
      color = inj_factor,
      shape = inj_factor,
      text = plotly_map_text), size = 1.5) +
  coord_sf(xlim = c(-122.39244, -122.00830), ylim = c(44.69349, 44.72592)) +
  scale_shape_manual(values = c(16, 18)) +
  labs(
    color = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)",
    shape = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)")
ggplotly(nsantiam_zoommap, tooltip = "text")

# northsantiam_bb <- matrix(c(-123.19244, -121.69830, 44.34349, 44.92592),
#   byrow = T, ncol = 2)

ssantiam_zoommap <- basemap +
# ggplot() +
  geom_sf(data = rst_prepost_yrs, 
    # %>% filter(crs_orig == "UTM"), 
    alpha = 0.8,
    aes(
      color = `Injunction period`,
      shape = `Injunction period`,
      text = plotly_map_text), size = 1.5) +
  coord_sf(xlim = c(-122.7, -122.375), ylim = c(44.3, 44.56)) + 
  # coord_sf(xlim = c(-122.39244, -122.00830), ylim = c(44.69349, 44.72592)) + 
  scale_shape_manual(values = c(16, 18)) + 
  labs(
    color = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)",
    shape = "Injunction period\n(click a category to hide it\non the map, double-click to\nhide all others)")
ggplotly(ssantiam_zoommap, tooltip = "text")

mfork_zoommap <- basemap +
  geom_sf(data = rst_prepost_yrs, # %>% filter(crs_orig == "UTM"), 
  inherit.aes = FALSE,
  size = 2,
    aes(
      shape = `Injunction period`,
      color = Operator,
      text = paste0(
        Location, 
        "\nMin-max operating years: ", `Year range`,
        # "\nInj: ", `Injunction period`,
        "\nTrap: ", Trap,
        "\nOperator: ", Operator))) +
  coord_sf(xlim = c(-123.0, -122.1), ylim = c(43.4, 44.03)) +
  labs(shape = "", color = "Injunction period,Operator\n(click to hide categories)")

#         min      max
# x -123.02455 -122.122
# y   43.41334   44.029
ggplotly(mfork_zoommap, tooltip = "text")


# Assign colnames that play nicely with osmdata package
colnames(basin_bb) <- colnames(northsantiam_bb) <- c('min','max')
rownames(basin_bb) <- rownames(northsantiam_bb) <- c('x','y')

# Search for these using OSM
southsantiam_bb <- getbb("South Santiam River")
middle_fork_bb <- getbb("Middle Fork Willamette River")