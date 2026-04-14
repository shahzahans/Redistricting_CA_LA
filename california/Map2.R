# California Demographic Concentration by Race and Ethnicity
# Dot map: color = largest CVAP group, size = total CVAP

library(sf)
library(tidyverse)
library(scales)
library(tigris)

options(tigris_use_cache = TRUE)

dir.create("Data_Folder/Cali_DATA/congress", showWarnings = FALSE, recursive = TRUE)
dir.create("Data_Folder/Cali_DATA/cvap", showWarnings = FALSE, recursive = TRUE)

unzip(
  "Data_Folder/Cali_DATA/ca_cong_adopted_2025.zip",
  exdir = "Data_Folder/Cali_DATA/congress"
)

unzip(
  "Data_Folder/Cali_DATA/ca_cvap_2024_bg_csv.zip",
  exdir = "Data_Folder/Cali_DATA/cvap"
)

cong <- st_read("Data_Folder/Cali_DATA/congress/ab604/AB604.shp", quiet = TRUE)
cvap_bg <- read_csv("Data_Folder/Cali_DATA/cvap/ca_cvap_2024_bg.csv", show_col_types = FALSE)

ca_bg_shapes <- block_groups(
  state = "CA",
  year = 2020,
  class = "sf"
)

cvap_bg <- cvap_bg |>
  mutate(GEOID = as.character(GEOID))

ca_bg_shapes <- ca_bg_shapes |>
  mutate(GEOID = as.character(GEOID))

bg_map <- ca_bg_shapes |>
  left_join(cvap_bg, by = "GEOID")

bg_map <- bg_map |>
  mutate(
    total_cvap = CVAP_WHT24 + CVAP_BLK24 + CVAP_ASN24 + CVAP_HSP24,
    largest_group = case_when(
      CVAP_HSP24 >= CVAP_WHT24 & CVAP_HSP24 >= CVAP_BLK24 & CVAP_HSP24 >= CVAP_ASN24 ~ "Latino",
      CVAP_WHT24 >= CVAP_HSP24 & CVAP_WHT24 >= CVAP_BLK24 & CVAP_WHT24 >= CVAP_ASN24 ~ "White",
      CVAP_BLK24 >= CVAP_HSP24 & CVAP_BLK24 >= CVAP_WHT24 & CVAP_BLK24 >= CVAP_ASN24 ~ "Black",
      CVAP_ASN24 >= CVAP_HSP24 & CVAP_ASN24 >= CVAP_WHT24 & CVAP_ASN24 >= CVAP_BLK24 ~ "Asian",
      TRUE ~ "Other"
    )
  )

bg_pts <- st_centroid(bg_map)

bg_pts <- st_transform(bg_pts, st_crs(cong))
cong   <- st_transform(cong, st_crs(bg_pts))

cities <- tibble(
  city = c("San Francisco", "Sacramento", "Fresno", "Los Angeles", "San Diego"),
  lon  = c(-122.4194, -121.4944, -119.7871, -118.2437, -117.1611),
  lat  = c(37.7749, 38.5816, 36.7378, 34.0522, 32.7157)
)

cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(cong))

p_demo_dots <- ggplot() +
  geom_sf(data = cong, fill = "gray97", color = "white", linewidth = 0.35) +
  geom_sf(
    data = bg_pts,
    aes(color = largest_group, size = total_cvap),
    alpha = 0.65
  ) +
  geom_sf(data = cities_sf, color = "black", size = 1.4) +
  geom_sf_text(
    data = cities_sf,
    aes(label = city),
    size = 3.1,
    color = "black",
    fontface = "bold",
    nudge_y = c(30000, 30000, -30000, 30000, -30000)
  ) +
  scale_color_manual(
    values = c(
      "Latino" = "#d95f02",
      "White"  = "#7570b3",
      "Black"  = "#1b9e77",
      "Asian"  = "#e6ab02",
      "Other"  = "gray60"
    ),
    name = "Largest CVAP group"
  ) +
  scale_size_continuous(
    range = c(0.2, 2.8),
    labels = comma_format(),
    name = "Total CVAP"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "California Demographic Concentration by Race and Ethnicity",
    subtitle = "Dots show block groups; color marks the largest CVAP group and size shows total CVAP",
    caption = "Source: Redistricting Data Hub CVAP data, U.S. Census TIGER/Line, and 2025 adopted congressional districts"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 12)),
    plot.caption = element_text(size = 9, color = "gray45", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

p_demo_dots

ggsave(
  "california_demographic_dots_clean.png",
  plot = p_demo_dots,
  width = 10,
  height = 8,
  dpi = 300
)