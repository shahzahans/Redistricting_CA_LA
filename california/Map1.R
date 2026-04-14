# California Congressional District Partisanship, 2024
# Subtitle: Democratic vote share by congressional district

library(sf)
library(tidyverse)
library(scales)
library(patchwork)

# make folders for unzipped data
dir.create("Data_Folder/Cali_DATA/congress", showWarnings = FALSE, recursive = TRUE)
dir.create("Data_Folder/Cali_DATA/precincts", showWarnings = FALSE, recursive = TRUE)

# unzip files
unzip(
  "Data_Folder/Cali_DATA/ca_cong_adopted_2025.zip",
  exdir = "Data_Folder/Cali_DATA/congress"
)

unzip(
  "Data_Folder/Cali_DATA/ca_2024_gen_prec.zip",
  exdir = "Data_Folder/Cali_DATA/precincts"
)

# optional: check extracted files
list.files("Data_Folder/Cali_DATA/congress", recursive = TRUE)
list.files("Data_Folder/Cali_DATA/precincts", recursive = TRUE)

# read files
cong <- st_read("Data_Folder/Cali_DATA/congress/ab604/AB604.shp", quiet = TRUE)
prec <- st_read("Data_Folder/Cali_DATA/precincts/ca_gen_2024_prec.geojson", quiet = TRUE)

# aggregate precinct presidential vote to congressional district
district_vote <- prec |>
  st_drop_geometry() |>
  mutate(CONG_DIST = as.character(CONG_DIST)) |>
  group_by(CONG_DIST) |>
  summarise(
    dem_votes = sum(G24PREDHAR, na.rm = TRUE),
    rep_votes = sum(G24PRERTRU, na.rm = TRUE),
    total_2party = dem_votes + rep_votes
  ) |>
  mutate(
    dem_share = if_else(total_2party > 0, dem_votes / total_2party, NA_real_)
  ) |>
  ungroup()

# join district vote data to district boundaries
cong_vote <- cong |>
  mutate(CONG_DIST = as.character(DISTRICT)) |>
  left_join(district_vote, by = "CONG_DIST")

# major California cities for reference
cities <- tibble(
  city = c("San Francisco", "Sacramento", "Fresno", "Los Angeles", "San Diego"),
  lon  = c(-122.4194, -121.4944, -119.7871, -118.2437, -117.1611),
  lat  = c(37.7749, 38.5816, 36.7378, 34.0522, 32.7157)
)

# convert cities to sf and match map projection
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(cong_vote))

# build map
p_vote <- ggplot(cong_vote) +
  geom_sf(aes(fill = dem_share), color = "white", linewidth = 0.35) +
  geom_sf(data = cities_sf, color = "black", size = 1.8) +
  geom_sf_text(
    data = cities_sf,
    aes(label = city),
    size = 3.5,
    color = "black",
    fontface = "bold",
    nudge_y = c(35000, 35000, -35000, 35000, -45000)
  ) +
  scale_fill_gradient2(
    low = "#7F0000",
    mid = "#F7F3ED",
    high = "#08306B",
    midpoint = 0.5,
    breaks = c(0.25, 0.5, 0.75),
    labels = c("75% R", "Even", "75% D"),
    name = "Partisan balance"
  ) +
  coord_sf(
    xlim = c(st_bbox(cong_vote)["xmin"] - 50000, st_bbox(cong_vote)["xmax"] + 50000),
    ylim = c(st_bbox(cong_vote)["ymin"] - 120000, st_bbox(cong_vote)["ymax"] + 50000),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "California Congressional District Partisanship, 2024",
    subtitle = "Democratic vote share by congressional district",
    caption = "Source: Redistricting Data Hub precinct results and 2025 adopted congressional districts"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 12)),
    plot.caption = element_text(size = 9, color = "gray35", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

p_vote

# save map
ggsave(
  "california_congressional_partisanship_clean.png",
  plot = p_vote,
  width = 10,
  height = 8,
  dpi = 300
)