# California Party Registration and Nonvoter Balance by Congressional District
# Left = registration balance
# Right = nonvoter balance

library(sf)
library(tidyverse)
library(scales)
library(tigris)
library(readr)
library(patchwork)

options(tigris_use_cache = TRUE)

# ----------------------------
# 1. Set up folders
# ----------------------------
dir.create("Data_Folder/Cali_DATA/l2_stats", showWarnings = FALSE, recursive = TRUE)
dir.create("Data_Folder/Cali_DATA/congress", showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 2. Unzip files
# ----------------------------
unzip(
  "Data_Folder/Cali_DATA/CA_l2_2024_gen_stats_2020block.zip",
  exdir = "Data_Folder/Cali_DATA/l2_stats"
)

unzip(
  "Data_Folder/Cali_DATA/ca_cong_adopted_2025.zip",
  exdir = "Data_Folder/Cali_DATA/congress"
)

# ----------------------------
# 3. Read L2 csv
# ----------------------------
l2_files <- list.files(
  "Data_Folder/Cali_DATA/l2_stats",
  recursive = TRUE,
  full.names = TRUE
)

csv_file <- l2_files[grepl("\\.csv$", l2_files, ignore.case = TRUE)][1]

l2_data <- read_csv(csv_file, show_col_types = FALSE)

# ----------------------------
# 4. Read congressional districts
# ----------------------------
cong <- st_read("Data_Folder/Cali_DATA/congress/ab604/AB604.shp", quiet = TRUE)

# ----------------------------
# 5. Download California census blocks
# ----------------------------
ca_blocks <- blocks(
  state = "CA",
  year = 2020,
  class = "sf"
)

# ----------------------------
# 6. Match IDs and keep needed columns
# ----------------------------
ca_blocks <- ca_blocks |>
  mutate(geoid20 = as.character(GEOID20))

l2_small <- l2_data |>
  mutate(geoid20 = as.character(geoid20)) |>
  transmute(
    geoid20,
    reg_dem = reg_party_democratic,
    reg_rep = reg_party_republican,
    voted_dem = voted_party_democratic,
    voted_rep = voted_party_republican,
    reg_all = reg_all,
    voted_all = voted_all
  )

# ----------------------------
# 7. Join L2 stats to blocks
# ----------------------------
block_map <- ca_blocks |>
  left_join(l2_small, by = "geoid20") |>
  filter(!is.na(reg_all), reg_all > 0)

# ----------------------------
# 8. Transform to district CRS
# ----------------------------
block_map <- st_transform(block_map, st_crs(cong))

# ----------------------------
# 9. Spatial join blocks to districts
# ----------------------------
blocks_with_district <- st_join(
  block_map,
  cong |> select(DISTRICT),
  left = FALSE
)

# ----------------------------
# 10. Aggregate to congressional districts
# ----------------------------
district_stats <- blocks_with_district |>
  st_drop_geometry() |>
  group_by(DISTRICT) |>
  summarise(
    reg_dem = sum(reg_dem, na.rm = TRUE),
    reg_rep = sum(reg_rep, na.rm = TRUE),
    voted_dem = sum(voted_dem, na.rm = TRUE),
    voted_rep = sum(voted_rep, na.rm = TRUE),
    reg_all = sum(reg_all, na.rm = TRUE),
    voted_all = sum(voted_all, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    reg_balance = if_else(
      reg_dem + reg_rep > 0,
      reg_dem / (reg_dem + reg_rep),
      NA_real_
    ),
    dem_not_voted = reg_dem - voted_dem,
    rep_not_voted = reg_rep - voted_rep,
    nonvote_total = dem_not_voted + rep_not_voted,
    nonvote_balance = if_else(
      nonvote_total > 0,
      dem_not_voted / nonvote_total,
      NA_real_
    )
  )

# ----------------------------
# 11. Join back to district polygons
# ----------------------------
cong_stats <- cong |>
  left_join(district_stats, by = "DISTRICT")

# ----------------------------
# 12. City labels
# ----------------------------
cities <- tibble(
  city = c("San Francisco", "Sacramento", "Fresno", "Los Angeles", "San Diego"),
  lon  = c(-122.4194, -121.4944, -119.7871, -118.2437, -117.1611),
  lat  = c(37.7749, 38.5816, 36.7378, 34.0522, 32.7157)
)

cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(cong_stats))

# ----------------------------
# 13. Bounding box for extra bottom padding
# ----------------------------
bbox <- st_bbox(cong_stats)

# ----------------------------
# 14. Common theme
# ----------------------------
map_theme <- theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "gray45", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

# ----------------------------
# 15. Left map: registration balance
# ----------------------------
p_reg <- ggplot(cong_stats) +
  geom_sf(
    aes(fill = reg_balance, alpha = reg_all),
    color = "white",
    linewidth = 0.35
  ) +
  geom_sf(data = cities_sf, color = "black", size = 1.2) +
  geom_sf_text(
    data = cities_sf,
    aes(label = city),
    size = 2.9,
    color = "black",
    fontface = "bold",
    nudge_y = c(30000, 30000, -30000, 30000, -30000)
  ) +
  scale_fill_gradient2(
    low = "#7F0000",
    mid = "#F7F3ED",
    high = "#08306B",
    midpoint = 0.5,
    breaks = c(0.25, 0.5, 0.75),
    labels = c("More R", "Even", "More D"),
    name = "Registration balance"
  ) +
  scale_alpha_continuous(
    range = c(0.45, 1),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"] - 50000, bbox["ymax"] + 30000),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Party Registration Balance",
    subtitle = "Blue = more registered Democrats; red = more registered Republicans",
    caption = "Source: L2 2024 general election statistics"
  ) +
  map_theme

# ----------------------------
# 16. Right map: nonvoter balance
# ----------------------------
p_nonvote <- ggplot(cong_stats) +
  geom_sf(
    aes(fill = nonvote_balance, alpha = nonvote_total),
    color = "white",
    linewidth = 0.35
  ) +
  geom_sf(data = cities_sf, color = "black", size = 1.2) +
  geom_sf_text(
    data = cities_sf,
    aes(label = city),
    size = 2.9,
    color = "black",
    fontface = "bold",
    nudge_y = c(30000, 30000, -30000, 30000, -30000)
  ) +
  scale_fill_gradient2(
    low = "#7F0000",
    mid = "#F7F3ED",
    high = "#08306B",
    midpoint = 0.5,
    breaks = c(0.25, 0.5, 0.75),
    labels = c("More R", "Even", "More D"),
    name = "Nonvoter balance"
  ) +
  scale_alpha_continuous(
    range = c(0.45, 1),
    guide = "none"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"] - 50000, bbox["ymax"] + 30000),
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Party Nonvoter Balance",
    subtitle = "Blue = more Democratic nonvoters; red = more Republican nonvoters",
    caption = "Source: L2 2024 general election statistics"
  ) +
  map_theme

# ----------------------------
# 17. Combine side by side
# ----------------------------
final_party_combo <- p_reg + p_nonvote +
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "California Party Registration and Nonvoter Balance by Congressional District",
    subtitle = "Left: registration balance | Right: nonvoter balance",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 12))
    )
  )

final_party_combo

# ----------------------------
# 18. Save combined figure
# ----------------------------
ggsave(
  "california_registration_vs_nonvoters_side_by_side.png",
  plot = final_party_combo,
  width = 18,
  height = 8,
  dpi = 300
)