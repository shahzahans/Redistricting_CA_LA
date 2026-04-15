# California Voter Registration / Turnout Map
# Uses: CA_l2_2024_gen_stats_2020block.zip

library(sf)
library(tidyverse)
library(scales)
library(tigris)
library(readr)

options(tigris_use_cache = TRUE)

# ----------------------------
# 1. Set up folders
# ----------------------------
dir.create("Data_Folder/Cali_DATA/l2_stats", showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# 2. Unzip L2 file
# ----------------------------
unzip(
  "Data_Folder/Cali_DATA/CA_l2_2024_gen_stats_2020block.zip",
  exdir = "Data_Folder/Cali_DATA/l2_stats"
)

# ----------------------------
# 3. See what files are inside
# ----------------------------
l2_files <- list.files("Data_Folder/Cali_DATA/l2_stats", recursive = TRUE, full.names = TRUE)
print(l2_files)

# ----------------------------
# 4. Find the first CSV automatically
# ----------------------------
csv_file <- l2_files[str_detect(tolower(l2_files), "\\.csv$")][1]

if (is.na(csv_file)) {
  stop("No CSV file found inside the L2 zip.")
}

cat("\nReading file:\n", csv_file, "\n\n")

# ----------------------------
# 5. Read the CSV
# ----------------------------
l2_data <- read_csv(csv_file, show_col_types = FALSE)

# ----------------------------
# 6. Inspect column names
# ----------------------------
cat("\nColumn names:\n")
print(names(l2_data))

cat("\nFirst few rows:\n")
print(head(l2_data))

# ----------------------------
# 7. Try to identify key columns
# ----------------------------
col_names <- names(l2_data)

geoid_col <- col_names[str_detect(tolower(col_names), "geoid|block")][1]

registered_candidates <- col_names[str_detect(
  tolower(col_names),
  "reg|registered|registrants"
)]

turnout_candidates <- col_names[str_detect(
  tolower(col_names),
  "turnout|voted|ballot|votes_cast|cast"
)]

cat("\nPossible GEOID column:\n")
print(geoid_col)

cat("\nPossible registration columns:\n")
print(registered_candidates)

cat("\nPossible turnout / votes columns:\n")
print(turnout_candidates)

# ----------------------------
# 8. Pick likely fields
# ----------------------------
# You may need to edit these after seeing printed column names
registered_col <- registered_candidates[1]
turnout_col    <- turnout_candidates[1]

if (is.na(geoid_col)) {
  stop("Could not identify a GEOID/block column automatically.")
}

if (is.na(registered_col) && is.na(turnout_col)) {
  stop("Could not identify likely registration or turnout columns automatically.")
}

# ----------------------------
# 9. Keep only needed columns and clean
# ----------------------------
l2_small <- l2_data |>
  mutate(GEOID = as.character(.data[[geoid_col]])) |>
  transmute(
    GEOID,
    registered = if (!is.na(registered_col)) as.numeric(.data[[registered_col]]) else NA_real_,
    turnout    = if (!is.na(turnout_col)) as.numeric(.data[[turnout_col]]) else NA_real_
  )

# create turnout rate if possible
l2_small <- l2_small |>
  mutate(
    turnout_rate = case_when(
      !is.na(registered) & !is.na(turnout) & registered > 0 ~ turnout / registered,
      TRUE ~ NA_real_
    )
  )

cat("\nSummary of cleaned data:\n")
print(summary(l2_small))

# ----------------------------
# 10. Download California census block geometry
# ----------------------------
# This may take a little time
ca_blocks <- blocks(
  state = "CA",
  year = 2020,
  class = "sf"
)

# ----------------------------
# 11. Match GEOID type
# ----------------------------
ca_blocks <- ca_blocks |>
  mutate(GEOID = as.character(GEOID20))

# ----------------------------
# 12. Join L2 stats to block geometry
# ----------------------------
block_map <- ca_blocks |>
  left_join(l2_small, by = "GEOID")

# ----------------------------
# 13. Filter to blocks with useful values
# ----------------------------
block_map <- block_map |>
  filter(!is.na(registered) | !is.na(turnout) | !is.na(turnout_rate))

# ----------------------------
# 14. Create centroids for dot map
# ----------------------------
block_pts <- st_centroid(block_map)

# ----------------------------
# 15. California city labels
# ----------------------------
cities <- tibble(
  city = c("San Francisco", "Sacramento", "Fresno", "Los Angeles", "San Diego"),
  lon  = c(-122.4194, -121.4944, -119.7871, -118.2437, -117.1611),
  lat  = c(37.7749, 38.5816, 36.7378, 34.0522, 32.7157)
)

cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(block_pts))

# ----------------------------
# 16. Choose which variable to map
# ----------------------------
# Priority:
# 1. turnout_rate
# 2. registered
# 3. turnout

map_var <- case_when(
  any(!is.na(block_pts$turnout_rate)) ~ "turnout_rate",
  any(!is.na(block_pts$registered))   ~ "registered",
  TRUE                                ~ "turnout"
)

cat("\nMapping variable:", map_var, "\n")

# ----------------------------
# 17. Build title/subtitle dynamically
# ----------------------------
map_title <- case_when(
  map_var == "turnout_rate" ~ "California Voter Turnout Rate by Census Block",
  map_var == "registered"   ~ "California Registered Voter Concentration",
  TRUE                      ~ "California Votes Cast by Census Block"
)

map_subtitle <- case_when(
  map_var == "turnout_rate" ~ "Dot color and size reflect turnout rate among registered voters",
  map_var == "registered"   ~ "Larger and darker dots indicate more registered voters",
  TRUE                      ~ "Larger and darker dots indicate more votes cast"
)

# ----------------------------
# 18. Build map
# ----------------------------
p_l2 <- ggplot() +
  geom_sf(data = ca_blocks, fill = "gray97", color = "white", linewidth = 0.05) +
  geom_sf(
    data = block_pts,
    aes(size = .data[[map_var]], color = .data[[map_var]]),
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
  scale_color_gradient(
    low = "#dbe9f6",
    high = "#08306B",
    labels = comma_format(),
    name = case_when(
      map_var == "turnout_rate" ~ "Turnout rate",
      map_var == "registered" ~ "Registered voters",
      TRUE ~ "Votes cast"
    )
  ) +
  scale_size_continuous(
    range = c(0.15, 3),
    labels = comma_format(),
    name = case_when(
      map_var == "turnout_rate" ~ "Turnout rate",
      map_var == "registered" ~ "Registered voters",
      TRUE ~ "Votes cast"
    )
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = map_title,
    subtitle = map_subtitle,
    caption = "Source: L2 2024 general election statistics aggregated to 2020 Census blocks"
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

p_l2

# ----------------------------
# 19. Save map
# ----------------------------
ggsave(
  "california_l2_voter_stats_map.png",
  plot = p_l2,
  width = 12,
  height = 9,
  dpi = 300
)