library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tigris)
library(patchwork)

options(tigris_use_cache = TRUE)

# -------------------------
# 1. Set working folder
# -------------------------
setwd("~/Downloads/Redistricting_CA_LA")

# -------------------------
# 2. Zip file paths
# -------------------------
ca_zip <- "Data_Folder/Cali_DATA/CA_l2_2024_gen_stats_2020block (1).zip"
la_zip <- "Data_Folder/Cali_DATA/LA_l2_2024_gen_stats_2020block.zip"

# -------------------------
# 3. Extract into l2_stats folders
# -------------------------
dir.create("Data_Folder/Cali_DATA/l2_stats/ca_turnout", recursive = TRUE, showWarnings = FALSE)
dir.create("Data_Folder/Cali_DATA/l2_stats/la_turnout", recursive = TRUE, showWarnings = FALSE)

unzip(ca_zip, exdir = "Data_Folder/Cali_DATA/l2_stats/ca_turnout", overwrite = TRUE)
unzip(la_zip, exdir = "Data_Folder/Cali_DATA/l2_stats/la_turnout", overwrite = TRUE)

# -------------------------
# 4. Find csv files
# -------------------------
ca_csvs <- list.files(
  "Data_Folder/Cali_DATA/l2_stats/ca_turnout",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

la_csvs <- list.files(
  "Data_Folder/Cali_DATA/l2_stats/la_turnout",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(ca_csvs)
print(la_csvs)

ca_csv <- ca_csvs[1]
la_csv <- la_csvs[1]

# -------------------------
# 5. Read data
# -------------------------
ca_dat <- read_csv(ca_csv, show_col_types = FALSE)
la_dat <- read_csv(la_csv, show_col_types = FALSE)

# check columns
names(ca_dat)
names(la_dat)

# -------------------------
# 6. Build turnout comparison
# -------------------------
make_party_map_data <- function(df, state_name) {
  df %>%
    mutate(
      geoid20 = str_pad(as.character(geoid20), width = 15, side = "left", pad = "0"),
      dem_voted = coalesce(voted_party_democratic, 0),
      rep_voted = coalesce(voted_party_republican, 0),
      total_two_party_voted = dem_voted + rep_voted,
      party_winner = case_when(
        total_two_party_voted == 0 ~ "No D/R voters",
        dem_voted > rep_voted ~ "Democratic turnout higher",
        rep_voted > dem_voted ~ "Republican turnout higher",
        TRUE ~ "Tie"
      ),
      margin = abs(dem_voted - rep_voted),
      margin_pct = if_else(
        total_two_party_voted > 0,
        margin / total_two_party_voted,
        NA_real_
      ),
      intensity = case_when(
        is.na(margin_pct) ~ "No D/R voters",
        margin_pct < 0.10 ~ "Very close",
        margin_pct < 0.25 ~ "Moderate",
        margin_pct < 0.50 ~ "Strong",
        TRUE ~ "Very strong"
      ),
      state = state_name
    ) %>%
    select(
      geoid20, dem_voted, rep_voted, total_two_party_voted,
      party_winner, margin, margin_pct, intensity, state
    )
}

ca_map_dat <- make_party_map_data(ca_dat, "California")
la_map_dat <- make_party_map_data(la_dat, "Louisiana")

# -------------------------
# 7. Get census blocks
# -------------------------
ca_blocks <- blocks(state = "CA", year = 2020, class = "sf")
la_blocks <- blocks(state = "LA", year = 2020, class = "sf")

# -------------------------
# 8. Join data to shapes
# -------------------------
ca_sf <- ca_blocks %>%
  mutate(geoid20 = GEOID20) %>%
  left_join(ca_map_dat, by = "geoid20")

la_sf <- la_blocks %>%
  mutate(geoid20 = GEOID20) %>%
  left_join(la_map_dat, by = "geoid20")

# -------------------------
# 9. Color groups
# -------------------------
assign_fill <- function(df) {
  df %>%
    mutate(
      fill_group = case_when(
        party_winner == "No D/R voters" ~ "No D/R voters",
        party_winner == "Tie" ~ "Tie",
        party_winner == "Democratic turnout higher" & intensity == "Very close" ~ "Democratic - very close",
        party_winner == "Democratic turnout higher" & intensity == "Moderate" ~ "Democratic - moderate",
        party_winner == "Democratic turnout higher" & intensity == "Strong" ~ "Democratic - strong",
        party_winner == "Democratic turnout higher" & intensity == "Very strong" ~ "Democratic - very strong",
        party_winner == "Republican turnout higher" & intensity == "Very close" ~ "Republican - very close",
        party_winner == "Republican turnout higher" & intensity == "Moderate" ~ "Republican - moderate",
        party_winner == "Republican turnout higher" & intensity == "Strong" ~ "Republican - strong",
        party_winner == "Republican turnout higher" & intensity == "Very strong" ~ "Republican - very strong",
        TRUE ~ "No D/R voters"
      )
    )
}

ca_sf <- assign_fill(ca_sf)
la_sf <- assign_fill(la_sf)

fill_colors <- c(
  "Democratic - very close"  = "#bcd7ff",
  "Democratic - moderate"    = "#79aef2",
  "Democratic - strong"      = "#3b7ddd",
  "Democratic - very strong" = "#114ebd",
  "Republican - very close"  = "#f7c4c4",
  "Republican - moderate"    = "#ee8d8d",
  "Republican - strong"      = "#d94b4b",
  "Republican - very strong" = "#a61212",
  "Tie"                      = "#bdbdbd",
  "No D/R voters"            = "#efefef"
)

# -------------------------
# 10. Plot
# -------------------------
plot_party_map <- function(sf_data, title_text) {
  ggplot(sf_data) +
    geom_sf(aes(fill = fill_group), color = NA) +
    scale_fill_manual(values = fill_colors, drop = FALSE) +
    labs(
      title = title_text,
      subtitle = "Registered Democratic vs Republican turnout by 2020 Census block",
      fill = "Party turnout advantage"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "right"
    )
}

ca_plot <- plot_party_map(ca_sf, "California: Democratic vs Republican Turnout")
la_plot <- plot_party_map(la_sf, "Louisiana: Democratic vs Republican Turnout")

combined_plot <- ca_plot + la_plot + plot_layout(ncol = 2)

print(combined_plot)

ggsave(
  "ca_la_dem_vs_rep_turnout_map.png",
  combined_plot,
  width = 16,
  height = 8,
  dpi = 300
)

