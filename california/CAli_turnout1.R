library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tigris)
library(scales)

options(tigris_use_cache = TRUE)

setwd("~/Downloads/Redistricting_CA_LA")

# -------------------------
# California data
# -------------------------
ca_csv <- list.files(
  "Data_Folder/Cali_DATA/l2_stats",
  pattern = "CA.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)[1]

ca_dat <- read_csv(ca_csv, show_col_types = FALSE)

# -------------------------
# Clean and aggregate to county
# -------------------------
ca_county_dat <- ca_dat %>%
  mutate(
    geoid20 = str_pad(as.character(geoid20), width = 15, pad = "0"),
    county_fips = substr(geoid20, 1, 5),
    dem_voted = coalesce(voted_party_democratic, 0),
    rep_voted = coalesce(voted_party_republican, 0)
  ) %>%
  group_by(county_fips) %>%
  summarise(
    dem_voted = sum(dem_voted, na.rm = TRUE),
    rep_voted = sum(rep_voted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_two_party = dem_voted + rep_voted,
    winner = case_when(
      total_two_party == 0 ~ "No D/R voters",
      dem_voted > rep_voted ~ "Democratic",
      rep_voted > dem_voted ~ "Republican",
      TRUE ~ "Tie"
    ),
    margin = abs(dem_voted - rep_voted),
    margin_pct = if_else(total_two_party > 0, margin / total_two_party, NA_real_),
    fill_group = case_when(
      winner == "No D/R voters" ~ "No D/R voters",
      winner == "Tie" ~ "Tie",
      winner == "Democratic" & margin_pct < 0.10 ~ "Democratic - very close",
      winner == "Democratic" & margin_pct < 0.25 ~ "Democratic - moderate",
      winner == "Democratic" & margin_pct < 0.50 ~ "Democratic - strong",
      winner == "Democratic" ~ "Democratic - very strong",
      winner == "Republican" & margin_pct < 0.10 ~ "Republican - very close",
      winner == "Republican" & margin_pct < 0.25 ~ "Republican - moderate",
      winner == "Republican" & margin_pct < 0.50 ~ "Republican - strong",
      winner == "Republican" ~ "Republican - very strong"
    )
  )

# -------------------------
# County shapes
# -------------------------
ca_counties <- counties(state = "CA", year = 2020, class = "sf") %>%
  mutate(county_fips = GEOID)

ca_sf <- ca_counties %>%
  left_join(ca_county_dat, by = "county_fips")

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

ca_plot <- ggplot(ca_sf) +
  geom_sf(aes(fill = fill_group), color = "white", linewidth = 0.15) +
  scale_fill_manual(values = fill_colors, drop = FALSE) +
  labs(
    title = "California: Democratic vs Republican Turnout",
    subtitle = "County-level aggregation from 2024 L2 block turnout data",
    fill = "Turnout advantage"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

print(ca_plot)

ggsave("CA_PNG/california_dem_vs_rep_county_map.png", ca_plot, width = 9, height = 7, dpi = 300)