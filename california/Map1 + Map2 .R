library(ggplot2)
library(scales)

p_overlay <- ggplot() +
  geom_sf(
    data = cong_vote,
    aes(fill = dem_share),
    color = "white",
    linewidth = 0.35,
    alpha = 0.85
  ) +
  geom_sf(
    data = bg_pts |> filter(!is.na(total_cvap), total_cvap >= 200),
    aes(color = largest_group, size = total_cvap),
    alpha = 0.65
  ) +
  geom_sf(data = cities_sf, color = "black", size = 1.5) +
  geom_sf_text(
    data = cities_sf,
    aes(label = city),
    size = 3.1,
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
    labels = c("75% R", "Even", "75% D"),
    name = "Partisan balance"
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
    range = c(0.3, 3),
    labels = comma_format(),
    name = "Total CVAP"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "California Redistricting: Partisanship and Demographic Concentration",
    subtitle = "District color shows partisan balance: dots show block-group demographic concentration",
    caption = "Source: Redistricting Data Hub precinct and CVAP data, U.S. Census TIGER/Line, and 2025 adopted congressional districts"
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

p_overlay

ggsave(
  "california_redistricting_overlay.png",
  plot = p_overlay,
  width = 12,
  height = 9,
  dpi = 300
)