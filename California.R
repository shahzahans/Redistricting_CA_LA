installed.packages('alarmdata')
library(alarmdata)
library(redist)
library(ggplot2)
library(ggredist)
library(dplyr)

# California map data
map_ca <- alarm_50state_map("CA")

# Build final map
p <- map_ca |>
  ggplot() +
  geom_district(
    aes(group = cd_2020, fill = ndv, denom = nrv + ndv),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_party_c(name = "Democratic vote share") +
  theme_map() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "right"
  )

# Show map
p

# Save high-quality version
ggsave(
  "california_congressional_partisanship.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

# Small summary table
plan_info <- data.frame(
  Item = c("State", "Plan Type", "Institution", "Effective Date", "Status"),
  Value = c("California", "Congressional Plan", "Legislature", "Nov. 4, 2025", "In litigation")
)
cat(
  sprintf("%-25s %s\n", "Item", "Value"),
  sprintf("%-25s %s\n", "---", "-----"),
  paste(sprintf("%-25s %s", plan_info$Item, plan_info$Value), collapse = "\n")
)

plan_info


