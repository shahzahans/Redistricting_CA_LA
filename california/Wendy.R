library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(dplyr)
library(plotly)

#Used a bit of Ximena's code to restructure the data file
ca_cvap_2024_bg <- read_csv("C:/Users/whare/OneDrive/Documents/Data and Society/Data/ca_cvap_2024_bg.csv")

subset_ca_cvap_2024_bg <- ca_cvap_2024_bg |>
   select(GEOID, COUNTY, COUNTYFP, TRACTCE, BLKGRPCE, C_TOT24, C_AMI24, C_ASI24, C_BLA24,C_NHP24, C_WHT24, C_HSP24, CVAP_TOT24,CVAP_AMI24, CVAP_ASI24,CVAP_BLA24, CVAP_NHP24, CVAP_WHT24, CVAP_HSP24)

# Full California county -> region mapping
county_region <- c(
  "IMPERIAL" = "Imperial",
  "SAN DIEGO" = "San Diego",
  "RIVERSIDE" = "Southland",
  "SAN BERNARDINO" = "Southland",
  "ORANGE" = "Southland",
  "LOS ANGELES" = "Southland",
  "VENTURA" = "Southland",
  "SANTA BARBARA" = "Central Coast",
  "SAN LUIS OBISPO" = "Central Coast",
  "MONTEREY" = "Central Coast",
  "SAN BENITO" = "Central Coast",
  "KERN" = "Central Valley",
  "TULARE" = "Central Valley",
  "KINGS" = "Central Valley",
  "FRESNO" = "Central Valley",
  "MADERA" = "Central Valley",
  "MERCED" = "Central Valley",
  "STANISLAUS" = "Central Valley",
  "SAN JOAQUIN" = "Central Valley",
  "SACRAMENTO" = "Central Valley",
  "YOLO" = "Central Valley",
  "SUTTER" = "Central Valley",
  "YUBA" = "Central Valley",
  "COLUSA" = "Central Valley",
  "BUTTE" = "Central Valley",
  "GLENN" = "Central Valley",
  "TEHAMA" = "Central Valley",
  "MARIPOSA" = "Sierra / Gold County",
  "TUOLUMNE" = "Sierra / Gold County",
  "AMADOR" = "Sierra / Gold County",
  "CALAVERAS" = "Sierra / Gold County",
  "EL DORADO" = "Sierra / Gold County",
  "PLACER" = "Sierra / Gold County",
  "NEVADA" = "Sierra / Gold County",
  "SIERRA" = "Sierra / Gold County",
  "INYO" = "Eastern Sierra",
  "MONO" = "Eastern Sierra",
  "ALPINE" = "Eastern Sierra",
  "SANTA CRUZ" = "Bay Area",
  "SANTA CLARA" = "Bay Area",
  "SAN MATEO" = "Bay Area",
  "ALAMEDA" = "Bay Area",
  "CONTRA COSTA" = "Bay Area",
  "SOLANO" = "Bay Area",
  "MARIN" = "Bay Area",
  "SAN FRANCISCO" = "Bay Area",
  "NAPA" = "Northwest",
  "SONOMA" = "Northwest",
  "LAKE" = "Northwest",
  "MENDOCINO" = "Northwest",
  "HUMBOLDT" = "Northwest",
  "SHASTA" = "Far North / Shasta",
  "PLUMAS" = "Far North / Shasta",
  "TRINITY" = "Far North / Shasta",
  "DEL NORTE" = "Far North / Shasta",
  "SISKIYOU" = "Far North / Shasta",
  "LASSEN" = "Far North / Shasta",
  "MODOC" = "Far North / Shasta"
)

# Build regional summary using Hispanic citizen population
df_region <- subset_ca_cvap_2024_bg |>
  mutate(region = county_region[COUNTY]) |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarise(
    hispanic_pop = sum(C_HSP24, na.rm = TRUE),
    total_pop = sum(C_TOT24, na.rm = TRUE)
  ) |>
  mutate(pct_hispanic = hispanic_pop / total_pop) |>
  arrange(desc(hispanic_pop))

# Plot totals
ggplot(df_region, aes(x = reorder(region, hispanic_pop), y = hispanic_pop)) +
  geom_col(fill = "blue", color = "black") +
  labs(
    title = "Hispanic Population by California Region",
    x = "Region",
    y = "Hispanic Population"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-6,
      suffix = "M",
      accuracy = 0.1
    )
  ) +
  theme_minimal()

fig <- plot_ly(
  data = df_region,
  x = ~region,
  y = ~hispanic_pop,
  type = "bar",
  hovertemplate = paste(
    "Region: %{x}<br>",
    "Population: %{y:,.0f}<br>",
    "Pct Hispanic: %{customdata:.1%}",
    "<extra></extra>"
  ),
  customdata = ~pct_hispanic
)

# Plot percentages
ggplot(df_region, aes(x = reorder(region, pct_hispanic), y = pct_hispanic)) +
  geom_col(fill = "darkred", color = "black") +
  labs(
    title = "Percent Hispanic Population by California Region",
    x = "Region",
    y = "Percent Hispanic"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

fig <- plot_ly(
  data = df_region,
  x = ~region,
  y = ~pct_hispanic,
  type = "bar",
  hovertemplate = paste(
    "Region: %{x}<br>",
    "Pct Hispanic: %{customdata:.1%}",
    "<extra></extra>"
  ),
  customdata = ~pct_hispanic
)
fig
cal_gen_stats <- read.csv("C:/Users/whare/OneDrive/Documents/Data and Society/Data/CA_l2_2024_gen_stats_2020county/CA_l2_2024_gen_stats_2020county.csv")

colnames(cal_gen_stats)[colnames(cal_gen_stats) == "countyname"] <- "COUNTY"

cal_gen_stats$COUNTY <- toupper(gsub(" County", "", cal_gen_stats$COUNTY))

subset_cal_gen_stats <- cal_gen_stats |>
  select(COUNTY, voted_age_18_19, voted_age_20_24, voted_age_25_29, voted_age_30_34, 
         voted_age_35_44, voted_age_45_54, voted_age_55_64, 
         voted_age_65_74, voted_age_75_84, voted_age_85over, 
         voted_party_democratic, voted_party_republican)

merged_df <- subset_ca_cvap_2024_bg |>
  left_join(subset_cal_gen_stats, by = "COUNTY")

age_pop <- merged_df |> 
  select(COUNTY, voted_age_18_19, voted_age_20_24, voted_age_25_29, voted_age_30_34, voted_age_35_44, voted_age_45_54, voted_age_55_64,
         voted_age_65_74, voted_age_75_84,voted_age_85over) |>
  pivot_longer(cols = starts_with("voted_age_"),
               names_to = "age_group",
               values_to = "age_pop") |>
  group_by(age_group) |>
  summarise(total_age_pop = sum(age_pop, na.rm = TRUE))

ggplot(age_pop, aes(x = age_group, y = total_age_pop, fill = age_group)) +
  geom_col() +
  labs(
    title = "Population by Age Group",
    x = "Age Group",
    y = "Population"
  ) + scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-6,
      suffix = "B",
      accuracy = 0.1
    ))+
  theme_minimal()

ggplot(hisp_age_pop, aes(x = age_group, y = C_HSP24, fill = age_group)) +
  geom_col() +
  labs(
    title = "Hispanic Population Compared with Age Groups",
    x = "Age Group",
    y = "Hispanic Population"
  ) +
  theme_minimal()

merged_df <- merged_df |>
  mutate(pct_hispanic = C_HSP24 / C_TOT24)

ggplot(merged_df, aes(x = voted_age_18_19, y = pct_hispanic)) +
  geom_col() +
  labs(
    title = "Percent Hispanic vs Age 18–19 Population",
    x = "Age 18–19 Population",
    y = "Percent Hispanic"
  ) +
  theme_minimal()

#nonvotes <- read.csv("california/CA_l2_2024_pri_stats_2020block")



#LA_table <- CA_l2_2024_pri_stats_2020block|>
#  summarize(across(voted_eur:reg_unk_party_others, sum)) |>
#  mutate(reg_others_party_democratic = reg_hisp_party_democratic + reg_esa_party_democratic + reg_oth_party_democratic + reg_unk_party_democratic,
#         reg_others_party_republican = reg_hisp_party_republican + reg_esa_party_republican + reg_oth_party_republican + reg_unk_party_republican,
#         pct_reg_others_party_democratic = (pct_voted_hisp_party_democratic + pct_voted_esa_party_democratic + pct_voted_oth_party_democratic + pct_voted_unk_party_democratic)/4,
#         pct_reg_others_party_republican = (pct_voted_hisp_party_republican + pct_voted_esa_party_republican + pct_voted_oth_party_republican + pct_voted_unk_party_republican)/4)
#View(LA_table)

#subset_ca_hisp <- LA_table |>
#  select(voted_hisp_party_democratic, reg_hisp_party_democratic, voted_hisp_party_republican, reg_hisp_party_republican, voted_aa_party_democratic,
#         reg_aa_party_democratic, voted_aa_party_republican, reg_aa_party_republican, voted_esa_party_democratic, reg_esa_party_democratic, 
#         voted_esa_party_republican, reg_esa_party_republican, voted_eur_party_democratic, reg_eur_party_democratic, voted_eur_party_republican, reg_eur_party_republican)
#View(subset_ca_hisp)


