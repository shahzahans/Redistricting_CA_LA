library(alarmdata)
library(redist)
library(tidyverse)
library(ggredist)
library(sf)
library(geojsonsf)
library(readxl)
map_la <- alarm_50state_map('LA')
plans_la <- alarm_50state_plans('LA')
shp <- st_read("Data_Folder/LA_DATA/la_2024_gen_all_prec")
sf_obj <- geojson_sf("https://redistricting.lls.edu/wp-content/uploads/la_2020_congress_2024-01-22.json")

### Tidy Datasets
precincts <- shp |>
  select(Parish, NAME20, GEOID20, GCON01DMAN:geometry) |>
  mutate(district = ifelse(GCON01DMAN | GCON01NHYE | GCON01RARR | GCON01RSCA | GCON01RSHA > 0, 1,
                           ifelse(GCON02DCAR | GCON02DDAV | GCON02RGRA | GCON02RLYN | GCON02RPER > 0, 2,
                                  ifelse(GCON03DGON > 0, 3,
                                         ifelse(GCON04RJOH > 0, 4,
                                                ifelse(GCON05DVAL | GCON05RMEN > 0, 5, 6)))))) |>
  pivot_longer(cols = GCON01DMAN:GCON06RGUI,
               names_to = "person",
               values_to = "votes") |>
  mutate(party = ifelse((person == "GCON01NHYE"), "noparty",
                        ifelse((person == "GCON01DMAN" | person == "GCON02DCAR" | person == "GCON02DDAV" | person == "GCON03DGON" | person == "GCON03DSUM" | person == "GCON05DVAL" | person == "GCON06DAND" | person == "GCON06DFIE" | person == "GCON06DJON" | person == "GCON06DWIL"), "democrat", "republican")))|>
  filter(votes > 0, party != "other") |>
  mutate(d_votes = ifelse(party == "republican", (-1)*votes, votes))

X2026_0401_sta_comb <- read_excel("2026_0401_sta_comb.xls", sheet = "Sheet1", skip = 8)
racedemo2026 <- X2026_0401_sta_comb |>
  select(-`...2`)
Race <- c("Total", "White", "Black", "Other")
names(racedemo2026) <- c("Parish",
                     paste("Reg", Race, sep = "_"),
                     paste("D", Race, sep ="_"),
                     paste("R", Race, sep = "_"),
                     paste("O", Race, sep = "_"))

### Load Datasets


saveRDS(precincts, "precincts2024.rds")

precincts2024 <- readRDS("precincts2024.rds")

### Plots


just_parishes <- precincts2024 |>
  ggplot() +
  geom_sf(aes(fill = Parish), color = NA) +
  theme(legend.position = "none") +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)
just_parishes


precincts2024 |>
  ggplot() +
  geom_sf(aes(fill = party, color = party), linewidth = 0.05) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)


precincts_votes <- precincts2024 |>
  group_by(GEOID20) |> 
  mutate(total = sum(votes)) |>
  group_by(GEOID20, party) |>
  mutate(prop_votes = d_votes / total) |>
  group_by(GEOID20) |>
  slice_max(order_by = abs(prop_votes), n = 1) |>
  ggplot() +
  geom_sf(aes(fill = prop_votes)) +
  scale_fill_party_b() +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)
precincts_votes


num_votes_map <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(votes = sum(votes)) |>
  ggplot() +
  geom_sf(aes(fill = cut(votes, breaks = c(0,50, 100, 250, 500, 750, 100000)))) +
  scale_fill_manual(values = c("gray90", "gray80", "gray65", "gray45", "gray20", "gray10")) +
  theme_bw() +
  geom_sf(data = sf_obj, fill = NA, color = "darkred", linewidth = 1)
num_votes_map



district_winner <- sf_obj |>
  left_join(
    precincts2024 |>
      group_by(district, person) |>
      summarize(votes = sum(d_votes)) |>
      mutate(prop_votes = votes / sum(abs(votes))) |>
      arrange(desc(abs(prop_votes))) |>
      head(n=6) |>
      mutate(majority = ifelse(prop_votes > 0, prop_votes - 0.5, prop_votes + 0.5)) |>
      st_drop_geometry() |>
      as_tibble(),
    by = c("Districts" = "district"))
district_winner <- district_winner |>
  ggplot() +
  geom_sf(aes(fill = majority)) +
  scale_fill_gradient2(low = "#b2182b",
                       mid = "white",
                       high = "#2166ac",
                       midpoint = -0.04)
district_winner

### Probably Worthless

districts2024 |>
  ggplot() +
  geom_sf(aes(fill = winner))




precincts2024_dem <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(num_winner = sum(d_votes)) |>
  mutate(party_winner = ifelse(num_winner < 0, "Republican_Winner", "Democrat_Winner")) |>
  filter(party_winner == "Democrat_Winner")

precincts2024_rep <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(num_winner = sum(d_votes)) |>
  mutate(party_winner = ifelse(num_winner < 0, "Republican_Winner", "Democrat_Winner")) |>
  filter(party_winner == "Republican_Winner")











precincts <- shp |>
  select(Parish, NAME20, GEOID20, GCON01DMAN:geometry) |>
  mutate(district = ifelse(GCON01DMAN | GCON01NHYE | GCON01RARR | GCON01RSCA | GCON01RSHA > 0, 1,
                           ifelse(GCON02DCAR | GCON02DDAV | GCON02RGRA | GCON02RLYN | GCON02RPER > 0, 2,
                                  ifelse(GCON03DGON > 0, 3,
                                         ifelse(GCON04RJOH > 0, 4,
                                                ifelse(GCON05DVAL | GCON05RMEN > 0, 5, 6)))))) |>
  pivot_longer(cols = GCON01DMAN:GCON06RGUI,
               names_to = "person",
               values_to = "votes")

ggplot(data = precincts) +
  geom_sf(fill = "green")
