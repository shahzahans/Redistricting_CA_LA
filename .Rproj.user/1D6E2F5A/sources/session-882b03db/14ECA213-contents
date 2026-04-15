library(alarmdata)
library(redist)
library(tidyverse)
library(ggredist)
library(sf)
library(geojsonsf)
map_la <- alarm_50state_map('LA')
plans_la <- alarm_50state_plans('LA')
shp <- st_read("la_2024_gen_all_prec")
sf_obj <- geojson_sf("https://redistricting.lls.edu/wp-content/uploads/la_2020_congress_2024-01-22.json")







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
                        ifelse((person == "GCON01DMAN" | person == "GCON02DCAR" | person == "GCON02DDAV" | person == "GCON03DGON" | person == "GCON03DSUM" | person == "GCON05DVAL" | person == "GCON06DAND" | person == "GCON06DFIE" | person == "GCON06DJON" | person == "GCON06DWIL"), "democrat", "republican"))) |>
  group_by(NAME20, Parish) |>
  slice_max(votes, n = 1, with_ties = FALSE)


precincts |>
  ggplot() +
  geom_sf(aes(fill = party, color = party), linewidth = 0.05) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)

saveRDS(precincts, "precincts2024.rds")
precincts2024 <- readRDS("precincts2024.rds")
  
  
  # mutate(d_votes = GCON01DMAN + GCON02DCAR + GCON02DDAV + GCON03DGON + GCON03DSUM +GCON05DVAL + GCON06DAND + GCON06DFIE + GCON06DJON + GCON06DWIL,
  #       r_votes = GCON01RARR + GCON01RSCA + GCON01RSHA + GCON02RGRA + GCON02RLYN + GCON02RPER + GCON03RHIG + GCON03RJOH + GCON04RMOR + GCON05RLET + GCON05RMEN + GCON06RGUI, 
  #       n_votes = GCON01NHYE) |> 





