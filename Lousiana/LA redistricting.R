
library(alarmdata)
library(redist)
library(tidyverse)
library(ggredist)
library(sf)
library(geojsonsf)
library(readxl)
map_la <- alarm_50state_map('LA')


block_turnout <- read_csv("Data_Folder/Loui_DATA/LA_general_runoff_20241207_block.csv") |>
  mutate(geoid20 = as.character(geoid20))
shp2 <- st_read("la_testish")
block_to_precinct <- shp2




### The fun begins
block20_precinct20 <- st_read("Data_Folder/Loui_DATA/la_2020_gen_2020_blocks")
precinct20.pop <- alarm_50state_map('LA')
block20.turnout24 <- read_csv("Data_Folder/Loui_DATA/LA_general_runoff_20241207_block.csv") |>
  mutate(geoid20 = as.character(geoid20)) |>
  select(-c(starts_with("voted"), starts_with("pct")))

block20_precinct20$PRECINCTID <- substr(block20_precinct20$PRECINCTID, 1, 6)
block20_precinct20 <- block20_precinct20 |>
  unite(joincode, COUNTYFP:PRECINCTID, sep = "_")

block20.turnout24_precinct20 <- block20_precinct20 |>
  left_join(block20.turnout24, by = c("GEOID20" = "geoid20"))

precinct20.turnout24  <- block20.turnout24_precinct20 |>
  group_by(joincode) |>
  summarize(across(reg_all:reg_unk_party_others, sum)) |>
  st_drop_geometry() |>
  as.data.frame()

# precinct20.pop <- precinct20.pop |>
#  mutate(joincode = paste0(.data$countyfp, "_", .data$vtd))

precinct20.pop$countyfp <- substr(precinct20.pop$GEOID, 3, 5)
precinct20.pop <- precinct20.pop|>
  unite(col = "joincode", countyfp, vtd, sep = "_")

turnout <- precinct20.pop |>
  left_join(precinct20.turnout24, by = c("joincode" = "joincode")) |>
  select(-adj)


turnout |>
  ggplot() +
  geom_sf(aes(fill = reg_party_democratic))



la_map = redist_map(turnout, existing_plan=cd_2020, pop_tol=0.001, total_pop = pop)
  


la_plans = redist_smc(la_map, nsims=1000, compactness = 1, runs = 1)

la_map <- la_map |>
  mutate(reg_party_democratic = ifelse(is.na(reg_party_democratic) == T, 0, reg_party_democratic)) |>  
  mutate(reg_party_republican = ifelse(is.na(reg_party_republican) == T, 0, reg_party_republican))

la_plans = match_numbers(la_plans, la_map$cd_2020)

# plot(la_map, adj=TRUE) + plot(la_map)

la_plans <- la_plans |>
  mutate(pct_dem = group_frac(la_map, reg_party_democratic, (reg_party_democratic + reg_party_republican)))


plans_5of6_dem <- la_plans |>
  group_by(draw) |>
  summarize(
    dem_count = sum(pct_dem > 0.5),
    .groups = "drop"
  ) |>
  filter(dem_count >= 5)
# redist.plot.plans(la_plans, draws=c(41,67,68,12,392), shp=la_map)





la_map_5to6 <- la_map |>
  mutate(district = factor(results)) |>
  group_by(district) |>
  summarize(demo = sum(reg_party_democratic), 
            repu = sum(reg_party_republican))

results <- get_plans_matrix(la_plans)[, 15]



fedup <- la_map |>
  mutate(district = factor(results)) |>
  group_by(district) |>
  summarize() 

ggplot(fedup) +
  geom_sf()

fedup$pct_dem <- c(0.584,0.563,0.411,0.507,0.5,0.502)

ggplot(fedup) +              # District level map
  geom_sf(aes(fill = 100*pct_dem)) +
  scale_fill_gradient2(name = "Partisan Lean %", high = "blue", mid = "#E9E5FF", low = "red", midpoint = 100*0.5) +
  labs(title = "Louisiana Gerrymandered Redistricting Plan", 
       subtitle = "Assuming 100% turnout rate in general election") +
  my_map_theme()



turnout_centroids <- st_centroid(la_map) |>
  mutate(winner_votes = reg_party_democratic - reg_party_republican)
  
LA_gplan_overprec <- la_map |>                     # Precinct level map
  ggplot() +
  geom_sf(color = "gray70") +
  geom_sf(data = fedup, fill = NA, color = "black", linewidth = 1.5) +
  geom_sf(data = turnout_centroids, size = 1, aes(, color = cut(
    winner_votes, 
    breaks = c(min(winner_votes),-500,-100,-1,0,1,100,500,max(winner_votes)), 
    labels = c("R+500 or more", "R+100 to R+500", "R+1 to R+100", "R+0 to R+1", "D+0 to D+1", "D+1 to D+100", "D+100 to D+500", "D+500 or more")))) +
  scale_color_manual("", values = c("#b2182b","#d6604d","#f4a582","#fddbc7", "#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  my_map_theme() +
  labs(title = "Gerrymandered District Plan Borders over Precincts", subtitle = "Assuming 100% Turnout Rate")
LA_gplan_overprec


### Many plans

# datasets

block20.results24 <- st_read("la_2024_gen_2020_blocks") |>
  mutate(D_total = rowSums(across(matches("^GCON\\d{2}D")))) |>
  mutate(R_total = rowSums(across(matches("^GCON\\d{2}R")))) |>
  st_drop_geometry() |>
  as.data.frame()

block20_precinct20 <- st_read("Data_Folder/Loui_DATA/la_2020_gen_2020_blocks")
block20_precinct20$PRECINCTID <- substr(block20_precinct20$PRECINCTID, 1, 6)
block20_precinct20 <- block20_precinct20 |>
  unite(joincode, COUNTYFP:PRECINCTID, sep = "_")
block20.results24_precinct20 <- block20_precinct20 |>
  left_join(block20.results24, by = c("GEOID20" = "GEOID20"))


precinct20.results24  <- block20.results24_precinct20 |>
  group_by(joincode) |>
  summarize(across(D_total:R_total, sum)) |>
  st_drop_geometry() |>
  as.data.frame()

precinct20.pop <- alarm_50state_map('LA')
precinct20.pop$countyfp <- substr(precinct20.pop$GEOID, 3, 5)
precinct20.pop <- precinct20.pop|>
  unite(col = "joincode", countyfp, vtd, sep = "_")


# Setting up redistricting

my_centroids <- precinct20.results24 |>
  st_centroid() |>
  select(joincode)

overlaps <- st_join(
  x = my_centroids,
  y = precinct20.pop |> select(joincode),
  join = st_within
)


missing <- which(is.na(overlaps$joincode.y))

overlaps$joincode.y[missing] <- st_nearest_feature(
  my_centroids[missing, ],
  precinct20.pop
) |> 
  (\(i) precinct20.pop$joincode[i])()


crosswalk <- overlaps |>
  st_drop_geometry() |>
  select(precinct_2024 = joincode.x,
         GEOID20       = joincode.y)

res24 <- precinct20.results24 |>
  st_drop_geometry() |>
  as.data.frame()


precinct20.results24_joined <- precinct20.pop |>
  left_join(crosswalk, by = c("joincode" = "GEOID20")) |>
  left_join(res24, by = c("precinct_2024" = "joincode"))



results24.2 <- precinct20.results24_joined|>
  select(-adj) |>
  filter(!stringr::str_ends(joincode, "ZZZZZ"))

results24.2 |>
  ggplot() +
  geom_sf(aes(fill = D_total - R_total))


# Redistricting

la_map = redist_map(results24.2, existing_plan=cd_2020, pop_tol=0.001, total_pop = pop)


la_map_noNA <- la_map |>
  mutate(D_total = ifelse(is.na(D_total) == T, 0, D_total),
         R_total = ifelse(is.na(R_total) == T, 0, R_total))

la_plans = redist_smc(la_map, nsims=50, compactness = 1, runs = 11, ncores = 11)

la_plans = match_numbers(la_plans, la_map$cd_2020)


# Interpretation 

la_plans <- la_plans |>
  mutate(D_total = group_frac(la_map_noNA, D_total, D_total / (D_total+0.000000001)),
         R_total = group_frac(la_map_noNA, R_total, R_total / (R_total+0.000000001)),
         d_points = group_frac(la_map_noNA, D_total, D_total + R_total))

la_plans <- la_plans |>
  mutate(d_winner = ifelse(d_points > 0.5, 1, 0))

la_plan_count <- la_plans |>
  group_by(draw) |>
  summarize(blue_districts = sum(d_winner)) |>
  filter(draw != "cd_2020")

la_plan_count |>
  ggplot() +
  geom_bar(aes(x = blue_districts))

sum(la_plan_count$blue_districts)
nrow(la_plan_count) - sum(la_plan_count$blue_districts)











### This is me messing around :)




precincts2024 <- readRDS("Data_Folder/Loui_DATA/precincts2024.rds")

redistplan_p24 <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(d_votes = sum(d_votes)) |>
  st_drop_geometry() |>
  as.data.frame()

coolattempt <- la_map |>
  left_join(redistplan_p24, by = c("GEOID" = "GEOID20"))



histy = redist_smc(coolattempt, nsims=1000, compactness = 1, runs = 2, ncores = 10)
histy = match_numbers(histy, coolattempt$cd_2020)

histy <- histy |>
  mutate(d_votes = group_frac(filter(coolattempt, is.na(d_votes) == F), d_votes, pop / pop))

rankinghisty <- histy |>
  group_by(draw) |>
  summarize(
    dem_count = sum(pct_dem > 0.5),
    .groups = "drop"
  ) |>
  filter(dem_count >= 5)




block20.results24 <- st_read("la_2024_gen_2020_blocks") |>
  mutate(D_total = rowSums(across(matches("^GCON\\d{2}D")))) |>
  mutate(R_total = rowSums(across(matches("^GCON\\d{2}R")))) |>
  st_drop_geometry() |>
  as.data.frame()

block20_precinct20 <- st_read("Data_Folder/Loui_DATA/la_2020_gen_2020_blocks")
block20_precinct20$PRECINCTID <- substr(block20_precinct20$PRECINCTID, 1, 6)
block20_precinct20 <- block20_precinct20 |>
  unite(joincode, COUNTYFP:PRECINCTID, sep = "_")
block20.results24_precinct20 <- block20_precinct20 |>
  left_join(block20.results24, by = c("GEOID20" = "GEOID20"))


precinct20.results24  <- block20.results24_precinct20 |>
  group_by(joincode) |>
  summarize(across(D_total:R_total, sum)) |>
  st_drop_geometry() |>
  as.data.frame()

precinct20.pop <- alarm_50state_map('LA')
precinct20.pop$countyfp <- substr(precinct20.pop$GEOID, 3, 5)
precinct20.pop <- precinct20.pop|>
  unite(col = "joincode", countyfp, vtd, sep = "_")


results24 <- precinct20.pop |>
  left_join(precinct20.results24, by = c("joincode" = "joincode")) |>
  select(-adj) |>
  filter(!stringr::str_ends(joincode, "ZZZZZ"))

results24 |>
  ggplot() +
  geom_sf(aes(fill = D_total - R_total))






library(tmap)
tmap_mode("view")
my_map <- precinct20.pop |>
  tm_shape() +
  tm_polygons(fill_alpha = 0.1)
tmap_leaflet(my_map)
my_map



test <- block20_precinct20 |>
  group_by(joincode) |>
  summarize()

test2 <- test |>
  st_drop_geometry() |>
  as.data.frame()

test |>
  ggplot() +
  geom_sf(aes(fill = joincode)) +
  theme(legend.position = "none")


test100 <- precinct20.pop |>
  full_join(test2, by = c("joincode" = "joincode"))
test100 |>
  ggplot() +
  geom_sf(aes(fill = joincode)) +
  theme(legend.position = "none")
 







precinct20.results24  <- block20.results24_precinct20 |>
  group_by(joincode) |>
  summarize(across(D_total:R_total, sum))


precinct20.pop <- st_transform(
  precinct20.pop,
  st_crs(precinct20.results24)
)


overlaps <- st_intersection(
  precinct20.results24 |> select(joincode),
  precinct20.pop |> select(joincode)
)
overlaps$overlap_area <- st_area(overlaps)



results24 <- precinct20.pop |>
  st_join(precinct20.results24, join = st_intersects, left = TRUE) |>
  select(-adj) |>
  filter(!stringr::str_ends(joincode.x, "ZZZZZ"))




la_map = redist_map(results24, existing_plan=cd_2020, pop_tol=0.001, total_pop = pop)



la_plans = redist_smc(la_map, nsims=50, compactness = 1, runs = 2, ncores = 11)

la_plans = match_numbers(la_plans, la_map$cd_2020)


la_plans <- la_plans |>
  mutate(D_total = group_frac(la_map, D_total, D_total / (D_total+0.000000001)),
         R_total = group_frac(la_map, R_total, R_total / (R_total+0.000000001)),
         d_points = group_frac(la_map, D_total, D_total + R_total))

la_plans <- la_plans |>
  mutate(d_winner = ifelse(d_points > 0.5, 1, 0))

la_plan_count <- la_plans |>
  group_by(draw) |>
  summarize(blue_districts = sum(d_winner))

la_plan_count |>
  ggplot() +
  geom_bar(aes(x = blue_districts))























