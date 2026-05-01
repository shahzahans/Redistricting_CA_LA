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