
#### for polsby 

CA_vote_join_projected <- st_transform(CA_vote_join, crs = 5070)

adj_ca_projected <- redist.adjacency(CA_vote_join_projected)

bad_precincts <- c(3442, 3443, 7296)

# Function to add adjacency; creates a connection between row1 and row2
add_adjacency <- function(adj_data, precinct1, precinct2) {
  # add precinct2 to adjacencies for precinct1
  # !!!NOTE!!! 
  # precinct numbers within adjacency vectors are zero-indexed, 
  # while elements of the list are not
  # Need to subtract 1 from precinct added to adjacency list to make it zero-indexed
  adj_data[[precinct1]] <- unique(c(adj_data[[precinct1]], precinct2 - 1))
  # add precinct1 to adjacencies for precinct2
  adj_data[[precinct2]] <- unique(c(adj_data[[precinct2]], precinct1 - 1))
  adj_data
}

# Function to fix adjacency for a specified precinct 
fix_adjacency <- function(adj_data, sf_data, precinct){
  # compute distances from the precinct to other precincts
  distances <- st_distance(sf_data[precinct,], sf_data) |> as.numeric()
  # find shortest nonzero distance to another precinct
  shortest_dist <- min(distances[distances != 0])
  # add a connection between input precinct and closest other precinct
  adj_data <- add_adjacency(adj_data, precinct, which(distances == shortest_dist))
  adj_data
}

for(precinct in bad_precincts) {
  adj_ca_projected <- fix_adjacency(adj_ca_projected, CA_vote_join_projected, precinct)
}

redist_obj_ca_vote_t <- redist_map(
  data = CA_vote_join_projected,
  pop = CA_vote_join_projected$total_pop,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj_ca_projected
) 


redist_obj_ca_vote_t$d_points <- redist_obj_ca_vote_t$data$dem_votes - redist_obj_ca_vote_t$data$rep_votes
redist_obj_ca_vote_t$ones <- rep(1,9129)



set.seed(1)
plans_ca_vote_t <- redist_smc(
  redist_obj_ca_vote_t,
  nsims = 20,      # number of plans
  runs = 1,
  ncores = 7,
  compactness = 1
)


plans_ca_vote_t <- plans_ca_vote_t |>
  mutate(polsby = comp_polsby(plans_ca_vote_t, shp = CA_vote_join_projected))


plans_ca_vote_t <- plans_ca_vote_t|>
  mutate(D_points = group_frac(redist_obj_ca_vote_t, d_points, ones),
         r_winner = ifelse(D_points < 0, 1, 0 ))


CA_bar_t <- plans_ca_vote_t|>
  group_by(draw)|>
  summarize(r_winner = sum(r_winner))|>
  ungroup()|>
  arrange(r_winner)

CA_bar_t


best_average_plan <- plans_ca_vote_t |>
  group_by(draw) |>
  summarize(avg_polsby = mean(polsby, na.rm = TRUE)) |>
  arrange(desc(avg_polsby)) 

print(best_average_plan)



plan1_ca_vote_t <- get_plans_matrix(plans_ca_vote_t)[, 8]

mapca_polsby <- redist_obj_ca_vote_t$data |>
  mutate(district = factor(plan1_ca_vote_t))



mapca_plan1_vote_t <- mapca_polsby |>
  group_by(district) |>
  summarize(
    total_votes = sum(total_votes),
    total_dem = sum(dem_votes, na.rm = TRUE),
    total_rep = sum(rep_votes, na.rm = TRUE),
    total_vap = sum(total_vap),
    total_vap_hisp = sum(total_vap_hisp),
    total_vap_asian =sum(total_vap_asian),
    total_vap_black = sum(total_vap_black),
    total_vap_white = sum(total_vap_white),
    total_18_19 = sum(VA_18_19),
    total_20_24 = sum(VA_20_24),
    total_25_29 = sum(VA_25_29)
  )

mapca_plan1_vote_t <- mapca_plan1_vote_t |>
  mutate(
    vote_diff = total_dem - total_rep,            # Positive = Dem won, Negative = Rep won
    margin_pct = (total_dem - total_rep) / total_votes,
    d_winner = ifelse(vote_diff > 0, 1, 0),
    pct_va_18_19 = total_18_19/total_vap *100,
    pct_va_20_24 = total_20_24/total_vap *100,
    pct_va_25_29 = total_25_29/total_vap *100,
    pct_vap_hisp = total_vap_hisp/total_vap *100,
    pct_vap_black = total_vap_black/total_vap *100,
    pct_vap_white = total_vap_white/total_vap *100,
    pct_vap_asian = total_vap_asian/total_vap *100,
    total_minority_vap = (total_vap_hisp + total_vap_black + total_vap_asian),
    pct_minority_vap = total_minority_vap / total_vap * 100# Percentage lead
  )    


mapca_district_plan1_t <- mapca_polsby |>
  group_by(district)|>
  summarize()


ca_vote_district_map1 <- ggplot(mapca_plan1_vote_t) +
  geom_sf(aes(fill = margin_pct)) +
  scale_fill_gradient2(
    low = "red",         # Strong Republican
    mid = "white",       # Toss-up
    high = "blue",       # Strong Democratic
    midpoint = 0,        # 0 means a perfect tie
    labels = scales::percent
  ) +
  geom_sf(data = mapca_district_plan1, fill = NA, color = "black", linewidth = 0.5)+
  theme_minimal() +
  labs(title = "Proposed Redistricting for California: Win Margins",
       fill = "Lead %")
ca_vote_district_map1 


nrow(mapca_plan1_vote)-sum(mapca_plan1_vote$d_winner)


mapca_plan1_vote_t$margin_bin <- cut(
  mapca_plan1_vote_t$margin_pct,
  breaks = c(-Inf, -0.15, -0.10, -0.05, -0.01, 0, 0.01, 0.10, 0.20, 0.50, Inf),
  labels = c(
    "R +15% or more",
    "R +15% to R +10%",
    "R +10% to R +5%",
    "R +5% to R +1%",
    "R +1% to 0%",
    "0% to D +1%",
    "D +1% to D +10%",
    "D +10% to D +20%",
    "D +20% to D +50%",
    "D +50% or more"
  ),
  include.lowest = TRUE,
  right = FALSE
)



mapca1_t <- ggplot(mapca_plan1_vote_t) +
  geom_sf(aes(fill = margin_bin)) +
  scale_fill_manual(
    values = c(
      "R +15% or more"     = "#67001f",
      "R +15% to R +10%"   = "#b2182b",
      "R +10% to R +5%"    = "#d6604d",
      "R +5% to R +1%"     = "#f4a582",
      "R +1% to 0%"        = "#fddbc7",
      "0% to D +1%"        = "#d1e5f0",
      "D +1% to D +10%"    = "#92c5de",
      "D +10% to D +20%"   = "#4393c3",
      "D +20% to D +50%"   = "#2166ac",
      "D +50% or more"     = "#053061"
    ),
    name = "Margin %"
  ) +
  geom_sf(data = mapca_district_plan1_t, fill = NA, color = "black", linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Proposed Redistricting for California: Win Margins")
mapca1_t

## tmap for polsby


tmap_mode("view")

tm_mapca_t <- tm_shape(mapca_plan1_vote_t) +
  tm_polygons(
    col = "margin_bin",
    palette = c(
      "R +15% or more" = "#67001f",
      "R +15% to R +10%" = "#b2182b",
      "R +10% to R +5%" = "#d6604d",
      "R +5% to R +1%" = "#f4a582",
      "R +1% to 0%" = "#fddbc7",
      "0% to D +1%" = "#d1e5f0",
      "D +1% to D +10%" = "#92c5de",
      "D +10% to D +20%" = "#4393c3",
      "D +20% to D +50%" = "#2166ac",
      "D +50% or more" = "#053061"
    ),
    title = "Margin %",
    popup.vars = TRUE
  ) +
  tm_shape(mapca_district_plan1_t) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_layout(
    title = "Proposed Redistricting for California: Win Margins",
    legend.outside = TRUE
  )
tm_mapca_t

### polsby for LA 



LA_vote_join_projected <- st_transform(join_vtd_pop2_filter, crs = 5070)

adj_la_projected <- redist.adjacency(LA_vote_join_projected)

redist_obj_la2_p <- redist_map(
  data = LA_vote_join_projected,
  pop = LA_vote_join_projected$pop,
  ndists = 6,
  pop_tol = 0.01,
  adj = adj_la_projected
)

redist_obj_la2_p$d_points <- redist_obj_la2_p$data$total_demo - redist_obj_la2_p$data$total_rep
redist_obj_la2_p$ones <- rep(1,3639)

plans_la2_p <- redist_smc(
  redist_obj_la2_p,
  nsims = 50,      # number of plans
  runs = 2,# independent chains
  compactness = 1
)

plans_la2_p <- plans_la2_p |>
  mutate(polsby = comp_polsby(plans_la2_p, shp = LA_vote_join_projected))

plans_la2_p <- plans_la2_p|>
  mutate(D_points = group_frac(redist_obj_la2_p, d_points, ones),
         r_winner = ifelse(D_points < 0, 1, 0 ))

LA_bar <- plans_la2_p|>
  group_by(draw)|>
  summarize(r_winner = sum(r_winner))|>
  ungroup()|>
  arrange(r_winner)


best_la_average_plan <- plans_la2_p |>
  group_by(draw) |>
  summarize(avg_polsby = mean(polsby, na.rm = TRUE)) |>
  arrange(desc(avg_polsby))

planla1_p <- get_plans_matrix(plans_la2_p)[, 18]

mapla_polsby <- redist_obj_la2_p$data |>
  mutate(district = factor(planla1_p))


mapla_district_plan1_p <- mapla_polsby |>
  group_by(district)|>
  summarize()

la_results <- mapla_polsby|>      
  group_by(district) |>
  summarize(
    total_dem = sum(total_demo, na.rm = TRUE),
    total_rep = sum(total_rep, na.rm = TRUE),
    total_votes = sum(total_votes),
    total_pop = sum(pop),
    total_black = sum(pop_black, na.rm = TRUE),
    total_hispanic = sum(pop_hisp, na.rm = TRUE),
    total_white = sum(pop_white, na.rm = TRUE),
    total_vap       = sum(vap, na.rm = TRUE),
    total_vap_hisp  = sum(vap_hisp, na.rm = TRUE),
    total_vap_white = sum(vap_white, na.rm = TRUE),
    total_vap_black = sum(vap_black, na.rm = TRUE)
  )


la_results <- la_results|>
  mutate(d_prop = (total_dem/total_votes),
         r_prop = (total_rep/total_votes),
         winner = case_when(
           d_prop > 0.5 ~ "Democratic",
           r_prop > 0.5 ~ "Republican",
           TRUE         ~ "Tie/Other"),
         total_minority = (total_black+total_hispanic),
         total_minority_vap = (total_vap_hisp + total_vap_black),
         pct_minority = ( total_minority / total_pop * 100),
         pct_vap_hisp  = total_vap_hisp  / total_vap * 100,
         pct_vap_white = total_vap_white / total_vap * 100,
         pct_vap_black = total_vap_black / total_vap * 100,
         pct_minority_vap = total_minority_vap / total_vap * 100
  )

la_results <- la_results |>
  mutate(
    vote_diff = total_dem - total_rep,            # Positive = Dem won, Negative = Rep won
    margin_pct = (total_dem - total_rep) / total_votes # Percentage lead
  )    

la_results$margin_bin <- cut(
  la_results$margin_pct,
  breaks = c(-Inf, -0.60, -0.30, -0.10, -0.01, 0, 0.01, 0.10, 0.20, 0.30, Inf),
  labels = c(
    "R +60% or more",
    "R +60% to R +30%",
    "R +30% to R +10%",
    "R +10% to R +1%",
    "R +1% to 0%",
    "0% to D +1%",
    "D +1% to D +10%",
    "D +10% to D +20%",
    "D +20% to D +30%",
    "D +30% or more"
  ),
  include.lowest = TRUE,
  right = FALSE
)


m_la_p <- 
  tm_shape(la_results) +
  tm_polygons(
    col = "margin_bin",
    palette = c(
      "R +60% or more" = "#67001f",
      "R +60% to R +30%" = "#b2182b",
      "R +30% to R +10%" = "#d6604d",
      "R +10% to R +1%" = "#f4a582",
      "R +1% to 0%" = "#fddbc7",
      "0% to D +1%" = "#d1e5f0",
      "D +1% to D +10%" = "#92c5de",
      "D +10% to D +20%" = "#4393c3",
      "D +20% to D +30%" = "#2166ac",
      "D +30% or more" = "#053061"
    ),
    title = "Margin %",
    popup.vars = TRUE
  ) +
  tm_shape(mapla_district_plan1_p) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(
    title = "Proposed Redistricting for Louisiana: Win Margins",
    legend.outside = TRUE
  )

m_la_p

##### for reock


CA_vote_join_projected <- st_transform(CA_vote_join, crs = 5070)

adj_ca_projected <- redist.adjacency(CA_vote_join_projected)

bad_precincts <- c(3442, 3443, 7296)

# Function to add adjacency; creates a connection between row1 and row2
add_adjacency <- function(adj_data, precinct1, precinct2) {
  # add precinct2 to adjacencies for precinct1
  # !!!NOTE!!! 
  # precinct numbers within adjacency vectors are zero-indexed, 
  # while elements of the list are not
  # Need to subtract 1 from precinct added to adjacency list to make it zero-indexed
  adj_data[[precinct1]] <- unique(c(adj_data[[precinct1]], precinct2 - 1))
  # add precinct1 to adjacencies for precinct2
  adj_data[[precinct2]] <- unique(c(adj_data[[precinct2]], precinct1 - 1))
  adj_data
}

# Function to fix adjacency for a specified precinct 
fix_adjacency <- function(adj_data, sf_data, precinct){
  # compute distances from the precinct to other precincts
  distances <- st_distance(sf_data[precinct,], sf_data) |> as.numeric()
  # find shortest nonzero distance to another precinct
  shortest_dist <- min(distances[distances != 0])
  # add a connection between input precinct and closest other precinct
  adj_data <- add_adjacency(adj_data, precinct, which(distances == shortest_dist))
  adj_data
}

for(precinct in bad_precincts) {
  adj_ca_projected <- fix_adjacency(adj_ca_projected, CA_vote_join_projected, precinct)
}

redist_obj_ca_vote_t <- redist_map(
  data = CA_vote_join_projected,
  pop = CA_vote_join_projected$total_pop,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj_ca_projected
) 


redist_obj_ca_vote_t$d_points <- redist_obj_ca_vote_t$data$dem_votes - redist_obj_ca_vote_t$data$rep_votes
redist_obj_ca_vote_t$ones <- rep(1,9129)




plans_ca_vote_t_2 <- redist_smc(
  redist_obj_ca_vote_t,
  nsims = 3000,      # number of plans
  runs = 1,
  ncores = 7,
  compactness = 1
)



plans_ca_vote_t_2 <- plans_ca_vote_t_2|>
  mutate(reock = comp_reock(plans_ca_vote_t_2, shp = CA_vote_join_projected))

plans_ca_vote_t_2 <- plans_ca_vote_t_2 |>
  mutate(reock = comp_reock(plans_ca_vote_t_2, shp = CA_vote_join_projected))

plans_ca_vote_t_2 <- plans_ca_vote_t_2 |>
  mutate(polsby = comp_polsby(plans_ca_vote_t_2, shp = CA_vote_join_projected))

plans_ca_vote_t_2 <- plans_ca_vote_t_2|>
  mutate(D_points = group_frac(redist_obj_ca_vote_t, d_points, ones),
         r_winner = ifelse(D_points < 0, 1, 0 ))


CA_bar_t_2 <- plans_ca_vote_t_2|>
  group_by(draw)|>
  summarize(r_winner = sum(r_winner))|>
  ungroup()|>
  arrange(r_winner)

saveRDS(plans_ca_vote_t_2, file = "proposed_maps_ca.rds")


best_average_plan_r <- plans_ca_vote_t_2 |>
  group_by(draw) |>
  summarize(avg_reock = mean(reock, na.rm = TRUE)) |>
  arrange(desc(avg_reock)) 


best_average_plan <- plans_ca_vote_t_2 |>
  group_by(draw) |>
  summarize(avg_polsby = mean(polsby, na.rm = TRUE)) |>
  arrange(desc(avg_polsby)) 


plan1_ca_vote_t <- get_plans_matrix(plans_ca_vote_t)[, 4]
mapca_polsby <- redist_obj_ca_vote_t$data |>
  mutate(district = factor(plan1_ca_vote_t))

#using ashers code

best_reock_vector <- get_plans_matrix(best_reock)[, 51236]

mapca_reock <- redist_obj_ca_vote_t$data |>
  mutate(district = factor(best_reock_vector))



mapca_plan_vote_t <- mapca_reock |>
  group_by(district) |>
  summarize(
    total_votes = sum(total_votes),
    total_dem = sum(dem_votes, na.rm = TRUE),
    total_rep = sum(rep_votes, na.rm = TRUE),
    total_vap = sum(total_vap),
    total_vap_hisp = sum(total_vap_hisp),
    total_vap_asian =sum(total_vap_asian),
    total_vap_black = sum(total_vap_black),
    total_vap_white = sum(total_vap_white),
    total_18_19 = sum(VA_18_19),
    total_20_24 = sum(VA_20_24),
    total_25_29 = sum(VA_25_29)
  )

mapca_plan_vote_t <- mapca_plan_vote_t |>
  mutate(
    vote_diff = total_dem - total_rep,            # Positive = Dem won, Negative = Rep won
    margin_pct = (total_dem - total_rep) / total_votes,
    d_winner = ifelse(vote_diff > 0, 1, 0),
    pct_va_18_19 = total_18_19/total_vap *100,
    pct_va_20_24 = total_20_24/total_vap *100,
    pct_va_25_29 = total_25_29/total_vap *100,
    pct_vap_hisp = total_vap_hisp/total_vap *100,
    pct_vap_black = total_vap_black/total_vap *100,
    pct_vap_white = total_vap_white/total_vap *100,
    pct_vap_asian = total_vap_asian/total_vap *100,
    total_minority_vap = (total_vap_hisp + total_vap_black + total_vap_asian),
    pct_minority_vap = total_minority_vap / total_vap * 100# Percentage lead
  )    


mapca_district_plan1_t <- mapca_reock|>
  group_by(district)|>
  summarize()



nrow(mapca_plan1_vote)-sum(mapca_plan1_vote$d_winner)


mapca_plan_vote_t$margin_bin <- cut(
  mapca_plan_vote_t$margin_pct,
  breaks = c(-Inf, -0.15, -0.10, -0.05, -0.01, 0, 0.01, 0.10, 0.20, 0.50, Inf),
  labels = c(
    "R +15% or more",
    "R +15% to R +10%",
    "R +10% to R +5%",
    "R +5% to R +1%",
    "R +1% to 0%",
    "0% to D +1%",
    "D +1% to D +10%",
    "D +10% to D +20%",
    "D +20% to D +50%",
    "D +50% or more"
  ),
  include.lowest = TRUE,
  right = FALSE
)



mapca1_t <- ggplot(mapca_plan1_vote_t) +
  geom_sf(aes(fill = margin_bin)) +
  scale_fill_manual(
    values = c(
      "R +15% or more"     = "#67001f",
      "R +15% to R +10%"   = "#b2182b",
      "R +10% to R +5%"    = "#d6604d",
      "R +5% to R +1%"     = "#f4a582",
      "R +1% to 0%"        = "#fddbc7",
      "0% to D +1%"        = "#d1e5f0",
      "D +1% to D +10%"    = "#92c5de",
      "D +10% to D +20%"   = "#4393c3",
      "D +20% to D +50%"   = "#2166ac",
      "D +50% or more"     = "#053061"
    ),
    name = "Margin %"
  ) +
  geom_sf(data = mapca_district_plan1_t, fill = NA, color = "black", linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Proposed Redistricting for California: Win Margins")
mapca1_t

## tmap for roeck


tmap_mode("view")

tm_mapca_t <- tm_shape(mapca_plan_vote_t) +
  tm_polygons(
    col = "margin_bin",
    palette = c(
      "R +15% or more" = "#67001f",
      "R +15% to R +10%" = "#b2182b",
      "R +10% to R +5%" = "#d6604d",
      "R +5% to R +1%" = "#f4a582",
      "R +1% to 0%" = "#fddbc7",
      "0% to D +1%" = "#d1e5f0",
      "D +1% to D +10%" = "#92c5de",
      "D +10% to D +20%" = "#4393c3",
      "D +20% to D +50%" = "#2166ac",
      "D +50% or more" = "#053061"
    ),
    title = "Margin %",
    popup.vars = TRUE
  ) +
  tm_shape(mapca_district_plan1_t) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_layout(
    title = "Proposed Redistricting for California: Win Margins",
    legend.outside = TRUE
  )
tm_mapca_t

tmap_save(tm_mapca_t, "my_calimap.html")

saveRDS(mapca_plan_vote_t, file = "California_proposed_data.rds")


saveRDS(map_ca, file = "Map_Ca")
### roeck for LA 



LA_vote_join_projected <- st_transform(join_vtd_pop2_filter, crs = 5070)

adj_la_projected <- redist.adjacency(LA_vote_join_projected)

redist_obj_la2_p <- redist_map(
  data = LA_vote_join_projected,
  pop = LA_vote_join_projected$pop,
  ndists = 6,
  pop_tol = 0.01,
  adj = adj_la_projected
)

redist_obj_la2_p$d_points <- redist_obj_la2_p$data$total_demo - redist_obj_la2_p$data$total_rep
redist_obj_la2_p$ones <- rep(1,3639)

plans_la2_p <- redist_smc(
  redist_obj_la2_p,
  nsims = 50,      # number of plans
  runs = 2,# independent chains
  compactness = 1
)

plans_la2_p <- plans_la2_p |>
  mutate(reock = comp_reock(plans_la2_p, shp = LA_vote_join_projected))

plans_la2_p <- plans_la2_p|>
  mutate(D_points = group_frac(redist_obj_la2_p, d_points, ones),
         r_winner = ifelse(D_points < 0, 1, 0 ))

LA_bar <- plans_la2_p|>
  group_by(draw)|>
  summarize(r_winner = sum(r_winner))|>
  ungroup()|>
  arrange(r_winner)


best_la_average_plan_r <- plans_la2_p |>
  group_by(draw) |>
  summarize(avg_reock = mean(reock, na.rm = TRUE)) |>
  arrange(desc(avg_reock))

planla1_p <- get_plans_matrix(plans_la2_p)[, 36]

mapla_polsby <- redist_obj_la2_p$data |>
  mutate(district = factor(planla1_p))


mapla_district_plan1_p <- mapla_polsby |>
  group_by(district)|>
  summarize()

la_results <- mapla_polsby|>      
  group_by(district) |>
  summarize(
    total_dem = sum(total_demo, na.rm = TRUE),
    total_rep = sum(total_rep, na.rm = TRUE),
    total_votes = sum(total_votes),
    total_pop = sum(pop),
    total_black = sum(pop_black, na.rm = TRUE),
    total_hispanic = sum(pop_hisp, na.rm = TRUE),
    total_white = sum(pop_white, na.rm = TRUE),
    total_vap       = sum(vap, na.rm = TRUE),
    total_vap_hisp  = sum(vap_hisp, na.rm = TRUE),
    total_vap_white = sum(vap_white, na.rm = TRUE),
    total_vap_black = sum(vap_black, na.rm = TRUE)
  )


la_results <- la_results|>
  mutate(d_prop = (total_dem/total_votes),
         r_prop = (total_rep/total_votes),
         winner = case_when(
           d_prop > 0.5 ~ "Democratic",
           r_prop > 0.5 ~ "Republican",
           TRUE         ~ "Tie/Other"),
         total_minority = (total_black+total_hispanic),
         total_minority_vap = (total_vap_hisp + total_vap_black),
         pct_minority = ( total_minority / total_pop * 100),
         pct_vap_hisp  = total_vap_hisp  / total_vap * 100,
         pct_vap_white = total_vap_white / total_vap * 100,
         pct_vap_black = total_vap_black / total_vap * 100,
         pct_minority_vap = total_minority_vap / total_vap * 100
  )

la_results <- la_results |>
  mutate(
    vote_diff = total_dem - total_rep,            # Positive = Dem won, Negative = Rep won
    margin_pct = (total_dem - total_rep) / total_votes # Percentage lead
  )    

la_results$margin_bin <- cut(
  la_results$margin_pct,
  breaks = c(-Inf, -0.60, -0.30, -0.10, -0.01, 0, 0.01, 0.10, 0.20, 0.30, Inf),
  labels = c(
    "R +60% or more",
    "R +60% to R +30%",
    "R +30% to R +10%",
    "R +10% to R +1%",
    "R +1% to 0%",
    "0% to D +1%",
    "D +1% to D +10%",
    "D +10% to D +20%",
    "D +20% to D +30%",
    "D +30% or more"
  ),
  include.lowest = TRUE,
  right = FALSE
)


m_la_p <- 
  tm_shape(la_results) +
  tm_polygons(
    col = "margin_bin",
    palette = c(
      "R +60% or more" = "#67001f",
      "R +60% to R +30%" = "#b2182b",
      "R +30% to R +10%" = "#d6604d",
      "R +10% to R +1%" = "#f4a582",
      "R +1% to 0%" = "#fddbc7",
      "0% to D +1%" = "#d1e5f0",
      "D +1% to D +10%" = "#92c5de",
      "D +10% to D +20%" = "#4393c3",
      "D +20% to D +30%" = "#2166ac",
      "D +30% or more" = "#053061"
    ),
    title = "Margin %",
    popup.vars = TRUE
  ) +
  tm_shape(mapla_district_plan1_p) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(
    title = "Proposed Redistricting for Louisiana: Win Margins",
    legend.outside = TRUE
  )

m_la_p




#### checking reock of old california map 2020

reock_scores_2020 <- map_ca |>
  group_by(cd_2020) |>
  summarize(geometry = st_union(geometry)) |>
  mutate(
    area = st_area(geometry),
    circle = st_minimum_bounding_circle(geometry),
    circle_area = st_area(circle),
    reock = as.numeric(area / circle_area)
  )

reock_scores_2020 |>
  summarize(avg_reock = mean(reock, na.rm = TRUE))
