library(alarmdata)
library(redist)
library(tidyverse)
library(ggredist)
library(sf)
library(geojsonsf)
library(readxl)
library(ggblend)
map_la <- alarm_50state_map('LA')
plans_la <- alarm_50state_plans('LA')
shp <- st_read("la_2024_gen_all_prec")
sf_obj <- geojson_sf("https://redistricting.lls.edu/wp-content/uploads/la_2020_congress_2024-01-22.json")
my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
  }



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

turnout <- read_csv("LA_l2_2024_gen_stats_2020county.csv")

party_voted <- turnout |>
  summarize(vote_dem = sum(voted_party_democratic), reg_dem = sum(reg_party_democratic), 
            vote_rep = sum(voted_party_republican), reg_rep = sum(reg_party_republican),
            vote_ind = sum(voted_party_ind_npp), reg_ind = sum(reg_party_ind_npp),
            vote_other = sum(voted_party_others), reg_other = sum(reg_party_others))

race_voted <- turnout |> 
  summarize(vote_european = sum(voted_eur), reg_european = sum(reg_eur), 
            vote_hispanic = sum(voted_hisp), reg_hispanic = sum(reg_hisp),
            vote_africanamerican = sum(voted_aa), reg_africanamerican = sum(reg_aa),
            vote_esa = sum(voted_esa), reg_esa = sum(reg_esa),
            vote_other = sum(voted_oth), reg_other = sum(reg_oth),
            vote_unknown = sum(voted_unk), reg_unknown = sum(reg_unk))


  




### Load Datasets


precincts2024 <- readRDS("precincts2024.rds")

### Plots


just_parishes <- precincts2024 |>
  ggplot() +
  geom_sf(aes(fill = Parish), color = NA) +
  theme(legend.position = "none") +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)
just_parishes


precincts_votes <- precincts2024 |>
  group_by(GEOID20) |> 
  mutate(total = sum(votes)) |>
  group_by(GEOID20, party) |>
  mutate(prop_votes = d_votes / total) |>
  group_by(GEOID20) |>
  slice_max(order_by = abs(prop_votes), n = 1) |>
  ggplot() +
  geom_sf(data=sf_obj, fill = "gray40", color = NA) +
  geom_sf(aes(fill = 100 * prop_votes), color = "gray20", linewidth = 0.01) +
  scale_fill_gradient2(low = "#b2182b",
                       mid = "white",
                       high = "#2166ac",
                       midpoint = 0,
                       breaks = c(-100, 0, 100),
                       labels = c("100% R", "0%", "100% D")) +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)
precincts_votes +
  my_map_theme() +
  labs(title = "Percentage of Votes in each Precinct",
       subtitle = "The percentages are of the votes for the Democrat or Republican party\n in the US House race",
       fill = "Percentage")


num_votes_map <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(votes = sum(votes)) |>
  ggplot() +
  geom_sf(aes(fill = cut(votes, 
                         breaks = c(0,50, 100, 250, 500, 750, 100000), 
                         labels = c("50", "100", "250", "500", "750", "750+")))) +
  scale_fill_manual(values = c("gray90", "gray80", "gray65", "gray45", "gray20", "gray10")) +
  theme_bw() +
  geom_sf(data = sf_obj, fill = NA, color = "darkred", linewidth = 1)
num_votes_map +
  labs(fill = "# of Votes",
       title = "Number of Votes in each Precinct") +
  my_map_theme() 



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
district_winner_map <- district_winner |>
  ggplot() +
  geom_sf(aes(fill = majority)) +
  scale_fill_gradient2(low = "#b2182b",
                       mid = "white",
                       high = "#2166ac",
                       midpoint = 0,
                       breaks = c(-0.3, -0.2, -0.1, 0, 0.1),
                       labels = c("30% R", "20% R", "10% R", "0%", "10% D"))
district_winner_map +
  my_map_theme() +
  labs(fill = "Majority %",
       title = "Percentage of Votes that each District's \nUS House Race Winner Won by",
       subtitle = "Percentages represent the proportion of votes over 50% (majority)",
       caption = "District 6 (Appears to be white) won by a democrat by a majority plus 0.7%")
### Needs prettying up 


agg_png("myplotplease.png", width = 2000, height = 1080, units = "px", res = 144)


num_votes_map <- precincts2024 |>
  group_by(GEOID20) |>
  summarize(votes = sum(votes)) |>
  ggplot() +
  geom_sf(color = NA, aes(fill = cut(votes, 
                                     breaks = c(2,50, 250, 1000, 2170)))) +
  scale_fill_manual(values = c("gray90", "gray80", "gray65", "gray45", "gray20", "gray10")) +
  labs(fill = "# of Votes",
       title = "Number of Votes in each Precinct") +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = centroids, aes(color = cut(
    winner_votes, 
    breaks = c(min(winner_votes),-500,-100,-1,0,100,5,500,max(winner_votes)), 
    labels = c("R+15 or more", "R+5 to R+15", "R+1 to R+5", "R+0 to R+1", "D+0 to D+1", "D+1 to D+5", "D+5 to D+15", "D+15 or more")))) +
  scale_color_manual("", values = c("#b2182b","#d6604d","#f4a582","#fddbc7", "#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  my_map_theme()
num_votes_map


centroids <- st_centroid(precincts2024, dist = 2) |>
  group_by(GEOID20) |>
  summarize(total_votes = sum(votes), winner_votes = sum(d_votes))



### Probably Worthless

districts2024 |>
  ggplot() +
  geom_sf(aes(fill = winner))

precincts2024 |>
  ggplot() +
  geom_sf(aes(fill = party, color = party), linewidth = 0.05) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  geom_sf(data = sf_obj, fill = NA, color = "black", linewidth = 1)

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
               values_to = "votes") |>
  mutate(party = ifelse((person == "GCON01NHYE"), "noparty",
                        ifelse((person == "GCON01DMAN" | person == "GCON02DCAR" | person == "GCON02DDAV" | person == "GCON03DGON" | person == "GCON03DSUM" | person == "GCON05DVAL" | person == "GCON06DAND" | person == "GCON06DFIE" | person == "GCON06DJON" | person == "GCON06DWIL"), "democrat", "republican")))|>
  filter(votes > 0, party != "other") |>
  mutate(d_votes = ifelse(party == "republican", (-1)*votes, votes))

precincts <- precincts |>
  group_by(district) |>
  summarize(votes = sum(votes))
precincts |>
ggplot() +
  geom_sf() +
  theme_dark()



map_la |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = nrv + ndv)) +
  scale_fill_party_c() +
  theme_map()








LA_CVAP <- la_cvap_2024_2020_b |>
  select(GEOID20, CVAP_TOT24:CVAP_BLK24)

summary(LA_CVAP)

library(arcgis)
library(arcgisbinding)



shp2 <- st_read("la_testish")

LA_CVAP <- shp2 |>
  select(GEOID20, VAP_TOTAL:VAP_HISPAN, geometry, COUNTY)

ggplot(data = LA_CVAP) +
  geom_sf(aes(fill = VAP_BLACK / VAP_TOTAL), color = NA) +
  scale_fill_gradient(low = "white",
                      high = "purple") +
  geom_sf(data = precincts2024, color = "black", fill = NA)






LA_CVAP_small <- shp2 |>
  filter(COUNTY == 125) |>
  select(GEOID20, VAP_TOTAL:VAP_HISPAN, geometry) 

LA_CVAP_small |>
  ggplot() +
  geom_sf(aes(fill = VAP_BLACK / VAP_TOTAL), color = NA)



LA_CVAP_parish <- LA_CVAP |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(COUNTY) |>
  summarize(VAP_TOTAL = sum(VAP_TOTAL), VAP_WHITE = sum(VAP_WHITE), VAP_BLACK = sum(VAP_BLACK), 
            VAP_ASIAN = sum(VAP_ASIAN), VAP_AMIND = sum(VAP_AMIND), VAP_OTHER = sum(VAP_OTHER),
            VAP_HISPAN = sum(VAP_HISPAN))



testing <- shp2 |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(COUNTY, VTD) |>
  summarize(VAP_TOTAL = sum(VAP_TOTAL), VAP_WHITE = sum(VAP_WHITE), VAP_BLACK = sum(VAP_BLACK), 
            VAP_ASIAN = sum(VAP_ASIAN), VAP_AMIND = sum(VAP_AMIND), VAP_OTHER = sum(VAP_OTHER),
            VAP_HISPAN = sum(VAP_HISPAN)) |>
  unite("join", COUNTY:VTD)


testing2 <- shp |>
  select(COUNTYFP, NAME20, Parish, geometry) |>
  unite("join", COUNTYFP:NAME20)

setdiff(testing$join, testing2$join)
setdiff(testing2$join, testing$join)

testing2$join <- gsub("Precinct ", "", testing2$join)
testing2$join <- gsub("Voting Districts Not Defined", "ZZZZZZ", testing2$join)


precinctdemo <- testing2 |>
  left_join(testing) |>
  drop_na()

precinctdemo |>
  ggplot() +
  geom_sf(aes(fill = VAP_BLACK / VAP_TOTAL))





generate_dots <- function(polygon, value, dots_per_unit = 1) {
  n_dots <- round(value * dots_per_unit)
  
  if (is.na(value) || value <= 0) return(NULL)
  
  n_dots <- round(value * dots_per_unit)
  if (n_dots <= 0) return(NULL)
  
  st_sample(st_geometry(polygon), size = n_dots, type = "random")
}

# Generate dots for "Murder" rate
dots_list <- lapply(1:nrow(precinctdemo), function(i) {
  generate_dots(precinctdemo[i, ], precinctdemo$VAP_BLACK[i], dots_per_unit = 2)
})

# Combine all dots into one sf object
dots_sf <- do.call(c, compact(dots_list))












precinctdemo <- precinctdemo |>
  mutate(prop_black = if_else(VAP_BLACK > 0, VAP_BLACK / VAP_TOTAL, NA_real_))
centroids <- st_point_on_surface(precinctdemo)
precinctdemo <- precinctdemo |>
  mutate(prop_white = if_else(VAP_WHITE > 0, VAP_WHITE / VAP_TOTAL, NA_real_))
centroids2 <- st_point_on_surface(precinctdemo)


ggplot() +
  geom_sf(data = centroids)



library(ragg)

agg_png("myplotplease.png", width = 1089, height = 1080, units = "px", res = 144)


p <- ggplot() +
  geom_sf(data = precinctdemo, fill = "white", color = "gray70") +
  
  # First blended layer (White VAP)
  ggblend::blend(
    geom_sf(
      data = centroids2,
      aes(size = prop_white),
      color = "yellow",
      alpha = 0.5
    ) *
    blend("lighten")
  ) +
  
  # Second blended layer (Black VAP)
  ggblend::blend(
    geom_sf(
      data = centroids,
      aes(size = prop_black),
      color = "blue",
      alpha = 0.5
    ) *
    blend("multiply",
    alpha = 0.5)
  ) +
  
  scale_size_continuous(range = c(0.2, 2)) +
  theme_minimal() +
  geom_sf(data = sf_obj, color = "black", linewidth = 1, fill = NA) +
  my_map_theme()

print(p)
dev.off()







boxplonk <- racedemo2026 |>
  filter(Parish == "STATE") |>
  select(Reg_White:Reg_Other) |>
  pivot_longer(cols = Reg_White:Reg_Other, names_to = "demo", values_to = "voters") |>
  mutate(polys = ifelse(demo == "Reg_White", y = c(0,1851706), 
                        ifelse(demo == "Reg_Black", y = c(0,925582), y = c(187404))))
  

ggplot(data = boxplonk) +
  geom_polygon(aes(x = demo))
    