install.packages('alarmdata')

library(alarmdata)
library(redist)



map_ca <- alarm_50state_map('CA')
plans_ca <- alarm_50state_plans('CA')

library(ggplot2)
library(ggredist)
map_ca |> 
  ggplot() +
  geom_sf(aes(fill = ndv / (nrv + ndv))) +
  scale_fill_party_c() +
  theme_map()
install.packages('ggredist')

map_ca |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = nrv + ndv)) +
  scale_fill_party_c() +
  theme_map()


map_la <- alarm_50state_map('LA')
plans_la <- alarm_50state_plans('LA')

library(dplyr)

install.packages('PL94171')
install.packages('censable')
install.packages('redist')
install.packages("geomander")

map_la |> 
  ggplot() +
  geom_district(aes(group = cd_2020, fill = ndv, denom = nrv + ndv)) +
  scale_fill_party_c() +
  theme_map()



la_analysis <- map_la |>
  group_by(cd_2020) |>
  summarize(
    total_vap = sum(vap),
    hisp_vap = sum(vap_hisp),
    asian_vap = sum(vap_asian),
    black_vap = sum(vap_black),
    white_vap = sum(vap_white),
    hisp_prop = hisp_vap/ total_vap,
    asian_prop = asian_vap / total_vap,
    black_prop = black_vap/ total_vap,
    white_prop = white_vap / total_vap
  )


ggplot(la_analysis) + 
  geom_sf(aes(fill = hisp_prop)) + 
  scale_fill_gradient(low = "white", 
                      high = "darkred", 
                      labels = scales::percent,
                      name = "Black VAP") + 
  theme_minimal()

ca_analysis <- map_ca |>
  group_by(cd_2020) |>
  summarize(
    total_vap = sum(vap),
    hisp_vap = sum(vap_hisp),
    asian_vap = sum(vap_asian),
    black_vap = sum(vap_black),
    white_vap = sum(vap_white),
    hisp_prop = hisp_vap/ total_vap,
    asian_prop = asian_vap / total_vap,
    black_prop = black_vap/ total_vap,
    white_prop = white_vap / total_vap
  )

ggplot(ca_analysis) + 
  geom_sf(aes(fill = black_prop)) + 
  scale_fill_gradient(low = "white", 
                      high = "darkred", 
                      labels = scales::percent,
                      name = "black VAP") + 
  theme_minimal()



state_p24_voters_by_srprec <- read_csv("state_p24_voters_by_p24_srprec_1.csv")

X25_us_rep_votes <- read_excel("25-us-rep-congress.xlsx")

X25_us_rep_votes <- X25_us_rep_votes[-1, ]
names(X25_us_rep_votes)[2] <- "DEM"
names(X25_us_rep_votes)[3] <- "REP"

cleaned_up_precinct_ca_votes <- state_p24_voters_by_srprec |>
  select(1:10)

cleaned_up_precinct_ca_votes_analysis <- cleaned_up_precinct_ca_votes |>
  mutate(
    total_votes = (DEM + REP),
    prop_dem_vote = (DEM / total_votes),
    prop_rep_vote = (REP / total_votes)
  )


ggplot(cleaned_up_precinct_ca_votes_analysis, aes(x = factor(COUNTY), y = prop_dem_vote)) + 
  geom_boxplot()

4/03
install.packages("geojsonsf")
library(geojsonsf)
library(sf)



https://redistrictingdatahub.org/download/?datasetid=54717&document=%2Fweb_ready_stage%2Felections_results_precinct_boundaries_2024%2Fca_2024_gen_prec.zip

ca_prec_24 <- st_read("C:/Users/ximen/OneDrive/data and society/Redistricting_CA_LA/ca_precinct_2024/ca_gen_2024_prec.geojson")

#attempting to save to object??
ca_prec_24_sf_obj <- geojson_sf("C:/Users/ximen/OneDrive/data and society/Redistricting_CA_LA/ca_precinct_2024/ca_gen_2024_prec.geojson")



#new work 4/06

clean_ca_prec_24 <- ca_prec_24 |> 
  select(-starts_with("G24PRO"))
clean_ca_prec_24 <- clean_ca_prec_24|> 
  select(-starts_with("TOTREG"))
clean_ca_prec_24 <- clean_ca_prec_24|> 
  select(-9)
clean_ca_prec_24 <- clean_ca_prec_24|> 
  select(-(10:13))
# tidy all columns?
library(tidyverse)

clean_ca_prec_24_pivot <- clean_ca_prec_24 |>
  pivot_longer(
    cols = starts_with(c("G24", "S24", "GCO", "GSU", "GSL")), # Catch both General and Special elections
    names_to = c("Election_Type", "Office", "Party", "Candidate"),
    # This Regex looks for: 
    # 1. First 3 chars (G24)
    # 2. Next 3 chars (PRE/USS)
    # 3. Next 1 char (D/R)
    # 4. Remaining chars (TRU/SCH)
    names_pattern = "^(.{3})(.*)([DR])(.{3})$", 
    values_to = "Votes",
    values_transform = list(Votes = as.numeric)
  )


clean_ca_prec_24_pivot_filter <- clean_ca_prec_24_pivot |>
  filter(Votes > 0)

#summary_ca_prec_votes <- clean_ca_prec_24_pivot_filter |>
#group_by(PRECINCT, Office, Party) |> 
# 3. Sum the votes
#summarize(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop")
#summary_ca_prec_votes <- clean_ca_prec_24_pivot_filter |>
# Fix the "Edge crosses edge" errors automatically
# st_make_valid() |>
#group_by(PRECINCT, Office, Party) |> 
#summarize(Total_Votes = sum(Votes, na.rm = TRUE), .groups = "drop")


saveRDS(clean_ca_prec_24_pivot_filter, "clean_ca_prec_24_pivot_filter.rds")

clean_ca_prec_24_co <- clean_ca_prec_24_pivot_filter|>
  filter(Election_Type == "GCO")

ca_prec_24_co_summary <- clean_ca_prec_24_co|>
  st_drop_geometry()

ca_prec_24_co_summary <- ca_prec_24_co_summary|>
  group_by(PRECINCT,Party) |>
  summarize(Total_votes = sum(Votes,na.rm = TRUE), .groups = "drop")

ca_prec_24_co_summary <- ca_prec_24_co_summary |>
  pivot_wider(
    names_from = Party, 
    values_from = Total_votes
  )
ca_prec_24_co_summary <- ca_prec_24_co_summary |>
  mutate(
    Total_Votes = (D + R),
    prop_d = (D/Total_Votes),
    prop_r = (R/Total_Votes))


saveRDS(clean_ca_prec_24_co, "ca_prec_24_congress_votes.rds")
saveRDS(ca_prec_24_co_summary, "ca_prec_24_congress_summary.rds" )


#4/08

ca_cvap_2024_bg <- read_csv("C:/Users/ximen/OneDrive/data and society/ca_cvap_2024_bg.csv")

#4/09 work map precinct 2024 data winning party 

attempt_ca_precinct <- clean_ca_prec_24_pivot_filter |>
  group_by(UNIQUE_ID)|>
  select(UNIQUE_ID,geometry,Election_Type, Party, Votes)|>
  filter(Election_Type == "GCO")

attempt_ca_precinct_wider <- attempt_ca_precinct |>
  pivot_wider(
    names_from = Party, 
    values_from = Votes,
    values_fn = {sum},
    values_fill = 0
  )
attempt_ca_precinct_wider <- attempt_ca_precinct_wider |>
  mutate(
    Total_Votes = (D + R),
    prop_d = (D/Total_Votes),
    prop_r = (R/Total_Votes),
    winner = case_when(
      prop_d > 0.5 ~ "Democratic",
      prop_r > 0.5 ~ "Republican",
      TRUE         ~ "Tie/Other"
    ))

precinct_map_ca <- ggplot(data = attempt_ca_precinct_wider) +
  geom_sf(aes(fill = winner), color = "white", size = 0.05) +
  scale_fill_manual(
    values = c(
      "Democratic" = "#2E5B88", # Standard Blue
      "Republican" = "#D73027", # Standard Red
      "Tie/Other"  = "#CCCCCC"  # Gray for ties
    ),
    name = "Winning Party"
  ) +
  theme_minimal()
print(precinct_map_ca)

# map for voting citizens CA

subset_ca_cvap_2024_bg <- ca_cvap_2024_bg |>
  select(GEOID, COUNTY, COUNTYFP, TRACTCE, BLKGRPCE, C_TOT24, C_AMI24, C_ASI24, C_BLA24,C_NHP24, C_WHT24, C_HSP24, CVAP_TOT24,CVAP_AMI24, CVAP_ASI24,CVAP_BLA24, CVAP_NHP24, CVAP_WHT24, CVAP_HSP24)

library(tigris)
library(sf)

ca_shapes <- block_groups(state = "CA", cb = TRUE, year = 2024)

ca_map_cvap <- ca_shapes |>
  left_join(subset_ca_cvap_2024_bg, by = "GEOID")

#good graph
ggplot(data = ca_map_cvap) +
  geom_sf(aes(fill = CVAP_TOT24), color = NA) + # 'fill' colors the map based on your citizens estimate
  scale_fill_fermenter(breaks  = c(0, 50,500,1000,1500,5000,10000), name = "# of Voting Citizens ", limits= c(0, 13000), palette = "YlOrRd", direction = 1) +
  theme_minimal() 




# precinct map of CA with grey areas(ties/other)

test1 <- clean_ca_prec_24_pivot |>
  filter(Election_Type == "GCO")|>
  select(UNIQUE_ID,geometry,Election_Type, Party, Votes)


test1_pivot <- test1|>
  pivot_wider(
    names_from = Party, 
    values_from = Votes,
    values_fn = {sum},
    values_fill = 0
  )

test1_pivot <- test1_pivot|>
  mutate(
    Total_Votes = (D + R),
    prop_d = (D/Total_Votes),
    prop_r = (R/Total_Votes),
    winner = case_when(
      prop_d > 0.5 ~ "Democratic",
      prop_r > 0.5 ~ "Republican",
      TRUE         ~ "Tie/Other"
    ))


test_map_ca <- ggplot(data = test1_pivot) +
  geom_sf(aes(fill = winner), color = NA, size = 0.05) +
  scale_fill_manual(
    values = c(
      "Democratic" = "#2E5B88", # Standard Blue
      "Republican" = "#D73027", # Standard Red
      "Tie/Other"  = "#CCCCCC"  # Gray for ties
    ),
    name = "Winning Party"
  ) +
  theme_minimal()
print(test_map_ca)
