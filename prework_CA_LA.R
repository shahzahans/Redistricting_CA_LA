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
  geom_sf(aes(fill = black_prop)) + 
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


