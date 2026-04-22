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



#redistricitng steps for CA


total_pop_ca_2020 = sum(map_ca$pop)

test1_pivotmap <- test1_pivot |>
  mutate(total_pop_ca_2020)

adj <- redist.adjacency(test1_pivotmap2)

redist_obj_ca <- redist_map(
  data = test1_pivotmap2,
  pop = total_pop_ca_2020,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj
)

disconnected = test1_pivotmap[c(3213, 4773, 5811, 6376, 18691, 18912, 18913, 19814, 22999, 23000), ]

test1_pivotmap2 <- test1_pivotmap[-c(3213, 4773, 5811, 6376, 18691, 18912, 18913, 19814, 22999, 23000), ]


# redistricting for LA 

total_value <- sum(map_la$pop, na.rm = TRUE)

la_map_redist <- precincts2024 |>
  mutate(total_pop_la_2020 = total_value)

adj_la <- redist.adjacency(la_map_redist)

redist_obj_la <- redist_map(
  data = la_map_redist,
  pop = la_map_redist$total_pop_la_2020,
  ndists = 6,
  pop_tol = 0.01,
  adj = adj_la
)


## add back total registered voters column 

test1_pivot_with_reg <- test1_pivot |>
  mutate(ca_prec_24$TOTREG)

saveRDS(test1_pivot_with_reg, "test1_pivot_with_reg.rds" )

test1_pivot_with_reg <- test1_pivot_with_reg |>
  mutate(turnout = Total_Votes / ca_prec_24$TOTREG)

test1_pivot_with_reg <- test1_pivot_with_reg |>
  mutate(turnout_no_inf = ifelse(ca_prec_24$TOTREG > 0, Total_Votes / ca_prec_24$TOTREG, NA_real_))



#boxplot of the turnout rate


Turnout_boxplot_ca <- 
  ggplot(test1_pivot_with_reg, aes(y = turnout, x = winner, color = winner)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                   limits = c(0, 1)) +
  scale_color_manual(values = c(
    "Democratic" = "#2E74C0",
    "Republican" = "#CB454A",
    "Tie/Other" = "gray50"
  ))+
  labs(
    x = "Winning Party (Democrat, Republican, Tie/Other)",
    y = "Turnout Percent",
    title = "Turnout Percent for each Party in California"
  ) +
  theme_minimal()

Turnout_boxplot_ca

#map of turnout CA

turnout_map_ca <- ggplot(test1_pivot_with_reg) +
  geom_sf(aes(fill = turnout_no_inf), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       limits = c(0,1),
                       name = "Turnout") +
  labs(
    title = "Voter Turnout by Precinct",
    subtitle = "Turnout = Total Votes / Total Registered",
    caption = "Precinct-level turnout"
  ) +
  theme_minimal()

turnout_map_ca





# ca pl9471 work


CA_geom <- pl_tidy_shp("CA", year = 2020, path = pl_url("CA", year = 2020))


save(CA_geom, file = "CA_geom.RData")

CA_geom <- CA_geom |>
  mutate(GEOID_11 = substr(GEOID, 1, 11))

CA_geom_summary <- CA_geom |>
  group_by(GEOID_11)|>
  summarize(Prec_pop = sum(pop))

# joining the above precinct pop with ca data ?

joined_ca <- test1_pivotmap2 |>
  left_join(
    st_drop_geometry(CA_geom_summary),
    by = c("UNIQUE_ID" = "GEOID_11")
  )

# la pl9471 work 
library("PL94171")

LA_geom <- pl_tidy_shp("LA", year = 2020, path = pl_url("LA", year = 2020))


LA_geom_summary <- LA_geom |>
  group_by(vtd)|>
  summarize(Prec_pop = sum(pop))



LA_map_grouping <- la_map_redist |>
  group_by(GEOID20)|>
  summarize( total_votes = sum(votes))


join_vtd_pop <- LA_map_grouping|>
  left_join(
    st_drop_geometry(map_la),
    by = c("GEOID20" = "GEOID")
  )


#redistricing plan for LA 


join_vtd_pop_filter <- join_vtd_pop|>
  mutate(pop = replace_na(pop, 0))

adj_la <- redist.adjacency(join_vtd_pop_filter)

redist_obj_la <- redist_map(
  data = join_vtd_pop_filter,
  pop = join_vtd_pop_filter$pop,
  ndists = 6,
  pop_tol = 0.01,
  adj = adj_la
)


plans_la <- redist_smc(
  redist_obj_la,
  nsims = 100,      # number of plans
  runs = 2           # independent chains
)


planla1 <- get_plans_matrix(plans_la)[, 1]

mapla_plan1 <- redist_obj_la$data |>
  mutate(district = factor(planla1))

potential_la_redist_map <- ggplot(mapla_plan1) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  theme_minimal()

potential_la_redist_map


planla2 <- get_plans_matrix(plans_la)[, 2]

mapla_plan2 <- redist_obj_la$data |>
  mutate(district = factor(planla2))

potential_la_redist_map2 <- ggplot(mapla_plan2) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  theme_minimal()

potential_la_redist_map2

points_black <- mapla_plan2 |>
  sf::st_centroid()


potential_la_redistmap2_black_pop <- ggplot(mapla_plan2) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  geom_sf(
    data = points_black,
    aes(size = pop_black),
    alpha = 0.7) +
  scale_size(range = c(1, 10)) +
  theme_minimal()

CA_block_24 <- st_read("C:/Users/ximen/OneDrive/large files/state_g24_voters_by_block20.csv")


CA_block_24_clean <- CA_block_24|>
  select(-c(AIP,PAF,MSC,LIB,NLP,GRN, REF, DCL,HISPDCL, HISPOTH, JEWDCL, JEWOTH, KORDCL,KOROTH, JPNDCL, JPNOTH,CHIDCL, CHIOTH, INDDCL,INDOTH, VIETDCL, VIETOTH, FILDCL, FILOTH, DEMMUNK, DEMFUNK, REPMUNK, REPFUNK, DCLMUNK,DCLM1824, DCLM2534, DCLM3544, DCLM4554, DCLM5564,	
            DCLM65PL, 	
            DCLFUNK,	
            DCLF1824,	
            DCLF2534,	
            DCLF3544,	
            DCLF4554,	
            DCLF5564,	
            DCLF65PL,	
            OTHMUNK,	
            OTHM1824,
            OTHM2534,
            OTHM3544, 
            OTHM4554,
            OTHM5564,	
            OTHM65PL,
            OTHFUNK,		
            OTHF1824,
            OTHF2534,	
            OTHF3544,	
            OTHF4554,	
            OTHF5564,
            OTHF65PL,DREG1G,DREG2G,DREG3G,DREG4G,DREG5G,DREG6G, DREG7G, DREG8G,DREG9G, RREG1G, RREG2G, RREG3G, RREG4G, RREG5G, RREG6G, RREG7G, RREG8G, RREG9G, IREG1G, IREG2G, IREG3G,IREG4G, IREG5G, IREG6G, IREG7G, IREG8G, IREG9G, OREG1G,OREG2G, OREG3G, OREG4G,OREG5G,OREG6G, OREG7G,OREG8G, OREG9G))


CA_block_24_clean <- CA_block_24_clean|>
  select(-c(TYPE, ELECTION, GEO_TYPE))

library(tigris)

ca_shapes <- block_groups(state = "CA", cb = TRUE, year = 2024)

CA_blocks <- blocks(state = "CA", year = 2024)


CA_PL_geo <- CA_blocks |>
  left_join(
    st_drop_geometry(CA_geom),
    by = c("GEOID20" = "GEOID"))

save(CA_PL_geo, file = "CA_PL_geo.RData")


adj_ca_2 <- redist.adjacency(CA_PL_geo)

redist_obj_ca2 <- redist_map(
  data = CA_PL_geo,
  pop = CA_PL_geo$pop,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj_ca_2
)

CA_PL_geo_contig <- CA_PL_geo[-c(71025, 95136, 114692, 116438, 120223, 120224, 120225, 120226, 120227,
                                 120228, 120333, 120334, 120343, 120347, 145611, 145612, 145614, 145615, 145617, 160885, 160926,
                                 166355, 166356, 166357, 166360, 166364, 166365, 176189, 176190, 176289, 176348, 181577, 181591,
                                 185363, 185374, 187030, 194505, 194506, 196325, 196327, 196328, 196330, 208687, 223834, 224282,
                                 230703, 258747, 264681, 264682, 264688, 264885, 274741, 277551, 277568, 280048, 280049, 283113,
                                 285344, 295150, 296907, 296908, 296909, 298546, 302260, 302271, 305447, 320203, 320204, 320205,
                                 320206, 320207, 320435, 324296, 324298, 324299, 324302, 324303, 324304, 324305, 324308, 324311,
                                 324313, 324317, 324320, 324322, 324323, 324324, 336049, 336930, 336931, 336932, 336933, 336934,
                                 336940, 336958, 336974, 350312, 353599, 353600, 353601, 353602, 353603, 353726, 356472, 356486,
                                 358316, 358317, 358318, 358319, 358320, 358321, 358322, 358323, 358324, 358325, 358326, 358327,
                                 358328, 361015, 361025, 361039, 361041, 362174, 368562, 368563, 368564, 368565, 368566, 368567,
                                 368568, 373084, 373085, 377797, 396946, 396949, 396954, 396959, 397517, 397518, 401457, 401458,
                                 401460, 401461, 401462, 401463, 401465, 401466, 401467, 401468, 401469, 401470, 404968, 416352,
                                 417318, 433433, 449097, 449106, 449119, 449121, 449122, 458687, 474367, 474368, 474369, 474370,
                                 474371, 513344, 513357, 513388),]



CA_PL_geo_contig_data <- CA_PL_geo_contig|>
  left_join(
    CA_block_24_clean,
    by = c("GEOID20" = "BLOCK20"))


adj_ca_3 <- redist.adjacency(CA_PL_geo_contig_data)

redist_obj_ca3 <- redist_map(
  data = CA_PL_geo_contig_data,
  pop = CA_PL_geo_contig_data$pop,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj_ca_3
)

plans_ca <- redist_smc(
  redist_obj_ca3,
  nsims = 3,      # number of plans
  runs = 2           # independent chains
)

#reduce the number of observations in the ca geo cont data


CA_numeric_group_attempt <- CA_PL_geo_contig_data |>
  mutate(across(
    c(TOTREG_R, DEM, REP, MALE, FEMALE, HISPDEM, HISPREP,
      DEMM1824, DEMM2534, DEMM3544, DEMM4554, DEMM5564, DEMM65PL,
      DEMF1824, DEMF2534, DEMF3544, DEMF4554, DEMF5564, DEMF65PL, REPM1824, REPM2534, REPM3544, REPM4554, REPM5564,
      REPM65PL, REPF1824, REPF2534, REPF3544, REPF4554, REPF5564, REPF65PL),
    ~ as.numeric(.x)
  ))

ca_group_attempt <- CA_numeric_group_attempt |>
  group_by(GEOID_11) |>
  summarise(POP = sum(pop), total_hisp_pop = sum(pop_hisp), total_white_pop = sum(pop_white), total_black_pop = sum(pop_black), total_asian_pop = sum(pop_asian), total_other_pop = sum(pop_other), total_reg = sum(TOTREG_R), total_dem = sum(DEM), total_rep = sum(REP), total_male = sum(MALE), total_female = sum(FEMALE), total_hisp_dem = sum(HISPDEM), total_hisp_rep = sum(HISPREP), total_demm1824 = sum(DEMM1824), total_demm2534 = sum(DEMM2534), total_demm3544 = sum(DEMM3544), total_demm4554 = sum(DEMM4554), total_demm5564 = sum(DEMM5564), total_demm64PL = sum(DEMM65PL), total_demf1824 = sum(DEMF1824), total_demf2534 = sum(DEMF2534), total_demf3544 = sum(DEMF3544), total_demf4554 = sum(DEMF4554), total_demf5564 = sum(DEMF5564), total_demf65PL = sum(DEMF65PL),
            total_repm1824 = sum(REPM1824), total_repm2534 = sum(REPM2534), total_repm3544 = sum(REPM3544), total_repm4554 = sum(REPM4554), total_repm5564 = sum(REPM5564), total_repm64PL = sum(REPM65PL),
            total_repf1824 = sum(REPF1824), total_repf2534 = sum(REPF2534), total_repf3544 = sum(REPF3544), total_repf4554 = sum(REPF4554), total_repf5564 = sum(REPF5564), total_repf64PL = sum(REPF65PL), na.rm = TRUE)


adj_ca_4 <- redist.adjacency(ca_group_attempt)

redist_obj_ca4 <- redist_map(
  data = ca_group_attempt,
  pop = ca_group_attempt$POP,
  ndists = 52,
  pop_tol = 0.01,
  adj = adj_ca_4
)

plans_ca <- redist_smc(
  redist_obj_ca4,
  nsims = 5,      # number of plans
  runs = 2           # independent chains
)

planca1 <- get_plans_matrix(plans_ca)[, 1]

mapca_plan1 <- redist_obj_ca4$data |>
  mutate(district = factor(planca1))

potential_ca_redist_map <- ggplot(mapca_plan1) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  theme_minimal()

potential_ca_redist_map

planca2 <- get_plans_matrix(plans_ca)[, 2]

mapca_plan2 <- redist_obj_ca4$data |>
  mutate(district = factor(planca2))

potential_ca_redist_map2 <- ggplot(mapca_plan2) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  theme_minimal()

potential_ca_redist_map2

# with district fill 
mapca_district_plan1 <- mapca_plan1 |>
  group_by(district)|>
  summarize()

potential_ca_redist_map_lines <- ggplot(mapca_plan1) +
  geom_sf(aes(fill = district)) +
  scale_fill_viridis_d() +
  geom_sf(data = mapca_district_plan1, fill = NA, color = "black", linewidth = 1) +
  theme_minimal()

potential_ca_redist_map_lines
