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
