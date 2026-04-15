### Pulling data
shp <- st_read("Data_Folder/Loui_DATA/la_2024_gen_all_prec")
nonvotes <- read_csv("Data_Folder/Loui_DATA/LA_l2_2024_gen_stats_2020county.csv")

### A less than ideal way to change fips codes
nonvotes$countyfips <- sub("[2]", "", nonvotes$countyfips)
nonvotes$countyfips <- sub("[2]", "", nonvotes$countyfips)


### Giving the nonvotes dataset Parish st objects
geometries <- shp |>
  group_by(COUNTYFP) |>
  summarize()
nonvotes <- geometries |>
  left_join(nonvotes, by = c("COUNTYFP" = "countyfips"))



### Setting up map data
nonvotes_map <- nonvotes |>
  mutate(d_points_voted = voted_party_democratic - voted_party_republican,
         d_points_reg = reg_party_democratic - reg_party_republican) |>
  select(COUNTYFP,countyname, geometry, d_points_voted, d_points_reg) |>
  pivot_longer(cols = c("d_points_reg", "d_points_voted"),
               names_to = "Reg_or_Voted",
               values_to = "d_points")


### Making the map. Note that the breaks are weird looking. I'll try to fix it. 
### The fill scale probably needs readjusting for Cali.
nonvotes_map |>
  ggplot() +
  geom_sf(color = "black", aes(fill = cut(
    d_points,
    breaks = c(-50034,-2000,-500,-164,0,61,500,2000, 136599)))) +
  scale_fill_manual("", values = c("#b2182b","#d6604d","#f4a582","#fddbc7", "#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  facet_wrap(~ Reg_or_Voted)

