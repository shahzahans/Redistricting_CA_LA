library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(dplyr)

#Used a bit of Ximena's code to restructure the data file
ca_cvap_2024_bg <- read_csv("ca_cvap_2024_bg.csv")

subset_ca_cvap_2024_bg <- ca_cvap_2024_bg |>
   select(GEOID, COUNTY, COUNTYFP, TRACTCE, BLKGRPCE, C_TOT24, C_AMI24, C_ASI24, C_BLA24,C_NHP24, C_WHT24, C_HSP24, CVAP_TOT24,CVAP_AMI24, CVAP_ASI24,CVAP_BLA24, CVAP_NHP24, CVAP_WHT24, CVAP_HSP24)

subset_ca_cvap_2024_bg |>
  arrange(desc(C_HSP24)) |>
  ggplot(aes(x = reorder(COUNTY, C_HSP24), y = C_HSP24)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Hispanic population per county",
       x = "County",
       y = "Hispanic Population") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nonvotes <- read.csv("california/CA_l2_2024_pri_stats_2020block")

LA_table <- CA_l2_2024_pri_stats_2020block|>
  summarize(across(voted_eur:reg_unk_party_others, sum)) |>
  mutate(reg_others_party_democratic = reg_hisp_party_democratic + reg_esa_party_democratic + reg_oth_party_democratic + reg_unk_party_democratic,
         reg_others_party_republican = reg_hisp_party_republican + reg_esa_party_republican + reg_oth_party_republican + reg_unk_party_republican,
         pct_reg_others_party_democratic = (pct_voted_hisp_party_democratic + pct_voted_esa_party_democratic + pct_voted_oth_party_democratic + pct_voted_unk_party_democratic)/4,
         pct_reg_others_party_republican = (pct_voted_hisp_party_republican + pct_voted_esa_party_republican + pct_voted_oth_party_republican + pct_voted_unk_party_republican)/4)
View(LA_table)

subset_ca_hisp <- LA_table |>
  select(voted_hisp_party_democratic, reg_hisp_party_democratic, voted_hisp_party_republican, reg_hisp_party_republican, voted_aa_party_democratic,
         reg_aa_party_democratic, voted_aa_party_republican, reg_aa_party_republican, voted_esa_party_democratic, reg_esa_party_democratic, 
         voted_esa_party_republican, reg_esa_party_republican, voted_eur_party_democratic, reg_eur_party_democratic, voted_eur_party_republican, reg_eur_party_republican)
View(subset_ca_hisp)


