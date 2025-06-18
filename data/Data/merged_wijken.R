merged_data <- merged_data %>%
  mutate(Distance_cafes = if_else(
    Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km.> 1,
    "Cafes further than 1 km",
    "Cafes closer than 1 km"
  ))

write.csv(merged_data, "data/Data/merged_data.csv")

#multipolygon koppelen aan merged_goeie (die wijkcodes)

merged_2016 <- merged_data %>%
  filter(Perioden == 2016)

merged_wijken <- wijken_sf %>%
  rename("Regioaanduiding.Codering..code." = "statcode")

merged_2016 <- merged_2016 %>% full_join(merged_wijken, by = "Regioaanduiding.Codering..code.")