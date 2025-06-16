library(dplyr)

merged_goeie <- merged_goeie %>%
  mutate(Category_cafes = if_else(
    Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km.> 1,
    "Cafes further than 1 km",
    "Cafes closer than 1 km"
  ))