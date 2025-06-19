merged_data <- merged_data %>%
  mutate(Distance_cafes = if_else(
    Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km.> 1,
    "Cafes further than 1 km",
    "Cafes closer than 1 km"
  ))

write.csv(merged_data, "data/Data/merged_data.csv")

#multipolygon koppelen aan merged_goeie (die wijkcodes)
library(cbsodataR)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)

maps      <- cbs_get_maps()
prov_year <- max(maps$year[maps$region == "provincie"])
wijk_year <- max(maps$year[maps$region == "wijk"])

zh_sf_raw     <- cbs_get_sf("provincie", prov_year) |>
  filter(statnaam == "Zuid-Holland")

wijken_sf_raw <- cbs_get_sf("wijk", wijk_year)

gem_codes <- c("0599", "1931")                      # GM-codes → digits
wijken_subset_raw <- wijken_sf_raw |>
  mutate(gem_code = str_extract(statcode, "\\d{4}")) |>
  filter(gem_code %in% gem_codes)

zh_sf        <- st_as_sf(zh_sf_raw)
wijken_subset <- st_as_sf(wijken_subset_raw)

merged_2016 <- st_as_sf(merged_2016)



merged_2016 <- merged_data %>%
  filter(Perioden == 2016)


merged_wijken <- wijken_sf_raw %>%
  rename("Regioaanduiding.Codering..code." = "statcode")

merged_2016 <- merged_2016 %>% full_join(merged_wijken, by = "Regioaanduiding.Codering..code.")


#filter alleen wijken

wijken_data <- merged_2016 %>%
  filter(Regioaanduiding.Soort.regio..omschrijving..x == "Wijk")





