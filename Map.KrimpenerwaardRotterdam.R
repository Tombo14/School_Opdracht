setwd("~/Documents/GitHub/School_Opdracht")
merged_data <- read_csv("data/Data/merged_data.csv")
maps <- cbs_get_maps()
prov_map_yr <- max(maps$year[maps$region == "provincie"])

prov_sf <- cbs_get_sf(region = "provincie", year = prov_map_yr)

zh_sf <- prov_sf %>% filter(statnaam == "Zuid-Holland")

ggplot(zh_sf) +
  geom_sf(fill = "#2b8cbe", colour = "white") +
  theme_void() +
  labs(title = paste0("Provincie Zuid-Holland – CBS kaart ", prov_map_yr))

muni_sf <- cbs_get_sf("provincie", prov_map_yr)

#wijken en buurten er over heen
prov_map_yr <- max(cbs_get_maps()$year[cbs_get_maps()$region == "provincie"])
zh_sf <- cbs_get_sf("provincie", prov_map_yr) %>%
  filter(statnaam == "Zuid-Holland")

wijk_map_yr <- max(cbs_get_maps()$year[cbs_get_maps()$region == "wijk"])
wijken_sf <- cbs_get_sf("wijk", wijk_map_yr)


merged_data <- merged_data %>%
  mutate(Distance_cafes = if_else(
    Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km.> 1,
    "Cafes further than 1 km",
    "Cafes closer than 1 km"
  ))

#write.csv(merged_data, "data/Data/merged_data.csv")

#multipolygon koppelen aan merged_goeie (die wijkcodes)
wijken_sf<-sf::st_as_sf(wijken_sf)
merged_2016 <- merged_data %>%
  filter(Perioden == 2016)

merged_2016<- as.data.frame(merged_2016)

#merged_2016$geometry <- NULL

wijken_sf <- wijken_sf %>%
  rename("Regioaanduiding.Codering..code." = "statcode")

merged_2016 <- merged_2016 %>% merge(wijken_sf, 
                                     by = "Regioaanduiding.Codering..code.")



##### Poging 3 ------goeieee
library(cbsodataR)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)

# 1. Most-recent vintages
maps      <- cbs_get_maps()
prov_year <- max(maps$year[maps$region == "provincie"])
wijk_year <- max(maps$year[maps$region == "wijk"])

# 2. Read layers (will return data.frames if Tibble Hunter is on)
zh_sf_raw     <- cbs_get_sf("provincie", prov_year) |>
  filter(statnaam == "Zuid-Holland")

wijken_sf_raw <- cbs_get_sf("wijk", wijk_year)

# 3. Keep neighbourhoods in Rotterdam & Krimpenerwaard
gem_codes <- c("0599", "1931")                      # GM-codes → digits
wijken_subset_raw <- wijken_sf_raw |>
  mutate(gem_code = str_extract(statcode, "\\d{4}")) |>
  filter(gem_code %in% gem_codes)

# 4. Convert back to sf **here**
zh_sf        <- st_as_sf(zh_sf_raw)
wijken_subset <- st_as_sf(wijken_subset_raw)

# 5. Plot

merged_2016 <- st_as_sf(merged_2016)

ggplot() +
  geom_sf(data = merged_2016,
          aes(fill = Distance_cafes), colour = "white", linewidth = .15) +
  theme_minimal() +
  labs(title = sprintf("Neighbourhoods in Rotterdam & Krimpenerwaard (%s)", wijk_year),
       fill  = "Neighbourhood") +
  guides(fill = guide_legend(override.aes = list(colour = "black")))

