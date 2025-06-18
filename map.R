library(cbsodataR)
library(ggplot2)
library(dplyr)
library(sf)

#kaart provincie zuid-holland

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

gemeente_sf <- cbs_get_sf("gemeente", wijk_map_yr)
rotterdam_code <- gemeente_sf %>% filter(statnaam == "Rotterdam") %>% pull(statcode)
krimpener_code <- gemeente_sf %>% filter(statnaam == "Krimpenerwaard") %>% pull(statcode)

wijken_subset <- wijken_sf %>%
  filter(substr(statcode, 1, 4) %in% c(rotterdam_code, krimpener_code))

ggplot() +
  geom_sf(data = zh_sf, fill = "#e5f5f9", color = "grey40") +
  geom_sf(data = wijken_subset, aes(fill = statnaam), color = "black", size = 0.2) +
  theme_void() +
  labs(title = "Wijken in Rotterdam & Krimpenerwaard (op kaart Zuid-Holland)",
       fill = "Gemeente")






