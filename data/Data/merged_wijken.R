
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

