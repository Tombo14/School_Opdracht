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


##### Poging 2 -----
library(cbsodataR)
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)

#— 1  most recent year for each level -----------------------------------------
maps <- cbs_get_maps()
prov_year      <- max(maps$year[maps$region == "provincie"])
gemeente_year  <- max(maps$year[maps$region == "gemeente"])
wijk_year      <- max(maps$year[maps$region == "wijk"])   # use the same for wijken & buurten

#— 2  read the three layers ----------------------------------------------------
zh_sf       <- cbs_get_sf("provincie", prov_year)   |> 
  filter(statnaam == "Zuid-Holland")

gemeente_sf <- cbs_get_sf("gemeente", gemeente_year)
wijken_sf   <- cbs_get_sf("wijk",     wijk_year)

#— 3  municipality codes we want (Rotterdam & Krimpenerwaard) ------------------
gem_codes <- gemeente_sf |> 
  filter(statnaam %in% c("Rotterdam", "Krimpenerwaard")) |> 
  mutate(code4 = str_extract(statcode, "\\d{4}")) |>         # keep only the digits
  pull(code4)

#— 4  keep only neighbourhoods that belong to those municipalities -------------
wijken_subset <- wijken_sf |> 
  mutate(gem_code = str_extract(statcode, "\\d{4}")) |> 
  filter(gem_code %in% gem_codes)

#— 5  plot ---------------------------------------------------------------------
ggplot() +
  geom_sf(data = zh_sf,        fill = "#e5f5f9", colour = "grey50") +
  geom_sf(data = wijken_subset,
          aes(fill = statnaam), colour = "white", linewidth = .15) +
  theme_void() +
  labs(title = sprintf("Wijken in Rotterdam & Krimpenerwaard (%s)", wijk_year),
       fill  = "Wijk") +
  guides(fill = guide_legend(override.aes = list(colour = "black")))

##### Poging 3 ------
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
ggplot() +
  geom_sf(data = zh_sf,        fill = "#e5f5f9", colour = "grey50") +
  geom_sf(data = wijken_subset,
          aes(fill = statnaam), colour = "white", linewidth = .15) +
  theme_void() +
  labs(title = sprintf("Wijken in Rotterdam & Krimpenerwaard (%s)", wijk_year),
       fill  = "Wijk") +
  guides(fill = guide_legend(override.aes = list(colour = "black")))