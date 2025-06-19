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
  #  geom_sf(data = zh_sf, fill = "#e5f5f9", colour = "grey50") +
  geom_sf(data = merged_2016,
          aes(fill = Distance_cafes), colour = "white", linewidth = .15) +
  theme_minimal() +
  #coord_sf(
    #xlim = c(65000, 135000),     # RD-coördinaten voor oost-west
    #ylim = c(420000, 455000),    # RD-coördinaten voor noord-zuid
    #expand = T
  #) +
  labs(title = sprintf("Neighbourhoods in Rotterdam & Krimpenerwaard (%s)", wijk_year),
       fill  = "Neighbourhood") +
  guides(fill = guide_legend(override.aes = list(colour = "black")))
