library(tidyverse)

ggplot(data = merged_goeie, mapping = aes(x= Regio.s, y = Alcoholgebruik.Overmatige.drinker....)) +
  geom_boxplot() +
  labs(x = "Regio's", y = "Alcoholgebruik") + 
  
#boxplot
  
ggplot(data = merged_goeie, mapping = aes(x = Regio.s, y = Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km.)) +
  geom_boxplot() +
  labs(x = "Regio's", y = "Afstand tot horeca en cafés") +
  scale_y_continuous(limits = c(0.5, 1.5), breaks = seq(0.5, 1.5, by = 0.2))

#staafdiagram voor afstand tot horeca/cafés voor regio per jaar

ggplot(data = merged_goeie, aes(x = Regio.s, y = Horeca.Cafés.en.dergelijke.Afstand.tot.café.e.d...km., fill = factor(Perioden))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2020" = "darkred", "2022" = "cadetblue2")) +
  labs(x = "\nRegio's", y = "Afstand tot horeca en cafes\n", fill = "Perioden") +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +

#staafdiagram voor alcoholgebruik voor regio per jaar
ggplot(data = merged_goeie, aes(x = Regio.s, y = Alcoholgebruik.Overmatige.drinker...., fill = factor(Perioden))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2020" = "darkred", "2022" = "cadetblue2")) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  labs(x = "\nRegio's", y = "Alcoholgebruik\n", fill = "Perioden")





#install.packages("cbsodataR")
#install.packages("sf")
library(ggplot2)
library(cbsodataR)
maps<- cbs_get_maps()
library(sf)
prov_sf <- cbs_get_sf(region = "provincie", year = 2022)

ggplot2::ggplot(prov_sf) +
  geom_sf()

