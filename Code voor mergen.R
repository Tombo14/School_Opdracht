library(dplyr)
library(readr)
nabijheid_voorzieningen2016 <- read_csv("~/Documents/School_Opdracht/data/Data/nabijheid_voorzieningen2016.csv")
nabijheid_voorzieningen2020 <- read_csv("~/Documents/School_Opdracht/data/Data/nabijheid_voorzieningen2020.csv")
nabijheid_voorzieningen2022 <- read_csv("~/Documents/School_Opdracht/data/Data/nabijheid_voorzieningen2022.csv")
AlcoholBuurten <- read_csv("data/AlcoholBuurten.csv")
nabijheid_voorzieningen2016$Perioden <- 2016
nabijheid_voorzieningen2020$Perioden <- 2020
nabijheid_voorzieningen2022$Perioden <- 2022

nabijheid_voorzieningen2016$...1 <- NULL


voorzien <- rbind(nabijheid_voorzieningen2016, nabijheid_voorzieningen2020,nabijheid_voorzieningen2022)
write_csv(voorzien,"data/Data/voorzieningen.csv")
merged_data <- voorzien %>%
  merge(AlcoholBuurten, by = c("Regioaanduiding.Codering..code.", "Perioden"))
write_csv(merged_data,"data/Data/merged_data.csv")
