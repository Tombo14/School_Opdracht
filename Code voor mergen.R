library(dplyr)
library(readr)
merged_data <- nabijheid_voorzieningen2016 %>%
  full_join(nabijheid_voorzieningen2020, by = "Regioaanduiding.Codering..code.", suffix = c("_2016", "_2020")) %>%
  full_join(nabijheid_voorzieningen2022, by = "Regioaanduiding.Codering..code.")
nabijheid_voorzieningen2016$...1 <- NULL
voorzien <- rbind(nabijheid_voorzieningen2016, nabijheid_voorzieningen2020,nabijheid_voorzieningen2022)
write_csv(voorzien,"data/Data/voorzieningen.csv")
merged_data <- voorzien %>%
  merge(AlcoholBuurten, by = c("Regioaanduiding.Codering..code.", "Perioden"))
write_csv(merged_data,"data/Data/merged_data.csv")
