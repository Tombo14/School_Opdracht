data2020 <- read.csv("~/Downloads/Gezondheid_per_wijk_en_buurt__2022_18062025_140356.csv", sep=";")
data2022 <- read.csv("~/Downloads/Gezondheid_per_wijk_en_buurt__2022_18062025_140404.csv", sep=";")
data2016 <- read.csv("~/Downloads/Gezondheid_per_wijk_en_buurt__2022_18062025_140754.csv", sep=";")

AlcoholGebrBuurt <- rbind(data2016, data2020, data2022)

write_csv(AlcoholGebrBuurt, "~/Documents/Data buurten Rotterdam/AlcoholBuurten.csv")