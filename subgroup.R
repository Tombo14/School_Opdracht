library(tidyverse)


#omzetten naar numeric + calculating average obesity rate
mean(as.numeric(merged_2016$Onder..en.overgewicht.Mate.van.overgewicht.Ernstig.overgewicht....), na.rm = TRUE) #15.56333

#subgroup

merged_2016 = mutate(merged_2016, Obesity_rate = ifelse(Onder..en.overgewicht.Mate.van.overgewicht.Ernstig.overgewicht.... >=15.56333,
                                                   "Above_average", "Below_average"))

write.csv(merged_2016, "Data/data/merged_2016.csv")
merged_2016$Alcoholgebruik.Overmatige.drinker.... <- merged_2016$Alcoholgebruik.Overmatige.drinker.... %>% as.numeric()
ggplot(merged_2016, aes(x=Obesity_rate, y=Alcoholgebruik.Overmatige.drinker....))+
  geom_boxplot() +
  labs(x="Obesity rate", y="Percentage of excessive alcohol use", title = "Alcohol vs obesity rate 2016")
