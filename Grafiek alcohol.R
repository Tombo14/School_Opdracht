library(dplyr)
library(ggplot2)

wijken_data <- merged_data %>%
  filter(Regioaanduiding.Soort.regio..omschrijving..x == "Wijk")
wijken_data <- wijken_data %>%
  filter(Alcoholgebruik.Overmatige.drinker.... != ".")


wijken_data$Alcoholgebruik.Overmatige.drinker.... <-
  as.numeric(gsub(",", ".", wijken_data$Alcoholgebruik.Overmatige.drinker....))

ggplot(wijken_data, aes(
  x = Perioden,
  y = Alcoholgebruik.Overmatige.drinker....,
  group = Wijken.en.buurten.x,
  color = Wijken.en.buurten.x
)) +
  geom_line(size = 1.5)+
  geom_point(size = 1.5) +
  scale_y_continuous(breaks=c(0:100)*0.5,limits = c(4,9))
labs(
  title = "Overmatige drinkers per wijk over tijd (tekstwaarden)",
  x = "Jaar",
  y = "% Overmatige drinkers (tekstformaat)",
  color = "Wijk"
) +
  theme_minimal()

library(ggplot2)

ggplot(wijken_data, aes(
  x = Perioden,
  y = Alcoholgebruik.Overmatige.drinker....,
  group = Wijken.en.buurten.x,
  color = Wijken.en.buurten.x
)) +
  geom_line() +
  geom_vline(xintercept = 2020) +
  annotate("text", x =2018.5, y=8.5,size = 5, label="COVID-19\nCrisis") +
  geom_point(size = 1.5) +
  scale_y_continuous(breaks = seq(4, 9, 0.5), limits = c(4, 9)) +
  labs(
    title = "Excessive alcohol consumption per neighborhood over time",
    x = "Year",
    y = "% Excessive drinkers",
    color = "Neighborhood"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12)
  )
