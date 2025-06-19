library(dplyr)

# Kolommen selecteren voor de score
cols_to_convert <- c(
  "Roker....",
  "Alcoholgebruik.Zware.drinker....",
  "Alcoholgebruik.Overmatige.drinker....",
  "Lichamelijke.gezondheid.Eén.of.meer.langdurige.aandoeningen....",
  "Hoog.risico.op.angst.of.depressie....",
  "Eenzaamheid.Ernstig.zeer.ernstig.eenzaam...."
)

# Zet komma's naar punten en maak getallen van tekens
merged_data[cols_to_convert] <- lapply(merged_data[cols_to_convert], function(x) {
  x <- gsub(",", ".", x)
  as.numeric(x)
})

# Bereken de ongezondheid_score als gemiddelde van de gekozen kolommen
merged_data <- merged_data %>%
  mutate(
    ongezondheid_score = rowMeans(select(., all_of(cols_to_convert)), na.rm = TRUE)
  )

library(dplyr)
library(ggplot2)

# Gemiddelde ongezondheid_score per jaar berekenen
library(dplyr)
library(ggplot2)

# Calculate average unhealthiness score per year and plot
merged_data%>%
  group_by(Perioden) %>%
  summarise(avg_unhealthiness = mean(ongezondheid_score, na.rm = TRUE)) %>%
  ggplot(aes(x = Perioden, y = avg_unhealthiness)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = c(20:35)*0.5, limits =c(12,17))+
  labs(
    title = "Average Unhealthiness Score per Year",
    x = "Year",
    y = "Unhealthiness Score"
  ) +
  theme_minimal()
