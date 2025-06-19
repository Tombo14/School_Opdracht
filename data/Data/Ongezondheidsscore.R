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