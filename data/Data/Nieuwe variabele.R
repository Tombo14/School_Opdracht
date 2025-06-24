library(dplyr)
library(tidyr)


wijken_delta <- wijken_delta %>%
  mutate(
    verandering_2016_2020 = case_when(
      `2020` > `2016` ~ "Toegenomen",
      `2020` < `2016` ~ "Afgenomen",
      `2020` == `2016` ~ "Gelijk gebleven",
      TRUE ~ NA_character_
    ),
    verandering_2020_2022 = case_when(
      `2022` > `2020` ~ "Toegenomen",
      `2022` < `2020` ~ "Afgenomen",
      `2022` == `2020` ~ "Gelijk gebleven",
      TRUE ~ NA_character_
    ),
    verandering_overmatig = paste(verandering_2016_2020, "→", verandering_2020_2022)
  )


wijken_delta <- wijken_delta %>%
  select(
    Regioaanduiding.Codering..code.,
    `2016`,
    `2020`,       # <-- zet 2020 hier tussen
    `2022`,
    verandering_2016_2020,
    verandering_2020_2022,
    verandering_overmatig
  )


library(ggplot2)
library(tidyr)
library(dplyr)

# Zet data van breed naar lang formaat
wijken_long <- wijken_delta %>%
  pivot_longer(cols = c("2016", "2020", "2022"),
               names_to = "Jaar",
               values_to = "Alcoholgebruik") %>%
  mutate(Jaar = as.numeric(Jaar))

# Plot: lijnen per wijk
ggplot(wijken_long, aes(x = Jaar,
                        y = Alcoholgebruik,
                        group = Regioaanduiding.Codering..code.)) +
  geom_line(alpha = 0.5, color = "#2b8cbe") +
  geom_point(size = 1, color = "#2b8cbe") +
  labs(
    title = "Verandering in overmatige alcoholconsumptie per wijk",
    x = "Jaar",
    y = "% Overmatige drinkers"
  ) +
  theme_minimal(base_size = 13)
