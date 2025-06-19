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
wijken_delta <- wijken_delta %>% select(-c(daling_2016_2020,daling_2020_2022, ))

