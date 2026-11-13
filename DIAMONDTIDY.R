data(diamonds)

library(tidyverse)

newDiamonds <- diamonds |>
  filter(x != 0, y != 0, z != 0) |>
  select(cut, x, y, z) |>
  pivot_longer(
    cols = c(x,y,z),
    names_to = "Variable",
    values_to = "Value"
  ) |>
  group_by(cut, Variable) |>
  summarise(
    count = n(),
    Min = min(Value),
    Q1 = quantile(Value, 0.25),
    Median = median(Value),
    SAM = mean(Value),
    Q3 = quantile(Value, 0.75),
    Max = max(Value),
    MAD = mad(Value),
    SASD = sd(Value)
  ) |>
  rename(
    Cut = cut
  )

view(newDiamonds)