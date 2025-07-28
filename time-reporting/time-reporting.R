library(tidyverse)

a_time <- read_csv("time-reporting/academy_time.csv") |>
  group_by(category) |>
  summarise(total_time = sum(as.duration(Duration)) / 3600, .groups = "drop")
