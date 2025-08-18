library(tidyverse)
library(googlesheets4)

gs4_auth("andy@openscapes.org")

cohort_1 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Kzl6akKd030oz-qPzBrikVbHyrjOLR7EgG5emSDkq9Q",
  sheet = "Signups",
  range = "A2:K162",
  .name_repair = janitor::make_clean_names
) |>
  filter(cohort_1_selected == "Yes")

cohort_2 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1fy5qDSscm6GU1OA3nAV_MX5YB8TJmehsUwYRewXass4",
  sheet = "Signups",
  range = "A2:I101",
  .name_repair = janitor::make_clean_names
) |>
  filter(confirm_fall_2025 == "Yes")

cohort_1_in_cohort_2 <- intersect(
  cohort_1$email_address,
  cohort_2$email_address
)

cohort_2_to_add_to_dataquest <- cohort_2 |>
  filter(!email_address %in% cohort_1$email_address) |>
  transmute(
    Name = paste(trimws(first_name), trimws(last_name)),
    Email = email_address,
    Admin = "",
    License = "X"
  )

# Print email addresses so we can add to Google Space
cat(cohort_2_to_add_to_dataquest$Email, sep = ", ")

to_remove_from_dataquest <- setdiff(
  cohort_1$email_address,
  cohort_2$email_address
) |>
  sort()

cat(to_remove_from_dataquest, sep = ",\n")
