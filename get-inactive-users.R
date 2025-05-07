library(tidyverse)
library(httr2)
library(googlesheets4)
library(glue)

req_base <- request("https://app.dataquest.io/api/team-reports/") |>
  req_url_path_append(Sys.getenv("DATAQUEST_TEAM_ID")) |>
  req_headers("API-Key" = Sys.getenv("DATAQUEST_API_KEY"))

## The query for all metrics takes a while to return everything, so probably
## better to get the individual endpoints as needed

# resp_all <- req_base |>
#   req_url_path_append("all") |>
#   req_perform()
# all_team_data <- resp_body_json(resp_all, simplifyVector = TRUE)

resp_time_spent <- req_base |>
  req_url_path_append("time_spent") |>
  req_perform()

time_spent <- resp_body_json(resp_time_spent, simplifyVector = TRUE)

## Find those who we should email, and assign half each to Andy and Ileana to
## email
to_email <- time_spent |>
  filter(
    `all time: hours spent` < 1,
    has_license
  ) |>
  mutate(emailer = rep(c("Ileana", "Andy"), n() / 2), .before = everything()) |>
  arrange(emailer)


## Upload to Google Sheet so we have a list and record of who we emailed
gs4_auth("andy@openscapes.org")

# The ID of the DataQuest planning/curriculum google sheet
ss <- "1XIVVFpeFl-c72npg0vhkC9kDWIDBbmmDnvsWQ7xaugs"

## This will create a new worksheet in the existing Google Sheet
write_sheet(
  to_email,
  ss = ss,
  sheet = glue("nudge-email-", as.character(Sys.Date()))
)
