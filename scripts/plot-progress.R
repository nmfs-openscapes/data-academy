library(tidyverse)
library(httr2)
library(googlesheets4)
library(glue)

gs4_auth("andy@openscapes.org")

da_curriculum <- read_sheet(
  "1XIVVFpeFl-c72npg0vhkC9kDWIDBbmmDnvsWQ7xaugs",
  sheet = "22 hr version",
  range = "A1:H24"
) |>
  janitor::clean_names() |>
  mutate(
    course = gsub(" \\(\\d.+\\)", "", course),
    monday_date = as.Date(monday_date),
  )

## Save all the data to a dated file for later comparison

req_base <- request("https://app.dataquest.io/api/team-reports/") |>
  req_url_path_append(Sys.getenv("DATAQUEST_TEAM_ID")) |>
  req_headers("API-Key" = Sys.getenv("DATAQUEST_API_KEY"))

date <- Sys.Date()

fs::dir_create("progress-data")
resp_all <- req_base |>
  req_url_path_append("all") |>
  req_perform(
    path = glue::glue("progress-data/{date}_dataquest-team-data.json")
  )

all_data <- jsonlite::read_json(
  glue::glue("progress-data/{date}_dataquest-team-data.json"),
  simplifyVector = TRUE
)

mission_progress <- all_data$mission_progress
mission_progress$query_date <- date

all_data_start_break_week <- all_data <- jsonlite::read_json(
  glue::glue("progress-data/2025-06-16_dataquest-team-data.json"),
  simplifyVector = TRUE
)

mission_progress_start_break_week <- all_data_start_break_week$mission_progress
mission_progress_start_break_week$query_date <- as.Date("2025-06-16")

time_spent <- all_data$time_spent

active_users <- time_spent |>
  filter(
    has_license,
    is_active_member,
    `all time: hours spent` >= 0,
    !email %in%
      c(
        "cassienickles95@gmail.com",
        "andy@openscapes.org",
        "ileana@openscapes.org"
      )
  )

n_participants <- nrow(active_users)

mission_prog_distinct <- mission_progress |>
  bind_rows(mission_progress_start_break_week) |>
  select(-path, -path_id, -step) |>
  semi_join(active_users, by = "email") |>
  filter(
    language == "R",
    !course_id %in% c("93", "N/A")
  ) |>
  group_by(
    learner_name,
    email,
    course_id,
    course,
    mission,
    query_date
  ) |>
  summarise(
    across(
      c(last_login, dttm_started_mission, progress_pct, dttm_completed),
      \(x) max(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

setdiff(da_curriculum$lesson, mission_prog_distinct$mission)
setdiff(mission_prog_distinct$mission, da_curriculum$lesson)

da_curriculum_progress <- da_curriculum |>
  select(-course) |>
  left_join(
    mission_prog_distinct,
    by = c("lesson" = "mission")
  )

progress_summary <- da_curriculum_progress |>
  group_by(
    da_lesson_id,
    week,
    monday_date,
    path,
    course,
    lesson,
    lesson_time_hours,
    query_date
  ) |>
  summarise(
    participants_started = sum(!is.na(progress_pct)),
    participants_completed = sum(progress_pct == 100, na.rm = TRUE),
    percent_started = participants_started / n_participants * 100,
    percent_completed = participants_completed / n_participants * 100,
    mean_progress = mean(progress_pct, na.rm = TRUE),
    median_progress = median(progress_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(da_lesson_id)

# Create a visualization showing progress by week
library(ggplot2)
library(scales)


# Plot 1: Average progress by week
progress_summary |>
  ggplot(aes(
    x = factor(da_lesson_id),
    y = percent_started,
    colour = course,
    shape = format(query_date, "%Y-%m-%d")
  )) +
  geom_point(
    alpha = 0.7,
    size = 5
  ) +
  labs(
    title = "Average Progress by Lesson",
    subtitle = glue::glue("Total number of participants: {n_participants}"),
    x = "Lesson",
    y = "Percent of Participants Completed",
    colour = "Course",
    shape = "Before/After Break Week"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  ) +
  facet_wrap(vars(monday_date), nrow = 1, scales = "free_x")
