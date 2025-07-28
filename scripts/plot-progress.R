library(tidyverse)
library(httr2)
library(googlesheets4)
library(glue)

## Save all the data to a dated file for later comparison

req_base <- request("https://app.dataquest.io/api/team-reports/") |>
  req_url_path_append(Sys.getenv("DATAQUEST_TEAM_ID")) |>
  req_headers("API-Key" = Sys.getenv("DATAQUEST_API_KEY"))

date <- Sys.Date()

fs::dir_create("progress-data")

today_file <- fs::path(
  "progress-data",
  glue::glue("{date}_dataquest-team-data.json")
)

resp_all <- req_base |>
  req_url_path_append("all") |>
  req_perform(
    path = today_file,
  )

## Now combine them to chart progress over time

gs4_auth("andy@openscapes.org")

da_curriculum <- read_sheet(
  "1XIVVFpeFl-c72npg0vhkC9kDWIDBbmmDnvsWQ7xaugs",
  sheet = "22 hr version",
  range = "A1:H25"
) |>
  janitor::clean_names() |>
  mutate(
    course = gsub(" \\(\\d.+\\)", "", course),
    monday_date = as.Date(monday_date),
  )

files <- fs::dir_ls("progress-data", regexp = "\\.json$")


all_data <- lapply(
  files,
  \(x) {
    date <- stringr::str_extract(x, "\\d{4}-\\d{2}-\\d{2}")
    all <- jsonlite::read_json(x, simplifyVector = TRUE)
    mp <- all$mission_progress
    mp$query_date <- as.Date(date)
    mp
  }
) |>
  bind_rows()

time_spent <- jsonlite::read_json(
  today_file,
  simplifyVector = TRUE
)$time_spent |>
  filter(
    # has_license,
    # is_active_member,
    `all time: hours spent` >= 1,
    !email %in%
      c(
        "cassienickles95@gmail.com",
        "andy@openscapes.org",
        "ileana@openscapes.org"
      )
  )

active_users <- time_spent |>
  distinct(email)

n_participants <- nrow(active_users)

mission_prog_distinct <- all_data |>
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
  arrange(da_lesson_id) |>
  mutate(
    course = factor(course, levels = unique(da_curriculum$course))
  )

# Create a visualization showing progress by week
library(ggplot2)
library(scales)


# Plot 1: Average progress by week
progress_summary |>
  filter(query_date == max(query_date, na.rm = TRUE)) |>
  ggplot(aes(
    x = factor(da_lesson_id),
    y = percent_completed,
    colour = course
  )) +
  geom_point(
    alpha = 0.7,
    size = 3
  ) +
  facet_wrap(vars(monday_date), nrow = 1, scales = "free_x") +
  labs(
    title = "Lesson Completion Rates by Week and Course",
    subtitle = glue::glue("Total number of participants: {n_participants}"),
    x = "Lesson",
    y = "Percent of Participants Completed",
    colour = "Course"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    sec.axis = sec_axis(
      ~ . * n_participants / 100,
      name = "Number of Participants",
      breaks = scales::pretty_breaks()
    )
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgrey", color = "lightgrey")
  )

## Plot 2: Average progress by week and query date. This shows how people have
## "caught up" week by week

# progress_summary |>
#   ggplot(aes(
#     x = factor(da_lesson_id),
#     y = percent_started,
#     colour = course,
#     shape = format(query_date, "%Y-%m-%d")
#   )) +
#   geom_point(
#     alpha = 0.7,
#     size = 5
#   ) +
#   labs(
#     title = "Lesson Completion Rates by Week and Course",
#     subtitle = glue::glue("Total number of participants: {n_participants}"),
#     x = "Lesson Week",
#     y = "Percent of Participants Completed",
#     colour = "Course",
#     shape = "Before/After Break Week"
#   ) +
#   scale_y_continuous(
#     labels = scales::percent_format(scale = 1),
#     limits = c(0, 100)
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     plot.subtitle = element_text(size = 12, color = "gray60")
#   ) +
#   facet_wrap(vars(monday_date), nrow = 1, scales = "free_x")

time_spent_summary <- time_spent |>
  group_by(email) |>
  summarise(
    across(ends_with("hours spent"), \(x) sum(x, na.rm = TRUE)),
  ) |>
  summarise(
    total_hours = sum(`all time: hours spent`, na.rm = TRUE),
    mean_hours = mean(`all time: hours spent`, na.rm = TRUE),
    max_hours = max(`all time: hours spent`, na.rm = TRUE),
    median_hours = median(`all time: hours spent`, na.rm = TRUE),
    total_users = n(),
    .groups = "drop"
  )

time_spent |>
  ggplot(aes(
    x = `all time: hours spent`
  )) +
  geom_histogram(
    bins = 20,
    position = "identity",
    fill = "#ccc",
    colour = "darkgrey"
  ) +
  labs(
    title = "Distribution of Time Spent by Participants",
    x = "Hours Spent on Dataquest Learning Platform",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 14)


all_current <- jsonlite::read_json(
  today_file,
  simplifyVector = TRUE
)

certificates_given <- all_current$course_progress |>
  filter(
    progress_pct == 100,
    !email %in%
      c(
        "cassienickles95@gmail.com",
        "andy@openscapes.org",
        "ileana@openscapes.org"
      )
  ) |>
  select(-path) |>
  left_join(
    da_curriculum |>
      select(course, path) |>
      distinct()
  ) |>
  group_by(path, course) |>
  summarize(n_completed = n_distinct(email)) |>
  filter(!is.na(path)) |>
  arrange(desc(n_completed))

knitr::kable(certificates_given) # copy and paste from Markdown into Google Doc
