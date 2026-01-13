library(data.table)
library(ggplot2)
library(scales)
library(gh)

# Fetch the data
covid_cases <- read.csv(
  paste0(
    "https://ourworldindata.org/explorers/covid.csv?",
    "v=1&csvType=full&useColumnShortNames=true&Metric=Confirmed+",
    "cases&Interval=Cumulative&Relative+to+population=false"
  )
)
setDT(covid_cases)
covid_cases[, Code := NULL]

covid_cases[, Day := as.Date(Day, format = "%Y-%m-%d")]
covid_cases <- covid_cases[Day < as.Date("2020-06-01"), ]

# order by date just to be safe
setorder(covid_cases, Day)

# sum cases across countries per day, then cumulative sum
covid_cases <- covid_cases[, .(daily_total = sum(total_cases)), by = Day][
  , cumulative_cases := cumsum(daily_total)
][]

ggplot2::ggplot(data = covid_cases) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = Day, y = cumulative_cases)) +
  ggplot2::scale_y_continuous(
    name = "Cumulative Global COVID-19 cases",
    breaks = scales::breaks_log(n = 10),
    labels = scales::label_comma(),
    transform = "log"
  ) +
  ggplot2::scale_x_date(name = "Date") +
  ggplot2::theme_minimal()

ibrary(gh)

commits <- gh(
  "/repos/{owner}/{repo}/commits",
  owner = "epiforecasts",
  repo  = "ringbp",
  since = "2020-01-01T00:00:00Z",
  until = "2020-06-01T00:00:00Z",
  .limit = Inf
)

commit_times <- vapply(commits, \(x) x$commit$author$date, FUN.VALUE = character(1))
commit_dates <- as.Date(commit_times)
commits_per_day <- table(commit_dates$dates)

commits <- as.data.table(commits_per_day)

setnames(commits, old = c("V1", "N"), new = c("date", "n_commits"))

# order by date just to be safe
setorder(commits, "date")

commits[, date := as.Date(date)]

ggplot2::ggplot(data = commits) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = n_commits)) +
  ggplot2::scale_y_continuous(name = "Number of {ringbp} commits per day") +
  ggplot2::scale_x_date(name = "Date") +
  ggplot2::theme_minimal()

cases_commits <- commits[covid_cases, on = "date"]
cases_commits[is.na(n_commits), n_commits := 0]


scale_factor <- max(cases_commits$cumulative_cases) /
  max(cases_commits$n_commits)

# cases_commits[
#   , `:=`(
#     log_cumulative_cases = log(cumulative_cases),
#     commits_scaled = n_commits * scale_factor
#   )
# ]

cases_commit_dynamics_plot <- ggplot2::ggplot(data = cases_commits) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = cumulative_cases),
    colour = "steelblue",
    linewidth = 1
  ) +
  ggplot2::geom_col(
    mapping = ggplot2::aes(x = date, y = n_commits * scale_factor),
    fill = "darkseagreen"
  ) +
  ggplot2::scale_y_continuous(
    name = "Cumulative Global COVID-19 cases",
    breaks = scales::breaks_pretty(n = 10),
    labels = scales::label_comma(),
    sec.axis = ggplot2::sec_axis(
      transform = ~ . / scale_factor,
      name = "Number of {ringbp} commits per day",
      breaks = scales::breaks_pretty(n = 5)
    )
  ) +
  ggplot2::scale_x_date(name = "Date") +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  file.path("inst", "plots", "cases_commit_dynamics.png"),
  plot = cases_commit_dynamics_plot,
  device = "png",
  width = 150,
  height = 125,
  units = "mm",
  dpi = 300
)
