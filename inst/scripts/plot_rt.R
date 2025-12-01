library(data.table)
library(ringbp)
library(ggplot2)

# read in results from run_sim_pandemic.R and run_sim_current.R
pandemic <- readRDS(file.path("inst", "extdata", "covid_sim_pandemic_ver.rds"))
current <- readRDS(file.path("inst", "extdata", "covid_sim_current_ver.rds"))

pandemic_data <- rbindlist(pandemic$data)
current_data <- rbindlist(current$data)

# harmonise pandemic and current scenarios

# remove columns that differ between pandemic and current versions and are not
# required for plotting
current_data[, c("r0_isolated", "disp_community", "disp_isolated", "quarantine",
                 "cap_max_days", "cap_cases", "onset_to_isolation",
                 "incubation_period") := NULL]

pandemic_data[, c("r0isolated", "disp.com", "disp.iso", "quarantine",
                  "cap_max_days", "cap_cases", "delay_shape",
                  "delay_scale", "k") := NULL]

# column names
setnames(
  pandemic_data,
  old = c(
    "index_R0", "prop.asym", "control_effectiveness", "num.initial.cases",
    "delay", "theta"
  ),
  new = c(
    "r0_community", "prop_asymptomatic", "prop_ascertain", "initial_cases",
    "delay", "prop_presymptomatic"
  )
)

# convert proportions of presymptomatic transmission into labels
idx <- c(
  "0.0106064" = "<1%",
  "0.15083" = "15%",
  "0.305603" = "30%"
)

# change proportion presymptomatic into percentage to match pandemic version
current_data[, prop_presymptomatic := idx[as.character(current_data$prop_presymptomatic)]]

# label each scenario and bind them
pandemic_data[, version := "pandemic"]
current_data[, version := "current"]

pandemic_rt <- lapply(pandemic$sims, \(x) x[week == max(week), effective_r0])
pandemic_median_rt <- vapply(pandemic_rt, median, FUN.VALUE = numeric(1))

current_rt <- lapply(current$sims, \(x) x[week == max(week), effective_r0])
current_median_rt <- vapply(current_rt, median, FUN.VALUE = numeric(1))

# dplyr::summarise(median_eff_r0 = median(effective_r0,
#                                         na.rm = TRUE),
#                  lower = quantile(effective_r0, 0.025,
#                                   na.rm = TRUE),
#                  iqr_lower = quantile(effective_r0,
#                                       0.25,
#                                       na.rm = TRUE),
#                  iqr_upper = quantile(effective_r0,
#                                       0.75,
#                                       na.rm = TRUE),
#                  upper = quantile(effective_r0,
#                                   0.975,
#                                   na.rm = TRUE))

pandemic_data[, median_rt := pandemic_median_rt]
current_data[, median_rt := current_median_rt]

covid_data <- rbindlist(list(pandemic_data, current_data), use.names = TRUE)

rt <- covid_data[
  initial_cases == 20 & prop_presymptomatic == "15%" & delay == "SARS" & prop_asymptomatic == 0,
  .(prop_ascertain, r0_community, median_rt, version)
]

rt_plot <- ggplot2::ggplot(
  data = rt,
  mapping = ggplot2::aes(
    x = prop_ascertain,
    y = median_rt,
    colour = as.factor(r0_community),
    fill = as.factor(r0_community)
  )
) +
  ggplot2::geom_line(
    ggplot2::aes(linetype = version),
    size = 0.75
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = version),
    colour = "black",
    size = 3,
    stroke = 1
  ) +
  ggplot2::geom_hline(yintercept = 1, lty = 2) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    breaks = seq(0, 1, 0.2),
    labels = seq(0, 100, 20)
  ) +
  ggplot2::scale_y_continuous(
    name = "Effective reproduction number (Rt)",
    limits = c(0, 4)
  ) +
  ggplot2::scale_fill_brewer(
    palette = "Set1",
    guide = "none"
  ) +
  ggplot2::scale_colour_brewer(
    palette = "Set1",
    name = "Basic reproduction\nnumber (R0)"
  ) +
  ggplot2::scale_shape_manual(values = c(21, 23)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")


# ggplot2::geom_ribbon(ggplot2::aes(ymin = lower,
#                                   ymax = upper,
#                                   col = NULL),
#                      alpha = 0.1) +
#   ggplot2::geom_ribbon(ggplot2::aes(ymin = iqr_lower,
#                                     ymax = iqr_upper,
#                                     col = NULL),
#                        alpha = 0.3)


ggplot2::ggsave(
  file.path("inst", "plots", "rt.png"),
  plot = rt_plot,
  device = "png",
  width = 150,
  height = 125,
  units = "mm",
  dpi = 300
)
