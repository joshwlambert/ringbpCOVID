library(data.table)
library(ringbp)
library(ggplot2)
library(grDevices)
library(patchwork)

# read in results from run_sim_pandemic.R and run_sim_current.R
pandemic <- readRDS(file.path("inst", "extdata", "covid_sim_pandemic_ver.rds"))
current <- readRDS(file.path("inst", "extdata", "covid_sim_current_ver.rds"))

# extinct_prob() in {ringbp} v0.1.2.9000 requires a `cap_cases` attribute in the scenario
# assign `cap_cases` attribute for scenarios simulated with {ringbp} v0.1.0
invisible(lapply(pandemic$sims, setattr, name = "cap_cases", value = 5000))

pandemic[, pext := ringbp::extinct_prob(sims[[1]], extinction_week = 12:16), by = scenario]
current[, pext := ringbp::extinct_prob(sims[[1]], extinction_week = 12:16), by = scenario]

pandemic_data <- rbindlist(pandemic$data)
pandemic_data[, `:=`(scenario = pandemic$scenario, pext = pandemic$pext)]

current_data <- rbindlist(current$data)
current_data[, `:=`(scenario = current$scenario, pext = current$pext)]

# harmonise pandemic and current scenarios --------------------------------

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
    "delay", "theta", "scenario", "pext"
  ),
  new = c(
    "r0_community", "prop_asymptomatic", "prop_ascertain", "initial_cases",
    "delay", "prop_presymptomatic", "scenario", "pext"
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

covid_data <- rbindlist(list(pandemic_data, current_data), use.names = TRUE)

prop_outbreak_control <- covid_data[
  delay == "SARS" & prop_presymptomatic == "15%" & r0_community == 2.5 & prop_asymptomatic == 0,
  .(prop_ascertain, initial_cases, pext, version)
]

outbreak_control_initial_cases_plot <- plot_outbreak_control(
  data = prop_outbreak_control,
  var = "initial_cases"
) +
  ggplot2::scale_fill_manual(
    values = grDevices::hcl.colors(n = 3, palette = "Zissou 1"),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = grDevices::hcl.colors(n = 3, palette = "Zissou 1"),
    name = "Number of\ninitial cases"
  )

prop_outbreak_control <- covid_data[
  initial_cases == 20 & r0_community == 2.5 & prop_asymptomatic == 0 & prop_presymptomatic == "15%",
  .(prop_ascertain, delay, pext, version)
]

# change delay names from 'SARS' and 'Wuhan' to 'Short' and 'Long'
idx <- c(
  "SARS" = "Short",
  "Wuhan" = "Long"
)
prop_outbreak_control[, delay := idx[prop_outbreak_control$delay]]

outbreak_control_delay_plot <- plot_outbreak_control(
  data = prop_outbreak_control,
  var = "delay"
) +
  ggplot2::scale_fill_manual(
    values = grDevices::hcl.colors(n = 2, palette = "Geyser"),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = grDevices::hcl.colors(n = 2, palette = "Geyser"),
    name = "Onset-to-isolation delay"
  )

prop_outbreak_control <- covid_data[
  delay == "SARS" & initial_cases == 20 & r0_community == 2.5 & prop_asymptomatic == 0,
  .(prop_ascertain, prop_presymptomatic, pext, version)
]

outbreak_control_prop_presymptomatic_plot <- plot_outbreak_control(
  data = prop_outbreak_control,
  var = "prop_presymptomatic"
) +
  ggplot2::scale_fill_manual(
    values = grDevices::hcl.colors(n = 5, palette = "Batlow")[2:4],
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = grDevices::hcl.colors(n = 5, palette = "Batlow")[2:4],
    name = "Transmission\nbefore symptoms"
  )

prop_outbreak_control <- covid_data[
  delay == "SARS" & initial_cases == 20 & r0_community == 2.5 & prop_presymptomatic == "15%",
  .(prop_ascertain, prop_asymptomatic, pext, version)
]

# change prop_asymptomatic to percentages
idx <- c(
  "0" = "0%",
  "0.1" = "10%"
)
prop_outbreak_control[, prop_asymptomatic := idx[as.character(prop_outbreak_control$prop_asymptomatic)]]

outbreak_control_prop_asymptomatic_plot <- plot_outbreak_control(
  data = prop_outbreak_control,
  var = "prop_asymptomatic"
) +
  ggplot2::scale_fill_manual(
    values = grDevices::hcl.colors(n = 2, palette = "Berlin"),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = grDevices::hcl.colors(n = 2, palette = "Berlin"),
    name = "Subclinical infections"
  )

outbreak_control_plot <-
  (outbreak_control_initial_cases_plot + outbreak_control_delay_plot) /
  (outbreak_control_prop_presymptomatic_plot + outbreak_control_prop_asymptomatic_plot) +
  plot_annotation(tag_levels = "A")

ggplot2::ggsave(
  file.path("inst", "plots", "outbreak_control.png"),
  plot = outbreak_control_plot,
  device = "png",
  width = 275,
  height = 250,
  units = "mm",
  dpi = 300
)
