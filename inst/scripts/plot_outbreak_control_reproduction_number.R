library(data.table)
library(ringbp)
library(ggplot2)

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
  initial_cases == 20 & prop_presymptomatic == "15%" & delay == "SARS" & prop_asymptomatic == 0,
  .(prop_ascertain, r0_community, pext, version)
]

outbreak_control_r_plot <- ggplot2::ggplot(
  data = prop_outbreak_control,
  mapping = ggplot2::aes(
    x = prop_ascertain,
    y = pext,
    colour = as.factor(r0_community)
  )
) +
  ggplot2::geom_line(
    ggplot2::aes(linetype = version),
    size = 0.75
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = as.factor(r0_community), shape = version),
    colour = "black",
    size = 3,
    stroke = 1
  ) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    breaks = seq(0, 1, 0.2),
    labels = seq(0, 100, 20)
  ) +
  ggplot2::scale_y_continuous(
    name = "Simulated outbreaks controlled (%)",
    breaks = seq(0, 1, 0.2),
    labels = seq(0, 100, 20)
  ) +
  ggplot2::scale_fill_brewer(
    palette = "Set1",
    guide = "none"
  ) +
  ggplot2::scale_colour_brewer(
    palette = "Set1",
    name = "Reproduction\nnumber"
  ) +
  ggplot2::scale_shape_manual(values = c(21, 23)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path("inst", "plots", "outbreak_control_r.png"),
  plot = outbreak_control_r_plot,
  device = "png",
  width = 150,
  height = 125,
  units = "mm",
  dpi = 300
)
