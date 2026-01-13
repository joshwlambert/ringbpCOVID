# Script to calculate the error between ringbp versions -------------------

library(data.table)
library(ringbp)

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

# get number of simulation replicates from simulation object
# all simulations have the same number of replicates so get from first element
nsim <- max(pandemic$sims[[1]]$sim)

covid_wide <- dcast(
  covid_data,
  r0_community + prop_asymptomatic + prop_ascertain + initial_cases +
    delay + prop_presymptomatic ~ version,
  value.var = "pext"
)

# calculate difference between current and pandemic probability of extinction
covid_wide[, delta_pext := current - pandemic]

hist(covid_wide$delta_pext)
summary(covid_wide$delta_pext)

# continuity correction for 0 pext
covid_wide[, current := (current * nsim + 0.5) / (nsim + 1)]
covid_wide[, pandemic := (pandemic * nsim + 0.5) / (nsim + 1)]

# calculate the relative difference between current and pandemic probability
# of extinction
covid_wide[, rel_delta_pext := ((current - pandemic) / pandemic) * 100]

hist(covid_wide$rel_delta_pext)
summary(covid_wide$rel_delta_pext)

# calculate odds ratio
outbreak_control_odds_ratio <- covid_wide$pandemic / covid_wide$current

hist(outbreak_control_odds_ratio, breaks = 30)
hist(outbreak_control_odds_ratio, breaks = 100, xlim = c(0, 5))
summary(outbreak_control_odds_ratio)
