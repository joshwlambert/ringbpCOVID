library(pak)
library(data.table)
library(future)
library(future.apply)

pkg_install("joshwlambert/ringbp@FluTracerPilot")

library(ringbp)

cat("Using {ringbp} version", as.character(packageVersion("ringbp")))

scenarios <- data.table(
  expand.grid(
    delay_group = list(data.table(
      delay = c("SARS", "Wuhan"),
      onset_to_isolation = c(
        \(n) stats::rweibull(n = n, shape = 1.651524, scale = 4.287786),
        \(n) stats::rweibull(n = n, shape = 2.305172, scale = 9.483875)
      )
    )),
    incubation_period_group = list(data.table(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.322737, scale = 6.492272)
    )),
    r0_community = c(1.5, 2.5, 3.5),
    r0_isolated = 0,
    disp_community = 0.16,
    disp_isolated = 1,
    # prop_presymptomatic that corresponds to k = c("<1%" = 30, "15%" = 1.95, "30%" = 0.7)
    prop_presymptomatic = c(0.0106064, 0.15083, 0.305603),
    prop_asymptomatic = c(0, 0.1),
    prop_ascertain = seq(0, 1, 0.2),
    initial_cases = c(5, 20, 40),
    quarantine = FALSE,
    cap_max_days = 365,
    cap_cases = 5000
  )
)

list_cols <- grep("_group", colnames(scenarios), value = TRUE)
non_list_cols <- setdiff(colnames(scenarios), list_cols)

expanded_groups <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
expanded_incub <- scenarios[, rbindlist(incubation_period_group), by = c(non_list_cols)]

scenarios <- merge(
  expanded_groups, expanded_incub, by = non_list_cols, allow.cartesian = TRUE
)

scenarios[, scenario :=  1:.N]

scenario_sims <- scenarios[, list(data = list(.SD)), by = scenario]

n <- 10

# Set up multicore if using see ?future::plan for details
# Use the workers argument to control the number of cores used.
if (!interactive() && on_hpc) {
  future::plan("multicore", workers = 16)
} else {
  future::plan("multisession", workers = 4)
}

# Run parameter sweep
scenario_sims[, sims := future_lapply(data, \(x, n) {
  scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = x$onset_to_isolation[[1]],
      latent_period = 1
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )
},
n = n,
future.seed = TRUE
)]

saveRDS(
  object = scenario_sims,
  file = file.path(
    "inst", "extdata", paste0("covid_sim_current_ver.rds")
  )
)

cat("Finished \n")
