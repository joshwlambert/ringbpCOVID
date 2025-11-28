library(remotes)
library(data.table)
library(future)
library(future.apply)

# not exact, works for differentiating LSHTM HPC from running locally
on_hpc <- nchar(Sys.getenv("SLURM_CLUSTER_NAME")) > 0

cat("Running on HPC: ", on_hpc, "\n")

cat("Running interactively: ", interactive(), "\n")

cat("Installing {ringbp}... \n")

install_github("epiforecasts/ringbp@238b4203ff22acd618f5e4588fe7c425eff01083")

library(ringbp)

cat("Using {ringbp} version", as.character(packageVersion("ringbp")), "\n")

scenarios <- data.table(
  expand.grid(
    delay_group = list(data.table(
      delay = c("SARS", "Wuhan"),
      delay_shape = c(1.651524, 2.305172),
      delay_scale = c(4.287786, 9.483875)
    )),
    k_group = list(data.table(
      theta = c("<1%", "15%", "30%"),
      k = c(30, 1.95, 0.7)
    )),
    index_R0 = c(1.5, 2.5, 3.5),
    r0isolated = 0,
    disp.com = 0.16,
    disp.iso = 1,
    prop.asym = c(0, 0.1),
    control_effectiveness = seq(0, 1, 0.2),
    num.initial.cases = c(5, 20, 40),
    quarantine = FALSE,
    cap_max_days = 365,
    cap_cases = 5000
  )
)

list_cols <- grep("_group", colnames(scenarios), value = TRUE)
non_list_cols <- setdiff(colnames(scenarios), list_cols)

expanded_groups <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
expanded_incub <- scenarios[, rbindlist(k_group), by = c(non_list_cols)]

scenarios <- merge(
  expanded_groups, expanded_incub, by = non_list_cols, allow.cartesian = TRUE
)

scenarios[, scenario :=  1:.N]

scenario_sims <- scenarios[, list(data = list(.SD)), by = scenario]

n <- 10

# Set up multicore if using see ?future::plan for details
# Use the workers argument to control the number of cores used.
if (!interactive() && on_hpc) {
  future::plan("multisession", workers = future::availableCores())
} else {
  future::plan("multisession", workers = max(1, future::availableCores() - 2))
}

# Run parameter sweep
scenario_sims[, sims := future_lapply(data, \(x, n) {
  scenario_sim(
    n.sim = n,
    num.initial.cases = x$num.initial.cases,
    r0community = x$index_R0,
    r0isolated = x$r0isolated,
    disp.com = x$disp.com,
    disp.iso = x$disp.iso,
    delay_shape = x$delay_shape,
    delay_scale = x$delay_scale,
    prop.asym = x$prop.asym,
    k = x$k,
    prop.ascertain = x$control_effectiveness,
    quarantine = x$quarantine,
    cap_max_days = x$cap_max_days,
    cap_cases = x$cap_cases
  )
},
n = n,
future.seed = TRUE
)]

cat("Finished simulation... \n")

cat("Saving simulation results... \n")

saveRDS(
  object = scenario_sims,
  file = file.path(
    "inst", "extdata", "covid_sim_pandemic_ver.rds"
  )
)

cat("Finished \n")
