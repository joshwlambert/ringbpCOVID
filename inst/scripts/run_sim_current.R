library(remotes)
library(data.table)
library(future)
library(future.apply)

# set data.table OpenMP to not paralellise
setDTthreads(1)

# not exact, works for differentiating LSHTM HPC from running locally
on_hpc <- nchar(Sys.getenv("SLURM_CLUSTER_NAME")) > 0

cat("Running on HPC: ", on_hpc, "\n")

cat("Running interactively: ", interactive(), "\n")

cat("Installing {ringbp}... \n")

# upgrade = "never" so dependencies are not upgraded
# (already installed by install_ringbpCOVID.sh)
install_github("epiforecasts/ringbp", upgrade = "never")

library(ringbp)

cat("Using {ringbp} version", as.character(packageVersion("ringbp")), "\n")

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
    # Hellewell et al. (2020) ran every scenario with quarantine inactive. Both
    # settings are swept here so that the quarantine-active comparison uses an
    # identical grid; downstream scripts subset to the arm they need.
    quarantine = c(FALSE, TRUE),
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

# PILOT RUN: reduced for quick turnaround on the HPC.
# Restore to 5000 (the value used by Hellewell et al.) for the final results.
n <- 100

# future.seed derives its L'Ecuyer streams from the current RNG state, so a
# fixed seed here makes the whole sweep reproducible
set.seed(20200124)

# Set up multicore if using see ?future::plan for details
# Use the workers argument to control the number of cores used.
if (!interactive() && on_hpc) {
  future::plan("multisession", workers = future::availableCores())
} else {
  future::plan("multisession", workers = max(1, future::availableCores() - 2))
}

# Run parameter sweep
scenario_sims[, sims := future_lapply(data, \(x, n) {
  # multisession workers are fresh R sessions that do not inherit the parent's
  # thread setting, so cap threads inside the worker too
  data.table::setDTthreads(1)
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
      # renamed from `symptomatic_ascertained` in epiforecasts/ringbp#196
      symptomatic_traced = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
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
    "inst", "extdata", paste0("covid_sim_current_ver.rds")
  )
)

cat("Finished \n")
