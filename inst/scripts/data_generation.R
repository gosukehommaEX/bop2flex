# Data generation for the article on sample size deviations in the BOP2 and
# BOP2-FE designs.
#
# Run from the package root (open bop2flex.Rproj first):
#   devtools::load_all()
#   source(file.path("inst", "scripts", "data_generation.R"))
#
# Results are written to inst/scripts/output/data. The script
# tables_and_figures.R reads them, so the layout of the tables and figures can
# be changed without repeating these computations. The file numbers.csv lists,
# with a key and a description, every number that may be quoted in the
# article, for the check of the manuscript against the results.
#
# All operating characteristics are exact. Random elements are the schedules
# of actual sample sizes only. The running time is about one hour, most of it
# for the oracle designs (n_oracle below).

if (!exists("flex_design", mode = "function")) library(bop2flex)

out_dir <- file.path("inst", "scripts", "output", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
t_start <- Sys.time()
elapsed <- function() {
  sprintf("%.1f min", as.numeric(difftime(Sys.time(), t_start,
                                          units = "mins")))
}

# Settings
n_sim <- 2000
n_oracle <- 100
seed <- 20260929

scenarios <- data.frame(
  scenario = paste0("S", 1:6),
  p0 = c(0.2, 0.3, 0.4, 0.5, 0.1, 0.2),
  p1 = c(0.4, 0.5, 0.6, 0.7, 0.3, 0.35),
  alpha = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.05),
  looks = c(rep("10,15,20,25,30,35,40", 4), "10,15,20,25,30",
            "20,30,40,50,60"),
  stringsAsFactors = FALSE
)
designs <- data.frame(
  design = c("BOP2", "BOP2-FE-1", "BOP2-FE-2"),
  type = c("BOP2", "BOP2-FE", "BOP2-FE"),
  method = c("power", "power", "OF"),
  stringsAsFactors = FALSE
)
deviations <- data.frame(
  deviation = c("moderate", "large"),
  interim_shift = c(2, 3),
  final_shift = c(4, 6),
  stringsAsFactors = FALSE
)
final_range <- 6
example_id <- "S1_BOP2-FE-1_binding"
example_schedule <- c(12, 16, 19, 25, 31, 37, 39)

parse_looks <- function(x) as.integer(strsplit(x, ",")[[1]])

# Configurations: every design with binding futility, and BOP2-FE-1 with
# non-binding futility
config <- rbind(
  expand.grid(s = seq_len(nrow(scenarios)), d = seq_len(nrow(designs)),
              binding = TRUE),
  expand.grid(s = seq_len(nrow(scenarios)), d = 2L, binding = FALSE)
)
config <- config[order(config$s, !config$binding, config$d), ]
rownames(config) <- NULL
config$scenario <- scenarios$scenario[config$s]
config$design <- designs$design[config$d]
config$rule <- ifelse(config$binding, "binding", "non-binding")
config$id <- paste(config$scenario, config$design, config$rule, sep = "_")

# Numbers that may be quoted in the article
numbers <- list()
add_number <- function(key, value, description) {
  numbers[[length(numbers) + 1]] <<- data.frame(
    key = key, value = as.numeric(value), description = description,
    stringsAsFactors = FALSE)
}
add_number("settings.n_sim", n_sim, "Random schedules per scenario and size")
add_number("settings.n_oracle", n_oracle,
           "Schedules per configuration for the oracle designs")
for (v in seq_len(nrow(deviations))) {
  add_number(paste0("settings.", deviations$deviation[v], ".interim_shift"),
             deviations$interim_shift[v],
             "Maximum shift of each interim sample size")
  add_number(paste0("settings.", deviations$deviation[v], ".final_shift"),
             deviations$final_shift[v],
             "Maximum shift of the final sample size")
}
add_number("settings.final_range", final_range,
           "Largest shift of the final sample size in the systematic study")

# 1. Planned designs
message("Planned designs (", elapsed(), ")")
planned <- vector("list", nrow(config))
names(planned) <- config$id
for (i in seq_len(nrow(config))) {
  sc <- scenarios[config$s[i], ]
  dg <- designs[config$d[i], ]
  planned[[i]] <- flex_design(p0 = sc$p0, p1 = sc$p1,
                              n = parse_looks(sc$looks), alpha = sc$alpha,
                              type = dg$type, method = dg$method,
                              binding = config$binding[i])
}
planned_summary <- do.call(rbind, lapply(seq_len(nrow(config)), function(i) {
  d <- planned[[i]]
  data.frame(id = config$id[i], scenario = config$scenario[i],
             design = config$design[i], rule = config$rule[i],
             p0 = d$p0, p1 = d$p1, alpha = d$alpha,
             looks = paste(d$n, collapse = ","), N = d$N,
             lambda = d$lambda, gamma = d$gamma,
             eta = if (is.null(d$eta)) NA_real_ else d$eta,
             futility = paste(d$boundary$futility, collapse = ","),
             efficacy = paste(d$boundary$efficacy, collapse = ","),
             type1 = d$oc$type1, power = d$oc$power, en0 = d$oc$en0,
             en1 = d$oc$en1, early_stop0 = d$oc$early_stop0,
             stringsAsFactors = FALSE)
}))
for (i in seq_len(nrow(planned_summary))) {
  r <- planned_summary[i, ]
  for (v in c("lambda", "gamma", "eta", "type1", "power", "en0", "en1",
              "early_stop0")) {
    add_number(paste("planned", r$id, v, sep = "."), r[[v]],
               paste("Planned design:", v))
  }
}
saveRDS(list(scenarios = scenarios, designs = designs,
             deviations = deviations, config = config, planned = planned,
             planned_summary = planned_summary),
        file.path(out_dir, "planned_designs.rds"))

# 2. Worked example
message("Worked example (", elapsed(), ")")
ex_design <- planned[[example_id]]
ex_naive <- flex_naive(ex_design, example_schedule, final = TRUE)
ex_proposed <- flex_update(ex_design, example_schedule, final = TRUE)
ex_compare <- flex_compare(ex_design, list(example_schedule))
ex_oracle <- flex_design(p0 = ex_design$p0, p1 = ex_design$p1,
                         n = example_schedule, alpha = ex_design$alpha)
worked <- list(id = example_id, design = ex_design,
               schedule = example_schedule,
               spending = flex_spending(ex_design), naive = ex_naive,
               proposed = ex_proposed, compare = ex_compare,
               oracle = ex_oracle)
saveRDS(worked, file.path(out_dir, "worked_example.rds"))
for (k in seq_along(example_schedule)) {
  for (meth in c("naive", "proposed")) {
    b <- worked[[meth]]$bounds
    add_number(sprintf("example.%s.look%d.futility", meth, k),
               b$futility[k], "Worked example: futility boundary")
    add_number(sprintf("example.%s.look%d.efficacy", meth, k),
               b$efficacy[k], "Worked example: efficacy boundary")
    add_number(sprintf("example.%s.look%d.alpha_spent", meth, k),
               b$alpha_spent[k], "Worked example: cumulative type I error")
  }
  add_number(sprintf("example.proposed.look%d.alpha_target", k),
             ex_proposed$bounds$alpha_target[k],
             "Worked example: value of the implied spending function")
  add_number(sprintf("example.oracle.look%d.efficacy", k),
             ex_oracle$boundary$efficacy[k],
             "Worked example: efficacy boundary of the oracle design")
}
for (j in seq_len(nrow(worked$spending))) {
  add_number(sprintf("example.spending.look%d", worked$spending$look[j]),
             worked$spending$alpha_cum[j],
             "Worked example: implied spending at a planned look")
}
for (meth in c("naive", "spending")) {
  r <- ex_compare[ex_compare$method == meth, ]
  lab <- if (meth == "spending") "proposed" else meth
  for (v in c("type1", "power", "en0", "en1")) {
    add_number(paste("example", lab, v, sep = "."), r[[v]],
               paste("Worked example:", v))
  }
}
for (v in c("type1", "power", "en0", "en1")) {
  add_number(paste("example.oracle", v, sep = "."), ex_oracle$oc[[v]],
             paste("Worked example, oracle design:", v))
}

# 3. Random schedules
message("Random schedules (", elapsed(), ")")
schedules <- list()
random <- list()
for (s in seq_len(nrow(scenarios))) {
  ids <- which(config$s == s)
  for (v in seq_len(nrow(deviations))) {
    key <- paste(scenarios$scenario[s], deviations$deviation[v], sep = "_")
    schedules[[key]] <- flex_schedules(
      planned[[ids[1]]], n_sim = n_sim,
      interim_shift = deviations$interim_shift[v],
      final_shift = deviations$final_shift[v], seed = seed + 10 * s + v)
    for (i in ids) {
      res <- flex_compare(planned[[i]], schedules[[key]])
      res$id <- config$id[i]
      res$scenario <- config$scenario[i]
      res$design <- config$design[i]
      res$rule <- config$rule[i]
      res$deviation <- deviations$deviation[v]
      random[[length(random) + 1]] <- res
    }
    message("  ", key, " done (", elapsed(), ")")
  }
}
random <- do.call(rbind, random)
rownames(random) <- NULL
saveRDS(schedules, file.path(out_dir, "random_schedules.rds"))
saveRDS(random, file.path(out_dir, "random_results.rds"))

# 4. Oracle designs calibrated on the actual schedule, binding futility,
# moderate deviations
message("Oracle designs (", elapsed(), ")")
oracle <- list()
for (i in which(config$binding)) {
  sc <- scenarios[config$s[i], ]
  dg <- designs[config$d[i], ]
  sch <- schedules[[paste(sc$scenario, "moderate", sep = "_")]]
  sch <- sch[seq_len(min(n_oracle, length(sch)))]
  res <- lapply(seq_along(sch), function(j) {
    od <- tryCatch(
      flex_design(p0 = sc$p0, p1 = sc$p1, n = sch[[j]], alpha = sc$alpha,
                  type = dg$type, method = dg$method),
      error = function(e) NULL)
    data.frame(id = config$id[i], schedule = j,
               oracle_type1 = if (is.null(od)) NA_real_ else od$oc$type1,
               oracle_power = if (is.null(od)) NA_real_ else od$oc$power,
               oracle_en0 = if (is.null(od)) NA_real_ else od$oc$en0,
               stringsAsFactors = FALSE)
  })
  oracle[[length(oracle) + 1]] <- do.call(rbind, res)
  message("  ", config$id[i], " done (", elapsed(), ")")
}
oracle <- do.call(rbind, oracle)
rownames(oracle) <- NULL
saveRDS(oracle, file.path(out_dir, "oracle_results.rds"))

# 5. Deviation of the final sample size only
message("Final sample size deviations (", elapsed(), ")")
final_dev <- list()
for (i in seq_len(nrow(config))) {
  d <- planned[[i]]
  num_looks <- length(d$n)
  finals <- seq(max(d$n[num_looks - 1] + 1L, d$N - final_range),
                d$N + final_range)
  sch <- lapply(finals, function(f) c(d$n[-num_looks], f))
  res <- flex_compare(d, sch)
  res$final_n <- finals[res$schedule]
  res$id <- config$id[i]
  res$scenario <- config$scenario[i]
  res$design <- config$design[i]
  res$rule <- config$rule[i]
  final_dev[[i]] <- res
}
final_dev <- do.call(rbind, final_dev)
rownames(final_dev) <- NULL
saveRDS(final_dev, file.path(out_dir, "final_deviation.rds"))

# 6. Summaries
message("Summaries (", elapsed(), ")")
q90 <- function(x) as.numeric(stats::quantile(x, 0.9, na.rm = TRUE))
grp <- interaction(random$id, random$deviation, random$method, drop = TRUE,
                   sep = "|", lex.order = TRUE)
random_summary <- do.call(rbind, lapply(split(random, grp), function(x) {
  data.frame(id = x$id[1], scenario = x$scenario[1], design = x$design[1],
             rule = x$rule[1], deviation = x$deviation[1],
             method = x$method[1], n = nrow(x),
             type1_mean = mean(x$type1), type1_median = stats::median(x$type1),
             type1_min = min(x$type1), type1_max = max(x$type1),
             exceed_pct = 100 * mean(x$exceeds_alpha),
             power_mean = mean(x$power), en0_mean = mean(x$en0),
             en1_mean = mean(x$en1), stringsAsFactors = FALSE)
}))
rownames(random_summary) <- NULL

paired <- merge(random[random$deviation == "moderate" &
                         random$schedule <= n_oracle &
                         random$rule == "binding", ],
                oracle, by = c("id", "schedule"))
paired$power_diff <- paired$power - paired$oracle_power
grp_o <- interaction(paired$id, paired$method, drop = TRUE, sep = "|",
                     lex.order = TRUE)
oracle_summary <- do.call(rbind, lapply(split(paired, grp_o), function(x) {
  data.frame(id = x$id[1], scenario = x$scenario[1], design = x$design[1],
             method = x$method[1], n = sum(!is.na(x$power_diff)),
             n_failed = sum(is.na(x$oracle_power)),
             oracle_power_mean = mean(x$oracle_power, na.rm = TRUE),
             power_mean = mean(x$power[!is.na(x$oracle_power)]),
             diff_mean = mean(x$power_diff, na.rm = TRUE),
             diff_median = stats::median(x$power_diff, na.rm = TRUE),
             diff_q90_loss = -as.numeric(stats::quantile(x$power_diff, 0.1,
                                                          na.rm = TRUE)),
             diff_min = min(x$power_diff, na.rm = TRUE),
             diff_max = max(x$power_diff, na.rm = TRUE),
             stringsAsFactors = FALSE)
}))
rownames(oracle_summary) <- NULL

grp_f <- interaction(final_dev$id, final_dev$method, drop = TRUE, sep = "|",
                     lex.order = TRUE)
final_summary <- do.call(rbind, lapply(split(final_dev, grp_f), function(x) {
  j <- which.max(x$type1)
  data.frame(id = x$id[1], scenario = x$scenario[1], design = x$design[1],
             rule = x$rule[1], method = x$method[1],
             type1_max = x$type1[j], final_at_max = x$final_n[j],
             n_exceed = sum(x$exceeds_alpha), n_schedules = nrow(x),
             stringsAsFactors = FALSE)
}))
rownames(final_summary) <- NULL

saveRDS(list(random = random_summary, oracle = oracle_summary,
             final = final_summary),
        file.path(out_dir, "summaries.rds"))

for (i in seq_len(nrow(random_summary))) {
  r <- random_summary[i, ]
  lab <- if (r$method == "spending") "proposed" else r$method
  for (v in c("type1_mean", "type1_median", "type1_min", "type1_max",
              "exceed_pct", "power_mean", "en0_mean", "en1_mean")) {
    add_number(paste("random", r$id, r$deviation, lab, v, sep = "."), r[[v]],
               paste("Random schedules:", v))
  }
}
for (i in seq_len(nrow(oracle_summary))) {
  r <- oracle_summary[i, ]
  lab <- if (r$method == "spending") "proposed" else r$method
  for (v in c("n", "n_failed", "oracle_power_mean", "power_mean",
              "diff_mean", "diff_median", "diff_q90_loss", "diff_min",
              "diff_max")) {
    add_number(paste("oracle", r$id, lab, v, sep = "."), r[[v]],
               paste("Oracle comparison, moderate deviations:", v))
  }
}
for (i in seq_len(nrow(final_summary))) {
  r <- final_summary[i, ]
  lab <- if (r$method == "spending") "proposed" else r$method
  for (v in c("type1_max", "final_at_max", "n_exceed", "n_schedules")) {
    add_number(paste("final", r$id, lab, v, sep = "."), r[[v]],
               paste("Final sample size deviation:", v))
  }
}

# Overall numbers across configurations
for (dev in deviations$deviation) {
  for (rule in c("binding", "non-binding")) {
    for (meth in c("naive", "spending")) {
      x <- random_summary[random_summary$deviation == dev &
                            random_summary$rule == rule &
                            random_summary$method == meth, ]
      lab <- if (meth == "spending") "proposed" else meth
      key <- paste("overall", dev, rule, lab, sep = ".")
      add_number(paste0(key, ".type1_max"), max(x$type1_max),
                 "Largest type I error rate over configurations")
      add_number(paste0(key, ".type1_mean_min"), min(x$type1_mean),
                 "Smallest mean type I error rate over configurations")
      add_number(paste0(key, ".type1_mean_max"), max(x$type1_mean),
                 "Largest mean type I error rate over configurations")
      add_number(paste0(key, ".exceed_pct_min"), min(x$exceed_pct),
                 "Smallest percentage of schedules above alpha")
      add_number(paste0(key, ".exceed_pct_max"), max(x$exceed_pct),
                 "Largest percentage of schedules above alpha")
    }
  }
}
add_number("overall.proposed.n_exceed",
           sum(random$exceeds_alpha[random$method == "spending"]),
           "Schedules with a type I error rate above alpha, proposed")
add_number("overall.proposed.n_evaluated", sum(random$method == "spending"),
           "Schedule and configuration pairs evaluated, proposed")
add_number("overall.final.proposed.n_exceed",
           sum(final_dev$exceeds_alpha[final_dev$method == "spending"]),
           "Final deviations with a type I error rate above alpha, proposed")
xo <- oracle_summary[oracle_summary$method == "spending", ]
add_number("overall.oracle.proposed.diff_mean_min", min(xo$diff_mean),
           "Smallest mean power difference, proposed minus oracle")
add_number("overall.oracle.proposed.diff_mean_max", max(xo$diff_mean),
           "Largest mean power difference, proposed minus oracle")
add_number("overall.oracle.proposed.diff_min", min(xo$diff_min),
           "Largest power loss of the proposed design against the oracle")
add_number("overall.oracle.n_failed", sum(oracle_summary$n_failed) / 2,
           "Oracle calibrations without a feasible design")

numbers <- do.call(rbind, numbers)
utils::write.csv(numbers, file.path(out_dir, "numbers.csv"),
                 row.names = FALSE)
saveRDS(numbers, file.path(out_dir, "numbers.rds"))
writeLines(c(paste("Finished:", format(Sys.time())),
             paste("Elapsed:", elapsed()),
             utils::capture.output(utils::sessionInfo())),
           file.path(out_dir, "session_info.txt"))
message("Done (", elapsed(), ")")
