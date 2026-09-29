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
#
# Methods compared at the actual analyses:
#   naive         planned probability cutoffs at the actual sample sizes
#   spending      flex_update(), the proposed method
#   recalibrated  naive boundaries at the interim analyses and, at the final
#                 analysis, the final boundary of the design calibrated again
#                 on the actual schedule (the design re-run with the actual
#                 sample sizes once they are known)
#   ce            conditional error rule for a deviation of the final sample
#                 size only: given the count at the last interim analysis,
#                 the final boundary keeps the conditional type I error rate
#                 of the planned design
#   oracle        design calibrated on the actual schedule as if it had been
#                 known in advance (not applicable in practice; a benchmark)

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

# Distribution of the number of responses after adding inc patients
conv <- function(dist, inc, p) {
  pmf <- stats::dbinom(0:inc, inc, p)
  out <- numeric(length(dist) + inc)
  for (j in 0:inc) {
    idx <- seq_along(dist) + j
    out[idx] <- out[idx] + pmf[j + 1] * dist
  }
  out
}

# Operating characteristics of the conditional error rule when the interim
# analyses follow the plan and the final analysis has n_final patients
ce_final_oc <- function(d, n_final, p, binding) {
  num_looks <- length(d$n)
  m_last <- d$n[num_looks - 1]
  running <- function(pp, bind) {
    dist <- 1
    prev <- 0
    rej <- 0
    en <- 0
    for (k in seq_len(num_looks - 1)) {
      dist <- conv(dist, d$n[k] - prev, pp)
      prev <- d$n[k]
      s <- 0:d$n[k]
      at_eff <- s >= d$boundary$efficacy[k]
      at_fut <- if (bind) s <= d$boundary$futility[k] & !at_eff else
        rep(FALSE, length(s))
      rej <- rej + sum(dist[at_eff])
      en <- en + d$n[k] * sum(dist[at_eff | at_fut])
      dist[at_eff | at_fut] <- 0
    }
    list(dist = dist, rej = rej, en = en)
  }
  run_p <- running(p, binding)
  rem_plan <- d$N - m_last
  rem <- n_final - m_last
  e_plan <- d$boundary$efficacy[num_looks]
  rej <- run_p$rej
  for (s in 0:m_last) {
    w <- run_p$dist[s + 1]
    if (w == 0) next
    cond_plan <- stats::pbinom(e_plan - s - 1, rem_plan, d$p0,
                               lower.tail = FALSE)
    need <- 0:(rem + 1)
    cond_new <- stats::pbinom(need - 1, rem, d$p0, lower.tail = FALSE)
    c_min <- min(need[cond_new <= cond_plan * (1 + 1e-9)])
    rej <- rej + w * stats::pbinom(c_min - 1, rem, p, lower.tail = FALSE)
  }
  list(reject = rej, en = run_p$en + n_final * sum(run_p$dist))
}

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

# 4. Oracle designs calibrated on the actual schedule and the recalibrated
# final boundary, moderate deviations
message("Oracle designs (", elapsed(), ")")
oracle <- list()
for (i in seq_len(nrow(config))) {
  sc <- scenarios[config$s[i], ]
  dg <- designs[config$d[i], ]
  sch <- schedules[[paste(sc$scenario, "moderate", sep = "_")]]
  sch <- sch[seq_len(min(n_oracle, length(sch)))]
  res <- lapply(seq_along(sch), function(j) {
    od <- tryCatch(
      flex_design(p0 = sc$p0, p1 = sc$p1, n = sch[[j]], alpha = sc$alpha,
                  type = dg$type, method = dg$method,
                  binding = config$binding[i]),
      error = function(e) NULL)
    out <- data.frame(id = config$id[i], schedule = j,
                      oracle_type1 = NA_real_, oracle_power = NA_real_,
                      oracle_en0 = NA_real_, oracle_final = NA_integer_,
                      recal_type1 = NA_real_, recal_power = NA_real_,
                      stringsAsFactors = FALSE)
    if (!is.null(od)) {
      nv <- flex_naive(planned[[i]], sch[[j]], final = TRUE)$bounds
      k <- nrow(nv)
      eff <- nv$efficacy
      fut <- nv$futility
      eff[k] <- od$boundary$efficacy[k]
      fut[k] <- eff[k] - 1L
      out$oracle_type1 <- od$oc$type1
      out$oracle_power <- od$oc$power
      out$oracle_en0 <- od$oc$en0
      out$oracle_final <- eff[k]
      out$recal_type1 <- flex_oc(sch[[j]], fut, eff, sc$p0,
                                 binding = config$binding[i])$reject
      out$recal_power <- flex_oc(sch[[j]], fut, eff, sc$p1)$reject
    }
    out
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
  extra <- lapply(seq_along(finals), function(j) {
    f <- finals[j]
    ce0 <- ce_final_oc(d, f, d$p0, d$binding)
    ce0_applied <- ce_final_oc(d, f, d$p0, TRUE)
    ce1 <- ce_final_oc(d, f, d$p1, TRUE)
    od <- tryCatch(
      flex_design(p0 = d$p0, p1 = d$p1, n = sch[[j]], alpha = d$alpha,
                  type = d$type,
                  method = if (is.na(d$method)) "power" else d$method,
                  binding = d$binding),
      error = function(e) NULL)
    if (is.null(od)) {
      na_oc <- list(reject = NA_real_, en = NA_real_)
      rc0 <- rc0_applied <- rc1 <- na_oc
      od <- list(oc = list(type1 = NA_real_, power = NA_real_,
                           en0 = NA_real_, en1 = NA_real_))
    } else {
      eff <- c(d$boundary$efficacy[-num_looks],
               od$boundary$efficacy[num_looks])
      fut <- c(d$boundary$futility[-num_looks], eff[num_looks] - 1L)
      rc0 <- flex_oc(sch[[j]], fut, eff, d$p0, binding = d$binding)
      rc0_applied <- flex_oc(sch[[j]], fut, eff, d$p0)
      rc1 <- flex_oc(sch[[j]], fut, eff, d$p1)
    }
    data.frame(
      schedule = j, looks = paste(sch[[j]], collapse = ","),
      method = c("ce", "recalibrated", "oracle"),
      type1 = c(ce0$reject, rc0$reject, od$oc$type1),
      power = c(ce1$reject, rc1$reject, od$oc$power),
      en0 = c(ce0_applied$en, rc0_applied$en, od$oc$en0),
      en1 = c(ce1$en, rc1$en, od$oc$en1),
      exceeds_alpha = c(ce0$reject, rc0$reject, od$oc$type1) >
        d$alpha * (1 + 1e-9),
      stringsAsFactors = FALSE)
  })
  res <- rbind(res, do.call(rbind, extra))
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
                         random$schedule <= n_oracle, ],
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
             n_exceed = sum(x$exceeds_alpha, na.rm = TRUE),
             n_schedules = nrow(x),
             stringsAsFactors = FALSE)
}))
rownames(final_summary) <- NULL

recal_summary <- do.call(rbind, lapply(split(oracle, oracle$id), function(x) {
  a <- planned[[x$id[1]]]$alpha
  ok <- !is.na(x$recal_type1)
  data.frame(id = x$id[1], n = sum(ok), n_failed = sum(!ok),
             recal_type1_mean = mean(x$recal_type1[ok]),
             recal_type1_max = max(x$recal_type1[ok]),
             recal_exceed_pct = 100 * mean(x$recal_type1[ok] > a * (1 + 1e-9)),
             recal_power_mean = mean(x$recal_power[ok]),
             oracle_power_mean = mean(x$oracle_power[ok]),
             stringsAsFactors = FALSE)
}))
rownames(recal_summary) <- NULL

# Power of the proposed boundaries against the conditional error rule when
# only the final sample size deviates, over the final sample sizes other than
# the planned maximum (at the planned maximum both equal the planned design)
tol_power <- 1e-9
ce_vs_summary <- do.call(rbind, lapply(split(final_dev, final_dev$id),
                                       function(x) {
  n_max <- planned[[x$id[1]]]$N
  pr <- x[x$method == "spending" & x$final_n != n_max, c("final_n", "power")]
  ce <- x[x$method == "ce" & x$final_n != n_max, c("final_n", "power")]
  pc <- merge(pr, ce, by = "final_n", suffixes = c("_proposed", "_ce"))
  d <- pc$power_proposed - pc$power_ce
  data.frame(id = x$id[1], scenario = x$scenario[1], design = x$design[1],
             rule = x$rule[1], n = nrow(pc),
             n_higher = sum(d > tol_power),
             n_equal = sum(abs(d) <= tol_power),
             n_lower = sum(d < -tol_power),
             diff_mean = mean(d), diff_min = min(d), diff_max = max(d),
             stringsAsFactors = FALSE)
}))
rownames(ce_vs_summary) <- NULL

saveRDS(list(random = random_summary, oracle = oracle_summary,
             final = final_summary, recalibrated = recal_summary,
             ce_vs_proposed = ce_vs_summary),
        file.path(out_dir, "summaries.rds"))
for (i in seq_len(nrow(recal_summary))) {
  r <- recal_summary[i, ]
  for (v in c("n", "n_failed", "recal_type1_mean", "recal_type1_max",
              "recal_exceed_pct", "recal_power_mean", "oracle_power_mean")) {
    add_number(paste("recalibrated", r$id, v, sep = "."), r[[v]],
               paste("Recalibrated final boundary, moderate deviations:", v))
  }
}

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
for (i in seq_len(nrow(ce_vs_summary))) {
  r <- ce_vs_summary[i, ]
  for (v in c("n", "n_higher", "n_equal", "n_lower", "diff_mean", "diff_min",
              "diff_max")) {
    add_number(paste("ce_vs_proposed", r$id, v, sep = "."), r[[v]],
               paste("Final sample size deviation, power of the proposed",
                     "boundaries minus the conditional error rule:", v))
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
xr <- recal_summary[grepl("_binding$", recal_summary$id), ]
add_number("overall.recalibrated.binding.type1_max", max(xr$recal_type1_max),
           "Largest type I error rate of the recalibrated final boundary")
add_number("overall.recalibrated.binding.exceed_pct_max",
           max(xr$recal_exceed_pct),
           "Largest percentage above alpha, recalibrated final boundary")
add_number("overall.recalibrated.binding.exceed_pct_min",
           min(xr$recal_exceed_pct),
           "Smallest percentage above alpha, recalibrated final boundary")
for (meth in c("ce", "recalibrated", "oracle")) {
  x <- final_summary[final_summary$method == meth, ]
  add_number(paste0("overall.final.", meth, ".type1_max"), max(x$type1_max),
             "Largest type I error rate over final sample size deviations")
  add_number(paste0("overall.final.", meth, ".n_exceed"), sum(x$n_exceed),
             "Final sample size deviations with type I error above alpha")
}
for (dg in designs$design) {
  x <- ce_vs_summary[ce_vs_summary$design == dg &
                       ce_vs_summary$rule == "binding", ]
  key <- paste("overall.ce_vs_proposed", dg, "binding", sep = ".")
  add_number(paste0(key, ".n"), sum(x$n),
             "Final sample sizes compared, proposed against conditional error")
  add_number(paste0(key, ".n_higher"), sum(x$n_higher),
             "Final sample sizes with higher power for the proposed boundaries")
  add_number(paste0(key, ".n_equal"), sum(x$n_equal),
             "Final sample sizes with equal power")
  add_number(paste0(key, ".n_lower"), sum(x$n_lower),
             "Final sample sizes with lower power for the proposed boundaries")
  add_number(paste0(key, ".diff_min"), min(x$diff_min),
             "Smallest power difference, proposed minus conditional error")
  add_number(paste0(key, ".diff_max"), max(x$diff_max),
             "Largest power difference, proposed minus conditional error")
}
xo <- oracle_summary[oracle_summary$method == "spending" &
                       grepl("_binding$", oracle_summary$id), ]
add_number("overall.oracle.proposed.diff_mean_min", min(xo$diff_mean),
           "Smallest mean power difference, proposed minus oracle")
add_number("overall.oracle.proposed.diff_mean_max", max(xo$diff_mean),
           "Largest mean power difference, proposed minus oracle")
add_number("overall.oracle.proposed.diff_min", min(xo$diff_min),
           "Largest power loss of the proposed design against the oracle")
add_number("overall.oracle.n_failed", sum(recal_summary$n_failed),
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
