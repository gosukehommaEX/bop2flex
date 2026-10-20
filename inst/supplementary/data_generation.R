# Data generation for the supplementary material of the article on sample
# size deviations in the BOP2 and BOP2-FE designs.
#
# Run from the package root after inst/scripts/data_generation.R, whose
# results (planned designs and random schedules) are read here:
#   devtools::load_all()
#   source(file.path("inst", "supplementary", "data_generation.R"))
#
# Results are written to inst/supplementary/output/data, with every number
# that may be quoted in the supplementary material listed in numbers.csv.
#
# Analyses
#   A. Effect of the floor on the efficacy boundary (flex_update with
#      efficacy_floor = FALSE)
#   B. Exhaustive enumeration of all schedules within a range of deviations
#   C. Other spending functions, alpha t^rho, instead of the implied one
#   D. All interim analyses shifted by the same number of patients
#   E. A setting based on a published trial (TREND; Zhang et al. 2025) in
#      which fewer patients than enrolled were evaluable
#   F. Grid points that give the same planned boundaries: the analyses of the
#      main article and of B and D repeated with the grid point of the
#      largest lambda instead of the reported one, and the grid points of the
#      TREND setting
#   G. Naive boundaries over the exhaustive schedules of B for every grid
#      point that gives the planned boundaries
#   H. Designs specified by their boundary table: floor of flex_update() and
#      the planned boundaries on the planned schedule

if (!exists("flex_design", mode = "function")) library(bop2flex)

main_dir <- file.path("inst", "scripts", "output", "data")
out_dir <- file.path("inst", "supplementary", "output", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
t_start <- Sys.time()
elapsed <- function() {
  sprintf("%.1f min", as.numeric(difftime(Sys.time(), t_start,
                                          units = "mins")))
}

pl <- readRDS(file.path(main_dir, "planned_designs.rds"))
schedules <- readRDS(file.path(main_dir, "random_schedules.rds"))
config <- pl$config
planned <- pl$planned
scenarios <- pl$scenarios

# Settings
n_spending <- 500
exhaustive <- data.frame(scenario = paste0("S", 1:6),
                         interim_shift = c(1, 1, 1, 1, 2, 2),
                         final_shift = c(2, 2, 2, 2, 4, 4),
                         stringsAsFactors = FALSE)
interim_shifts <- -3:3
spending_rho <- c(1, 2, 3)
trend <- list(p0 = 0.41, p1 = 0.56, n = c(48, 65), alpha = 0.05,
              schedules = list(c(44, 65), c(44, 54)))

numbers <- list()
add_number <- function(key, value, description) {
  numbers[[length(numbers) + 1]] <<- data.frame(
    key = key, value = as.numeric(value), description = description,
    stringsAsFactors = FALSE)
}
add_number("settings.n_spending", n_spending,
           "Schedules per configuration for the spending functions")
for (r in seq_len(nrow(exhaustive))) {
  add_number(paste0("settings.exhaustive.", exhaustive$scenario[r],
                    ".interim_shift"), exhaustive$interim_shift[r],
             "Exhaustive enumeration: largest interim shift")
  add_number(paste0("settings.exhaustive.", exhaustive$scenario[r],
                    ".final_shift"), exhaustive$final_shift[r],
             "Exhaustive enumeration: largest final shift")
}

# Type I error rate (with the futility rule of the design) and power of the
# boundaries in a flex_bounds object
oc_bounds <- function(b, d) {
  x <- b$bounds
  t1 <- flex_oc(x$m, x$futility, x$efficacy, d$p0,
                binding = d$binding)$reject
  pw <- if (is.null(d$p1)) NA_real_ else
    flex_oc(x$m, x$futility, x$efficacy, d$p1)$reject
  c(type1 = t1, power = pw)
}

# A. Floor on the efficacy boundary
message("A. Efficacy floor (", elapsed(), ")")
floor_res <- list()
for (i in seq_len(nrow(config))) {
  d <- planned[[i]]
  for (dev in c("moderate", "large")) {
    sch <- schedules[[paste(config$scenario[i], dev, sep = "_")]]
    rows <- lapply(seq_along(sch), function(j) {
      b1 <- flex_update(d, sch[[j]], final = TRUE)
      b0 <- flex_update(d, sch[[j]], final = TRUE, efficacy_floor = FALSE)
      o1 <- oc_bounds(b1, d)
      o0 <- oc_bounds(b0, d)
      k <- nrow(b1$bounds)
      data.frame(id = config$id[i], deviation = dev, schedule = j,
                 changed = !identical(b1$bounds$efficacy,
                                      b0$bounds$efficacy),
                 max_raise = max(b1$bounds$efficacy - b0$bounds$efficacy),
                 final_eff_floor = b1$bounds$efficacy[k],
                 final_eff_nofloor = b0$bounds$efficacy[k],
                 type1_floor = o1[["type1"]], type1_nofloor = o0[["type1"]],
                 power_floor = o1[["power"]], power_nofloor = o0[["power"]],
                 stringsAsFactors = FALSE)
    })
    floor_res[[length(floor_res) + 1]] <- do.call(rbind, rows)
  }
  message("  ", config$id[i], " done (", elapsed(), ")")
}
floor_res <- do.call(rbind, floor_res)
rownames(floor_res) <- NULL
floor_summary <- do.call(rbind, lapply(
  split(floor_res, interaction(floor_res$id, floor_res$deviation,
                               drop = TRUE, sep = "|", lex.order = TRUE)),
  function(x) {
    a <- planned[[x$id[1]]]$alpha
    data.frame(id = x$id[1], deviation = x$deviation[1], n = nrow(x),
               changed_pct = 100 * mean(x$changed),
               max_raise = max(x$max_raise),
               n_final_zero = sum(x$final_eff_nofloor == 0),
               type1_floor_mean = mean(x$type1_floor),
               type1_floor_max = max(x$type1_floor),
               type1_nofloor_mean = mean(x$type1_nofloor),
               type1_nofloor_max = max(x$type1_nofloor),
               n_exceed_nofloor = sum(x$type1_nofloor > a * (1 + 1e-9)),
               power_floor_mean = mean(x$power_floor),
               power_nofloor_mean = mean(x$power_nofloor),
               power_loss_max = max(x$power_nofloor - x$power_floor),
               stringsAsFactors = FALSE)
  }))
rownames(floor_summary) <- NULL
saveRDS(list(results = floor_res, summary = floor_summary),
        file.path(out_dir, "efficacy_floor.rds"))
for (r in seq_len(nrow(floor_summary))) {
  x <- floor_summary[r, ]
  for (v in setdiff(names(x), c("id", "deviation"))) {
    add_number(paste("floor", x$id, x$deviation, v, sep = "."), x[[v]],
               paste("Efficacy floor:", v))
  }
}

# B. Exhaustive enumeration
message("B. Exhaustive enumeration (", elapsed(), ")")
enum_schedules <- function(n, a, b) {
  num_looks <- length(n)
  grid <- expand.grid(c(rep(list(-a:a), num_looks - 1), list(-b:b)))
  sch <- lapply(seq_len(nrow(grid)), function(r) {
    s <- n + as.integer(unlist(grid[r, ]))
    s <- pmax(cummax(s), 1L)
    as.integer(sort(unique(s)))
  })
  sch[!duplicated(vapply(sch, paste, character(1), collapse = ","))]
}
exh_res <- list()
for (i in seq_len(nrow(config))) {
  d <- planned[[i]]
  ex <- exhaustive[exhaustive$scenario == config$scenario[i], ]
  sch <- enum_schedules(d$n, ex$interim_shift, ex$final_shift)
  res <- flex_compare(d, sch)
  res$id <- config$id[i]
  exh_res[[i]] <- res
  message("  ", config$id[i], ": ", length(sch), " schedules (", elapsed(),
          ")")
}
exh_res <- do.call(rbind, exh_res)
rownames(exh_res) <- NULL
exh_summary <- do.call(rbind, lapply(
  split(exh_res, interaction(exh_res$id, exh_res$method, drop = TRUE,
                             sep = "|", lex.order = TRUE)),
  function(x) {
    data.frame(id = x$id[1], method = x$method[1], n = nrow(x),
               type1_mean = mean(x$type1), type1_max = max(x$type1),
               exceed_pct = 100 * mean(x$exceeds_alpha),
               n_exceed = sum(x$exceeds_alpha), stringsAsFactors = FALSE)
  }))
rownames(exh_summary) <- NULL
saveRDS(list(results = exh_res, summary = exh_summary, settings = exhaustive),
        file.path(out_dir, "exhaustive.rds"))
for (r in seq_len(nrow(exh_summary))) {
  x <- exh_summary[r, ]
  lab <- if (x$method == "spending") "proposed" else x$method
  for (v in c("n", "type1_mean", "type1_max", "exceed_pct", "n_exceed")) {
    add_number(paste("exhaustive", x$id, lab, v, sep = "."), x[[v]],
               paste("Exhaustive enumeration:", v))
  }
}
add_number("overall.exhaustive.proposed.n_exceed",
           sum(exh_summary$n_exceed[exh_summary$method == "spending"]),
           "Exhaustive enumeration: schedules above alpha, proposed")
add_number("overall.exhaustive.n_evaluated",
           sum(exh_summary$n[exh_summary$method == "spending"]),
           "Exhaustive enumeration: schedule and design pairs evaluated")

# C. Other spending functions
message("C. Spending functions (", elapsed(), ")")
spend_res <- list()
tt <- seq(0, 1, length.out = 201)
for (i in which(config$binding)) {
  d <- planned[[i]]
  sch <- schedules[[paste(config$scenario[i], "moderate", sep = "_")]]
  sch <- sch[seq_len(min(n_spending, length(sch)))]
  specs <- list(implied = flex_spending(d)[, c("t", "alpha_cum")])
  for (rho in spending_rho) {
    specs[[paste0("rho", rho)]] <- data.frame(t = tt,
                                              alpha_cum = d$alpha * tt^rho)
  }
  for (nm in names(specs)) {
    bp <- flex_update(d, d$n, final = TRUE, spending = specs[[nm]])
    op <- oc_bounds(bp, d)
    rr <- vapply(sch, function(s) {
      oc_bounds(flex_update(d, s, final = TRUE, spending = specs[[nm]]), d)
    }, numeric(2))
    spend_res[[length(spend_res) + 1]] <- data.frame(
      id = config$id[i], scenario = config$scenario[i],
      design = config$design[i], spending = nm,
      reproduces = identical(bp$bounds$efficacy, d$boundary$efficacy) &&
        identical(bp$bounds$futility, d$boundary$futility),
      planned_type1 = op[["type1"]], planned_power = op[["power"]],
      type1_mean = mean(rr["type1", ]), type1_max = max(rr["type1", ]),
      power_mean = mean(rr["power", ]), stringsAsFactors = FALSE)
  }
  message("  ", config$id[i], " done (", elapsed(), ")")
}
spend_res <- do.call(rbind, spend_res)
rownames(spend_res) <- NULL
saveRDS(spend_res, file.path(out_dir, "spending_functions.rds"))
for (r in seq_len(nrow(spend_res))) {
  x <- spend_res[r, ]
  for (v in c("reproduces", "planned_type1", "planned_power", "type1_mean",
              "type1_max", "power_mean")) {
    add_number(paste("spending", x$id, x$spending, v, sep = "."), x[[v]],
               paste("Spending function:", v))
  }
}
add_number("overall.spending.implied.n_reproduces",
           sum(spend_res$reproduces[spend_res$spending == "implied"]),
           "Designs whose planned boundaries the implied spending recovers")
add_number("overall.spending.other.n_reproduces",
           sum(spend_res$reproduces[spend_res$spending != "implied"]),
           "Design and spending pairs recovering the plan, other functions")
add_number("overall.spending.n_designs", sum(config$binding),
           "Designs in the spending function comparison")

# D. All interim analyses shifted by the same number of patients
message("D. Interim shifts (", elapsed(), ")")
shift_res <- list()
for (i in seq_len(nrow(config))) {
  d <- planned[[i]]
  num_looks <- length(d$n)
  sch <- lapply(interim_shifts, function(k) c(d$n[-num_looks] + k, d$N))
  valid <- vapply(sch, function(s) all(s >= 1) && all(diff(s) > 0),
                  logical(1))
  res <- flex_compare(d, sch[valid])
  res$shift <- interim_shifts[valid][res$schedule]
  res$id <- config$id[i]
  res$scenario <- config$scenario[i]
  res$design <- config$design[i]
  res$rule <- config$rule[i]
  shift_res[[i]] <- res
}
shift_res <- do.call(rbind, shift_res)
rownames(shift_res) <- NULL
saveRDS(shift_res, file.path(out_dir, "interim_shift.rds"))
for (meth in c("naive", "spending")) {
  x <- shift_res[shift_res$method == meth & shift_res$rule == "binding", ]
  lab <- if (meth == "spending") "proposed" else meth
  add_number(paste0("overall.shift.", lab, ".type1_max"), max(x$type1),
             "Interim shifts: largest type I error rate, binding designs")
  add_number(paste0("overall.shift.", lab, ".n_exceed"),
             sum(x$exceeds_alpha),
             "Interim shifts: cases above alpha, binding designs")
  add_number(paste0("overall.shift.", lab, ".n_cases"), nrow(x),
             "Interim shifts: cases, binding designs")
}

# E. Setting based on the TREND trial
message("E. TREND setting (", elapsed(), ")")
tr_design <- flex_design(p0 = trend$p0, p1 = trend$p1, n = trend$n,
                         alpha = trend$alpha)
tr_rows <- list()
tr_add <- function(label, method, b) {
  x <- b$bounds
  k <- nrow(x)
  o <- oc_bounds(b, tr_design)
  data.frame(schedule = label, method = method,
             interim_m = x$m[1], interim_futility = x$futility[1],
             interim_efficacy = x$efficacy[1], final_m = x$m[k],
             final_efficacy = x$efficacy[k], type1 = o[["type1"]],
             power = o[["power"]], stringsAsFactors = FALSE)
}
tr_rows[[1]] <- tr_add("planned", "planned",
                       flex_update(tr_design, tr_design$n, final = TRUE))
for (s in trend$schedules) {
  label <- paste(s, collapse = ",")
  nv <- flex_naive(tr_design, s, final = TRUE)
  carry <- nv
  carry$bounds$futility[1] <- tr_design$boundary$futility[1]
  carry$bounds$efficacy[1] <- tr_design$boundary$efficacy[1]
  tr_rows[[length(tr_rows) + 1]] <- tr_add(label, "planned counts", carry)
  tr_rows[[length(tr_rows) + 1]] <- tr_add(label, "naive", nv)
  tr_rows[[length(tr_rows) + 1]] <- tr_add(
    label, "spending", flex_update(tr_design, s, final = TRUE))
}
tr_res <- do.call(rbind, tr_rows)
rownames(tr_res) <- NULL
saveRDS(list(design = tr_design, settings = trend, results = tr_res),
        file.path(out_dir, "trend_setting.rds"))
for (v in c("lambda", "gamma", "eta")) {
  add_number(paste0("trend.design.", v), tr_design[[v]],
             "TREND setting: tuning parameter")
}
for (v in c("type1", "power", "en0")) {
  add_number(paste0("trend.design.", v), tr_design$oc[[v]],
             "TREND setting: planned operating characteristic")
}
for (r in seq_len(nrow(tr_res))) {
  x <- tr_res[r, ]
  lab <- if (x$method == "spending") "proposed" else
    gsub(" ", "_", x$method)
  for (v in c("interim_futility", "interim_efficacy", "final_efficacy",
              "type1", "power")) {
    add_number(paste("trend", gsub(",", "_", x$schedule), lab, v, sep = "."),
               x[[v]], paste("TREND setting:", v))
  }
}

# F. Grid points that give the same planned boundaries
# flex_design() reports the first grid point in grid order (the smallest
# lambda, then the smallest gamma and eta) among those that give the selected
# boundaries. The grid point with the largest lambda (then the smallest gamma
# and eta) defines the same planned design but different probability cutoffs
# at other sample sizes. It changes the naive boundaries and, through the
# floor and the futility cutoff, the proposed ones. When it coincides with the
# reported grid point, the results of the main article and of B and D are
# reused.
message("F. Equivalent grid points (", elapsed(), ")")
main_final <- readRDS(file.path(main_dir, "final_deviation.rds"))
main_random <- readRDS(file.path(main_dir, "random_results.rds"))
main_oracle <- readRDS(file.path(main_dir, "oracle_results.rds"))
designs <- pl$designs
tol_power <- 1e-9
final_range <- 6
example_id <- "S1_BOP2-FE-1_binding"
example_schedule <- c(12, 16, 19, 25, 31, 37, 39)

# Summaries of one design from flex_compare() results for the exhaustive
# schedules (exh), the final sample sizes (fin, with column final_n), the
# common interim shifts (sh) and the random schedules (rnd, with columns
# deviation and schedule), from the conditional error rule (ce) and from the
# oracle power and recalibrated type I error rate (orc, by schedule)
sens_summary <- function(d, exh, fin, sh, rnd, orc, ce) {
  out <- list()
  x <- exh[exh$method == "naive", ]
  out$exhaustive.n <- nrow(x)
  out$exhaustive.naive.n_exceed <- sum(x$exceeds_alpha)
  out$exhaustive.naive.exceed_pct <- 100 * mean(x$exceeds_alpha)
  out$exhaustive.naive.type1_max <- max(x$type1)
  out$exhaustive.proposed.n_exceed <-
    sum(exh$exceeds_alpha[exh$method == "spending"])
  x <- fin[fin$method == "naive", ]
  out$final.n <- nrow(x)
  out$final.naive.n_exceed <- sum(x$exceeds_alpha)
  out$final.naive.type1_max <- max(x$type1)
  pr <- fin[fin$method == "spending", ]
  out$final.proposed.n_exceed <- sum(pr$exceeds_alpha)
  pr <- pr[pr$final_n != d$N, c("final_n", "power")]
  pc <- merge(pr, ce[ce$final_n != d$N, c("final_n", "power")],
              by = "final_n", suffixes = c("_proposed", "_ce"))
  dd <- pc$power_proposed - pc$power_ce
  out$ce_vs_proposed.n <- nrow(pc)
  out$ce_vs_proposed.n_higher <- sum(dd > tol_power)
  out$ce_vs_proposed.n_equal <- sum(abs(dd) <= tol_power)
  out$ce_vs_proposed.n_lower <- sum(dd < -tol_power)
  out$ce_vs_proposed.diff_min <- min(dd)
  out$ce_vs_proposed.diff_max <- max(dd)
  x <- sh[sh$method == "naive", ]
  out$shift.n <- nrow(x)
  out$shift.naive.n_exceed <- sum(x$exceeds_alpha)
  out$shift.proposed.n_exceed <- sum(sh$exceeds_alpha[sh$method == "spending"])
  for (dev in c("moderate", "large")) {
    x <- rnd[rnd$deviation == dev & rnd$method == "naive", ]
    y <- rnd[rnd$deviation == dev & rnd$method == "spending", ]
    key <- paste0("random.", dev)
    out[[paste0(key, ".naive.type1_mean")]] <- mean(x$type1)
    out[[paste0(key, ".naive.type1_max")]] <- max(x$type1)
    out[[paste0(key, ".naive.exceed_pct")]] <- 100 * mean(x$exceeds_alpha)
    out[[paste0(key, ".proposed.type1_mean")]] <- mean(y$type1)
    out[[paste0(key, ".proposed.type1_max")]] <- max(y$type1)
    out[[paste0(key, ".proposed.n_exceed")]] <- sum(y$exceeds_alpha)
    out[[paste0(key, ".proposed.power_mean")]] <- mean(y$power)
  }
  y <- rnd[rnd$deviation == "moderate" & rnd$method == "spending" &
             rnd$schedule <= max(orc$schedule), c("schedule", "power")]
  po <- merge(y, orc, by = "schedule")
  ok <- !is.na(po$oracle_power)
  diff <- po$power[ok] - po$oracle_power[ok]
  out$oracle.proposed.n <- sum(ok)
  out$oracle.proposed.diff_mean <- mean(diff)
  out$oracle.proposed.diff_min <- min(diff)
  out$oracle.proposed.diff_max <- max(diff)
  ok <- !is.na(orc$recal_type1)
  out$recalibrated.type1_max <- max(orc$recal_type1[ok])
  out$recalibrated.exceed_pct <-
    100 * mean(orc$recal_type1[ok] > d$alpha * (1 + 1e-9))
  out
}

sens <- list()
eq_list <- list()
for (i in seq_len(nrow(config))) {
  id <- config$id[i]
  d <- planned[[i]]
  dg <- designs[config$d[i], ]
  cal <- flex_design(p0 = d$p0, p1 = d$p1, n = d$n, alpha = d$alpha,
                     type = dg$type, method = dg$method,
                     binding = config$binding[i])
  if (!isTRUE(all.equal(cal$boundary, d$boundary))) {
    stop("The calibration of ", id, " does not reproduce the planned design.")
  }
  eq <- cal$calibration$equivalent
  eq_list[[id]] <- eq
  top <- eq[eq$lambda == max(eq$lambda), ][1, ]
  dl <- flex_plan(p0 = d$p0, n = d$n, alpha = d$alpha, lambda = top$lambda,
                  gamma = top$gamma,
                  eta = if (is.na(top$eta)) NULL else top$eta,
                  type = dg$type, method = dg$method, p1 = d$p1,
                  binding = config$binding[i])
  if (!isTRUE(all.equal(dl$boundary, d$boundary))) {
    stop("The largest lambda of ", id, " does not give the planned design.")
  }
  same <- isTRUE(all.equal(c(top$lambda, top$gamma), c(d$lambda, d$gamma))) &&
    (is.na(top$eta) || isTRUE(all.equal(top$eta, d$eta)))
  ce <- main_final[main_final$id == id & main_final$method == "ce", ]
  or_i <- main_oracle[main_oracle$id == id, ]
  if (same) {
    exh <- exh_res[exh_res$id == id, ]
    fin <- main_final[main_final$id == id &
                        main_final$method %in% c("naive", "spending"), ]
    sh <- shift_res[shift_res$id == id, ]
    rnd <- main_random[main_random$id == id, ]
    orc <- or_i[, c("schedule", "oracle_power", "recal_type1")]
  } else {
    ex <- exhaustive[exhaustive$scenario == config$scenario[i], ]
    exh <- flex_compare(dl, enum_schedules(d$n, ex$interim_shift,
                                           ex$final_shift))
    num_looks <- length(d$n)
    finals <- seq(max(d$n[num_looks - 1] + 1L, d$N - final_range),
                  d$N + final_range)
    fin <- flex_compare(dl, lapply(finals, function(f) {
      c(d$n[-num_looks], f)
    }))
    fin$final_n <- finals[fin$schedule]
    sch_s <- lapply(interim_shifts, function(k) {
      c(d$n[-num_looks] + k, d$N)
    })
    valid <- vapply(sch_s, function(s) all(s >= 1) && all(diff(s) > 0),
                    logical(1))
    sh <- flex_compare(dl, sch_s[valid])
    rnd <- do.call(rbind, lapply(c("moderate", "large"), function(dev) {
      r <- flex_compare(dl, schedules[[paste(config$scenario[i], dev,
                                             sep = "_")]])
      r$deviation <- dev
      r
    }))
    sch_m <- schedules[[paste(config$scenario[i], "moderate", sep = "_")]]
    recal <- vapply(or_i$schedule, function(j) {
      fin_j <- or_i$oracle_final[or_i$schedule == j]
      if (is.na(fin_j)) return(NA_real_)
      s <- sch_m[[j]]
      nv <- flex_naive(dl, s, final = TRUE)$bounds
      k <- nrow(nv)
      eff <- nv$efficacy
      fut <- nv$futility
      eff[k] <- fin_j
      fut[k] <- eff[k] - 1L
      flex_oc(s, fut, eff, d$p0, binding = d$binding)$reject
    }, numeric(1))
    orc <- data.frame(schedule = or_i$schedule,
                      oracle_power = or_i$oracle_power, recal_type1 = recal)
  }
  sm <- sens_summary(dl, exh, fin, sh, rnd, orc, ce)
  if (id == example_id) {
    nv <- flex_naive(dl, example_schedule, final = TRUE)$bounds
    sm$example.naive.type1 <- flex_oc(nv$m, nv$futility, nv$efficacy, d$p0,
                                      binding = d$binding)$reject
    sm$example.naive.efficacy_final <- nv$efficacy[nrow(nv)]
  }
  sens[[i]] <- data.frame(
    id = id, n_points = nrow(eq), n_lambda = length(unique(eq$lambda)),
    lambda_min = min(eq$lambda), lambda = top$lambda, gamma = top$gamma,
    eta = top$eta, same = same, sm, check.names = FALSE,
    stringsAsFactors = FALSE)
  message("  ", id, if (same) " (reused)" else "", " done (", elapsed(), ")")
}
sens_cols <- unique(unlist(lapply(sens, names)))
sens <- do.call(rbind, lapply(sens, function(x) {
  x[setdiff(sens_cols, names(x))] <- NA_real_
  x[sens_cols]
}))
rownames(sens) <- NULL
for (r in seq_len(nrow(sens))) {
  for (v in setdiff(sens_cols, "id")) {
    if (is.na(sens[[v]][r]) && v != "eta") next
    add_number(paste("sensitivity", sens$id[r], v, sep = "."), sens[[v]][r],
               paste("Grid point of the largest lambda:", v))
  }
}
bind <- grepl("_binding$", sens$id)
add_number("overall.sensitivity.n_multi_lambda", sum(sens$n_lambda > 1),
           "Designs whose equivalent grid points contain several lambdas")
add_number("overall.sensitivity.n_designs", nrow(sens),
           "Designs in the analysis of equivalent grid points")
for (v in c("exhaustive.n", "exhaustive.naive.n_exceed",
            "exhaustive.proposed.n_exceed", "final.n",
            "final.naive.n_exceed", "final.proposed.n_exceed",
            "shift.proposed.n_exceed", "random.moderate.proposed.n_exceed",
            "random.large.proposed.n_exceed")) {
  add_number(paste0("overall.sensitivity.", v), sum(sens[[v]]),
             paste("Grid point of the largest lambda, all designs:", v))
}
add_number("overall.sensitivity.shift.binding.n", sum(sens$shift.n[bind]),
           "Grid point of the largest lambda: interim shifts, binding")
add_number("overall.sensitivity.shift.binding.naive.n_exceed",
           sum(sens$shift.naive.n_exceed[bind]),
           "Grid point of the largest lambda: interim shifts above alpha")
add_number("overall.sensitivity.exhaustive.naive.type1_max",
           max(sens$exhaustive.naive.type1_max),
           "Grid point of the largest lambda: largest naive type I error")
add_number("overall.sensitivity.final.naive.type1_max",
           max(sens$final.naive.type1_max),
           "Grid point of the largest lambda: largest naive type I error")
for (dev in c("moderate", "large")) {
  key <- paste0("random.", dev)
  pct <- sens[[paste0(key, ".naive.exceed_pct")]]
  add_number(paste0("overall.sensitivity.", key, ".naive.exceed_pct_min"),
             min(pct), "Smallest percentage of schedules above alpha, naive")
  add_number(paste0("overall.sensitivity.", key, ".naive.exceed_pct_max"),
             max(pct), "Largest percentage of schedules above alpha, naive")
  add_number(paste0("overall.sensitivity.", key, ".naive.n_above_half"),
             sum(pct > 50), "Designs with more than half above alpha, naive")
  add_number(paste0("overall.sensitivity.", key, ".naive.type1_max"),
             max(sens[[paste0(key, ".naive.type1_max")]]),
             "Largest naive type I error rate over designs")
  add_number(paste0("overall.sensitivity.", key, ".proposed.type1_max_ratio"),
             max(sens[[paste0(key, ".proposed.type1_max")]] /
                   vapply(planned, function(x) x$alpha, numeric(1))),
             "Largest proposed type I error rate divided by alpha")
}
for (grp in c("all", "binding")) {
  sel <- if (grp == "all") rep(TRUE, nrow(sens)) else bind
  add_number(paste0("overall.sensitivity.oracle.", grp, ".diff_mean_min"),
             min(sens$oracle.proposed.diff_mean[sel]),
             "Smallest mean power difference, proposed minus oracle")
  add_number(paste0("overall.sensitivity.oracle.", grp, ".diff_mean_max"),
             max(sens$oracle.proposed.diff_mean[sel]),
             "Largest mean power difference, proposed minus oracle")
}
add_number("overall.sensitivity.recalibrated.binding.type1_max",
           max(sens$recalibrated.type1_max[bind]),
           "Largest type I error rate of the recalibrated final boundary")
add_number("overall.sensitivity.recalibrated.binding.exceed_pct_min",
           min(sens$recalibrated.exceed_pct[bind]),
           "Smallest percentage above alpha, recalibrated final boundary")
add_number("overall.sensitivity.recalibrated.binding.exceed_pct_max",
           max(sens$recalibrated.exceed_pct[bind]),
           "Largest percentage above alpha, recalibrated final boundary")
for (dgn in designs$design) {
  x <- sens[bind & config$design == dgn, ]
  key <- paste("overall.sensitivity.ce_vs_proposed", dgn, sep = ".")
  for (v in c("n", "n_higher", "n_equal", "n_lower")) {
    add_number(paste(key, v, sep = "."), sum(x[[paste0("ce_vs_proposed.", v)]]),
               "Grid point of the largest lambda: proposed against CE")
  }
  add_number(paste0(key, ".diff_min"), min(x$ce_vs_proposed.diff_min),
             "Smallest power difference, proposed minus conditional error")
  add_number(paste0(key, ".diff_max"), max(x$ce_vs_proposed.diff_max),
             "Largest power difference, proposed minus conditional error")
}

# Grid points of the TREND setting
tr_eq <- tr_design$calibration$equivalent
tr_eq_res <- do.call(rbind, lapply(seq_len(nrow(tr_eq)), function(r) {
  x <- flex_plan(p0 = trend$p0, n = trend$n, alpha = trend$alpha,
                 lambda = tr_eq$lambda[r], gamma = tr_eq$gamma[r],
                 eta = tr_eq$eta[r], p1 = trend$p1)
  if (!isTRUE(all.equal(x$boundary, tr_design$boundary))) {
    stop("An equivalent grid point of the TREND setting differs.")
  }
  out <- data.frame(lambda = tr_eq$lambda[r], gamma = tr_eq$gamma[r],
                    eta = tr_eq$eta[r])
  for (s in trend$schedules) {
    lab <- paste(s, collapse = "_")
    nv <- flex_naive(x, s, final = TRUE)$bounds
    pr <- flex_update(x, s, final = TRUE)$bounds
    out[[paste0("naive_futility_", lab)]] <- nv$futility[1]
    out[[paste0("naive_efficacy_", lab)]] <- nv$efficacy[1]
    out[[paste0("naive_type1_", lab)]] <-
      flex_oc(nv$m, nv$futility, nv$efficacy, trend$p0)$reject
    out[[paste0("proposed_futility_", lab)]] <- pr$futility[1]
    out[[paste0("proposed_efficacy_", lab)]] <- pr$efficacy[1]
  }
  out
}))
add_number("trend.equivalent.n_points", nrow(tr_eq_res),
           "TREND setting: grid points giving the planned boundaries")
add_number("trend.equivalent.n_lambda", length(unique(tr_eq_res$lambda)),
           "TREND setting: distinct lambdas among these grid points")
prop_key <- do.call(paste, tr_eq_res[grepl("^proposed_", names(tr_eq_res))])
add_number("trend.equivalent.n_proposed_distinct", length(unique(prop_key)),
           "TREND setting: distinct proposed interim boundaries")
for (s in trend$schedules) {
  lab <- paste(s, collapse = "_")
  add_number(paste0("trend.equivalent.", lab, ".n_naive_exceed"),
             sum(tr_eq_res[[paste0("naive_type1_", lab)]] >
                   trend$alpha * (1 + 1e-9)),
             "TREND setting: grid points whose naive boundaries exceed alpha")
}
same_lg <- abs(tr_eq_res$lambda - tr_design$lambda) < 1e-9 &
  abs(tr_eq_res$gamma - tr_design$gamma) < 1e-9
ok_65 <- tr_eq_res$naive_type1_44_65 <= trend$alpha * (1 + 1e-9)
eta_ok <- min(tr_eq_res$eta[same_lg & ok_65])
add_number("trend.equivalent.same_lambda_gamma.eta_min_ok", eta_ok,
           paste("TREND setting: smallest eta with the reported lambda and",
                 "gamma whose naive boundaries do not exceed alpha"))
add_number("trend.equivalent.same_lambda_gamma.eta_max",
           max(tr_eq_res$eta[same_lg]),
           "TREND setting: largest eta with the reported lambda and gamma")
add_number("trend.equivalent.same_lambda_gamma.all_ok_above",
           all(ok_65[same_lg & tr_eq_res$eta >= eta_ok - 1e-9]),
           "TREND setting: all larger eta keep the naive type I error")
r_ok <- which(same_lg & abs(tr_eq_res$eta - eta_ok) < 1e-9)
for (s in trend$schedules) {
  lab <- paste(s, collapse = "_")
  for (v in c("naive_futility_", "naive_efficacy_", "naive_type1_")) {
    add_number(paste0("trend.equivalent.eta_min_ok.", lab, ".",
                      sub("_$", "", v)),
               tr_eq_res[[paste0(v, lab)]][r_ok],
               "TREND setting: naive boundaries at the smallest such eta")
  }
}
saveRDS(list(summary = sens, trend = tr_eq_res),
        file.path(out_dir, "equivalent_grid_points.rds"))

# G. Naive boundaries over all grid points that give the planned boundaries
# The naive boundaries evaluate the futility and efficacy cutoffs at the
# actual sample sizes, so they depend on gamma and eta as well as on lambda.
# For the exhaustive schedules of B, the number of schedules with a naive type
# I error rate above alpha is counted for every grid point of each design.
message("G. Naive boundaries over all equivalent grid points (", elapsed(),
        ")")
# Smallest count whose posterior probability reaches each cutoff (m + 1 if
# none, or if the cutoff is NA), from the posterior probabilities post of 0,
# ..., m responses, which increase with the count. This is the rule of the
# efficacy boundary in flex_boundary(); the futility boundary is one less.
count_at_least <- function(post, cutoff) {
  out <- findInterval(cutoff, post, left.open = TRUE)
  out[is.na(cutoff)] <- length(post)
  as.integer(out)
}
# Number of schedules in sch on which the naive boundaries of each grid point
# in eq (rows) have a type I error rate above alpha
naive_exceed_all <- function(d, eq, sch) {
  a <- d$prior[["a"]]
  b <- d$prior[["b"]]
  eta <- if (is.null(eq$eta)) rep(NA_real_, nrow(eq)) else eq$eta
  cnt <- integer(nrow(eq))
  for (s in sch) {
    num <- length(s)
    fut <- matrix(0L, nrow(eq), num)
    eff <- matrix(0L, nrow(eq), num)
    for (k in seq_len(num)) {
      post <- flex_posterior(0:s[k], s[k], d$p0, a, b)
      if (k == num) {
        eff[, k] <- count_at_least(post, eq$lambda)
        fut[, k] <- eff[, k] - 1L
      } else {
        t <- min(s[k] / d$N, 1)
        cut_eff <- if (d$type == "BOP2") {
          rep(NA_real_, nrow(eq))
        } else if (d$method == "power") {
          1 - (1 - eq$lambda) * t^eta
        } else {
          2 * stats::pnorm(stats::qnorm((1 + eq$lambda) / 2) / sqrt(t)) - 1
        }
        eff[, k] <- count_at_least(post, cut_eff)
        fut[, k] <- pmin(count_at_least(post, eq$lambda * t^eq$gamma) - 1L,
                         eff[, k] - 1L)
      }
    }
    key <- do.call(paste, c(as.data.frame(cbind(fut, eff)), sep = ","))
    first <- which(!duplicated(key))
    t1 <- vapply(first, function(r) {
      flex_oc(s, fut[r, ], eff[r, ], d$p0, binding = d$binding)$reject
    }, numeric(1))
    cnt <- cnt + (t1[match(key, key[first])] > d$alpha * (1 + 1e-9))
  }
  cnt
}
all_points <- list()
for (i in seq_len(nrow(config))) {
  id <- config$id[i]
  d <- planned[[i]]
  eq <- eq_list[[id]]
  ex <- exhaustive[exhaustive$scenario == config$scenario[i], ]
  sch <- enum_schedules(d$n, ex$interim_shift, ex$final_shift)
  cnt <- naive_exceed_all(d, eq, sch)
  # The reported grid point (first row) and the grid point of the largest
  # lambda must give the counts obtained with flex_compare() in B and F
  top_row <- which.max(eq$lambda)
  if (cnt[1] != sum(exh_res$exceeds_alpha[exh_res$id == id &
                                            exh_res$method == "naive"]) ||
      cnt[top_row] != sens$exhaustive.naive.n_exceed[sens$id == id]) {
    stop("The naive counts over the grid points of ", id,
         " do not agree with B and F.")
  }
  all_points[[id]] <- data.frame(eq, n_exceed = cnt)
  add_number(paste("allpoints", id, "exhaustive.n", sep = "."), length(sch),
             "All equivalent grid points: exhaustive schedules")
  add_number(paste("allpoints", id, "exhaustive.naive.n_exceed_min",
                   sep = "."), min(cnt),
             "All equivalent grid points: fewest schedules above alpha, naive")
  add_number(paste("allpoints", id, "exhaustive.naive.n_exceed_max",
                   sep = "."), max(cnt),
             "All equivalent grid points: most schedules above alpha, naive")
  add_number(paste("allpoints", id, "exhaustive.naive.exceed_pct_min",
                   sep = "."), 100 * min(cnt) / length(sch),
             "All equivalent grid points: smallest percentage above alpha")
  add_number(paste("allpoints", id, "exhaustive.naive.exceed_pct_max",
                   sep = "."), 100 * max(cnt) / length(sch),
             "All equivalent grid points: largest percentage above alpha")
  message("  ", id, ": ", nrow(eq), " grid points (", elapsed(), ")")
}
ap_min <- vapply(all_points, function(x) min(x$n_exceed), numeric(1))
ap_max <- vapply(all_points, function(x) max(x$n_exceed), numeric(1))
single <- vapply(all_points, function(x) length(unique(x$lambda)) == 1,
                 logical(1))
add_number("overall.allpoints.exhaustive.naive.n_exceed_min_sum", sum(ap_min),
           "All equivalent grid points: sum over designs of the fewest")
add_number("overall.allpoints.exhaustive.naive.n_exceed_max_sum", sum(ap_max),
           "All equivalent grid points: sum over designs of the most")
add_number("overall.allpoints.n_single_lambda_varying",
           sum(single & ap_max > ap_min),
           "Designs with a single lambda whose naive counts still vary")
saveRDS(all_points, file.path(out_dir, "all_grid_points.rds"))

# H. Designs specified by their boundary table
# For a design given only by its boundary table, flex_update() replaces the
# cutoffs in the floor (C_E(t) at an interim analysis of a BOP2-FE design and
# lambda at the final analysis) by the smallest posterior probability at the
# planned efficacy boundaries that do not exceed the planned sample sizes. A
# BOP2 table does not stop for efficacy at an interim analysis. For the
# calibrated designs and the TREND design, this floor is compared with the
# lambda of every grid point that gives the table, and the table design is
# checked to give the planned boundaries on the planned schedule.
message("H. Designs given by their boundary table (", elapsed(), ")")
table_check <- function(d, lambda_all) {
  e <- d$boundary$efficacy
  reach <- which(e <= d$n)
  floor_cut <- min(vapply(reach, function(j) {
    flex_posterior(e[j], d$n[j], d$p0, d$prior[["a"]], d$prior[["b"]])
  }, numeric(1)))
  tab <- flex_plan(p0 = d$p0, n = d$n, alpha = d$alpha,
                   futility = d$boundary$futility, efficacy = e, p1 = d$p1,
                   binding = d$binding)
  b <- flex_update(tab, d$n, final = TRUE)$bounds
  c(floor = floor_cut, lambda_max = max(lambda_all),
    reproduces = identical(b$futility, d$boundary$futility) &&
      identical(b$efficacy, d$boundary$efficacy))
}
table_res <- lapply(seq_len(nrow(config)), function(i) {
  table_check(planned[[i]], eq_list[[config$id[i]]]$lambda)
})
names(table_res) <- config$id
table_res[["trend"]] <- table_check(tr_design, tr_eq$lambda)
for (id in names(table_res)) {
  for (v in names(table_res[[id]])) {
    add_number(paste("table", id, v, sep = "."), table_res[[id]][[v]],
               paste("Design given by its boundary table:", v))
  }
}
tr_des <- do.call(rbind, table_res[config$id])
add_number("overall.table.n_designs", nrow(tr_des),
           "Designs given by their boundary table")
add_number("overall.table.n_floor_above_lambda_max",
           sum(tr_des[, "floor"] > tr_des[, "lambda_max"]),
           "Designs whose table floor exceeds every equivalent lambda")
add_number("overall.table.n_reproduces", sum(tr_des[, "reproduces"]),
           "Designs whose table gives the planned boundaries")

numbers <- do.call(rbind, numbers)
utils::write.csv(numbers, file.path(out_dir, "numbers.csv"),
                 row.names = FALSE)
saveRDS(numbers, file.path(out_dir, "numbers.rds"))
writeLines(c(paste("Finished:", format(Sys.time())),
             paste("Elapsed:", elapsed()),
             utils::capture.output(utils::sessionInfo())),
           file.path(out_dir, "session_info.txt"))
message("Done (", elapsed(), ")")
