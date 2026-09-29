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

numbers <- do.call(rbind, numbers)
utils::write.csv(numbers, file.path(out_dir, "numbers.csv"),
                 row.names = FALSE)
saveRDS(numbers, file.path(out_dir, "numbers.rds"))
writeLines(c(paste("Finished:", format(Sys.time())),
             paste("Elapsed:", elapsed()),
             utils::capture.output(utils::sessionInfo())),
           file.path(out_dir, "session_info.txt"))
message("Done (", elapsed(), ")")
