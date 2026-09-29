# Supplementary tables and figures for the article on sample size deviations
# in the BOP2 and BOP2-FE designs.
#
# Run from the package root after both data generation scripts:
#   source(file.path("inst", "supplementary", "tables_and_figures.R"))
#
# Tables are written to inst/supplementary/output/tables as complete LaTeX
# table environments (booktabs and rotating); figures are written to
# inst/supplementary/output/figures as PDF and EPS files. The numbering S1,
# S2, ... is expected to come from the supplementary document, for example
# with \renewcommand{\thetable}{S\arabic{table}}.

library(ggplot2)

main_dir <- file.path("inst", "scripts", "output", "data")
data_dir <- file.path("inst", "supplementary", "output", "data")
tab_dir <- file.path("inst", "supplementary", "output", "tables")
fig_dir <- file.path("inst", "supplementary", "output", "figures")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(tab_dir, full.names = TRUE))
unlink(list.files(fig_dir, full.names = TRUE))

pl <- readRDS(file.path(main_dir, "planned_designs.rds"))
random <- readRDS(file.path(main_dir, "random_results.rds"))
oracle <- readRDS(file.path(main_dir, "oracle_results.rds"))
fl <- readRDS(file.path(data_dir, "efficacy_floor.rds"))
ex <- readRDS(file.path(data_dir, "exhaustive.rds"))
sp <- readRDS(file.path(data_dir, "spending_functions.rds"))
sh <- readRDS(file.path(data_dir, "interim_shift.rds"))
tr <- readRDS(file.path(data_dir, "trend_setting.rds"))
numbers <- readRDS(file.path(data_dir, "numbers.rds"))
num <- function(key) {
  v <- numbers$value[numbers$key == key]
  if (length(v) != 1) stop("Number not found: ", key)
  v
}

scenarios <- pl$scenarios
ps <- pl$planned_summary
design_levels <- pl$designs$design
parse_looks <- function(x) as.integer(strsplit(x, ",")[[1]])
fmt <- function(x, digits) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}
looks_tex <- function(looks) {
  n <- parse_looks(looks)
  if (length(n) > 3 && length(unique(diff(n))) == 1) {
    sprintf("$%d, %d, \\ldots, %d$", n[1], n[2], n[length(n)])
  } else {
    paste0("$", paste(n, collapse = ", "), "$")
  }
}
bound_tex <- function(b, looks, efficacy) {
  b <- parse_looks(b)
  n <- parse_looks(looks)
  out <- as.character(b)
  if (efficacy) {
    out[seq_along(b) < length(b) & b == n + 1L] <- "--"
  }
  paste(out, collapse = ", ")
}
table_env <- function(caption, label, colspec, header, rows, note) {
  c("\\begin{sidewaystable}", "\\centering",
    paste0("\\caption{", caption, "}"), paste0("\\label{", label, "}"),
    "\\scriptsize", "\\setlength{\\tabcolsep}{3pt}",
    paste0("\\begin{tabular}{", colspec, "}"), "\\toprule", header,
    "\\midrule", rows, "\\bottomrule", "\\end{tabular}", "\\par\\medskip",
    "\\begin{minipage}{0.95\\textwidth}", "\\footnotesize", note,
    "\\end{minipage}", "\\end{sidewaystable}")
}
row_tex <- function(...) paste0(paste(..., sep = " & "), " \\\\")

# Table S1: planned designs
rows <- character(0)
for (s in scenarios$scenario) {
  x <- ps[ps$scenario == s, ]
  if (length(rows) > 0) rows <- c(rows, "\\midrule")
  for (j in seq_len(nrow(x))) {
    r <- x[j, ]
    first <- j == 1
    rows <- c(rows, row_tex(
      if (first) r$scenario else "", if (first) fmt(r$p0, 2) else "",
      if (first) fmt(r$p1, 2) else "", if (first) fmt(r$alpha, 2) else "",
      if (first) looks_tex(r$looks) else "", r$design, r$rule,
      fmt(r$lambda, 2), fmt(r$gamma, 2), fmt(r$eta, 1),
      bound_tex(r$futility, r$looks, FALSE),
      bound_tex(r$efficacy, r$looks, TRUE), fmt(r$type1, 3),
      fmt(r$power, 3), fmt(r$en0, 1)))
  }
}
writeLines(table_env(
  "Planned BOP2 and BOP2-FE designs selected by the exact grid search",
  "tab:planned", "lllllllrrrllrrr",
  paste("Setting & $p_0$ & $p_1$ & $\\alpha$ & Looks & Design & Futility &",
        "$\\lambda$ & $\\gamma$ & $\\eta$ & Futility boundaries &",
        "Efficacy boundaries & Type I error & Power & $E_0(N)$ \\\\"),
  rows,
  paste("Looks are the planned cumulative sample sizes, the last being the",
        "maximum sample size $N$. The tuning parameters maximize the power",
        "subject to a type I error rate of at most $\\alpha$, with ties broken",
        "by the expected sample size under $p_0$, over $\\lambda$ from 0 to 1",
        "and $\\gamma$ from 0.01 to 1 in steps of 0.01 and $\\eta$ from 0.1",
        "to 3 in steps of 0.1. BOP2-FE-1 and BOP2-FE-2 use the power and the",
        "O'Brien-Fleming type efficacy cutoffs. At an interim analysis the",
        "trial stops for futility when the number of responses is at most",
        "the futility boundary and for efficacy when it is at least the",
        "efficacy boundary; at the final analysis the null hypothesis is",
        "rejected when the number of responses is at least the efficacy",
        "boundary. A dash means no efficacy stopping. The type I error rate",
        "is computed with the futility rule shown; the power and $E_0(N)$,",
        "the expected sample size under $p_0$, are computed with the",
        "futility boundaries applied. All values are exact.")),
  file.path(tab_dir, "tableS1_planned_designs.tex"))

# Table S2: floor on the efficacy boundary, moderate deviations
fs <- fl$summary[fl$summary$deviation == "moderate", ]
rows <- character(0)
for (s in scenarios$scenario) {
  x <- ps[ps$scenario == s, ]
  if (length(rows) > 0) rows <- c(rows, "\\midrule")
  for (j in seq_len(nrow(x))) {
    f <- fs[fs$id == x$id[j], ]
    rows <- c(rows, row_tex(
      if (j == 1) s else "", x$design[j], x$rule[j],
      round(f$changed_pct * f$n / 100), f$max_raise, f$n_final_zero,
      fmt(f$type1_floor_mean, 3), fmt(f$type1_nofloor_mean, 3),
      fmt(f$type1_floor_max, 3), fmt(f$type1_nofloor_max, 3),
      fmt(f$power_floor_mean, 3), fmt(f$power_nofloor_mean, 3)))
  }
}
writeLines(table_env(
  paste("Effect of the floor on the efficacy boundary over the random",
        "schedules with moderate deviations"),
  "tab:floor", "lllrrrrrrrrr",
  c(paste(" & & & & & & \\multicolumn{2}{c}{Type I error, mean} &",
          "\\multicolumn{2}{c}{Type I error, max} &",
          "\\multicolumn{2}{c}{Power, mean} \\\\"),
    "\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}\\cmidrule(lr){11-12}",
    paste("Setting & Design & Futility & Changed & Largest raise &",
          "Final boundary 0 & Floor & No floor & Floor & No floor & Floor &",
          "No floor \\\\")),
  rows,
  sprintf(paste(
    "The proposed boundaries were computed with and without the floor,",
    "which keeps the efficacy boundary at or above the smallest number",
    "of responses whose posterior probability reaches the planned final",
    "cutoff $\\lambda$. Changed: number of the %d schedules in which the",
    "floor raised at least one efficacy boundary. Largest raise: largest",
    "increase of an efficacy boundary, in responses. Final boundary 0:",
    "number of schedules in which, without the floor, the null hypothesis",
    "would be rejected at the final analysis with no responses. Both",
    "versions keep the type I error rate at or below the nominal level.",
    "All operating characteristics are exact."), fs$n[1])),
  file.path(tab_dir, "tableS2_efficacy_floor.tex"))

# Table S3: exhaustive enumeration
es <- ex$summary
rows <- character(0)
for (s in scenarios$scenario) {
  x <- ps[ps$scenario == s, ]
  st <- ex$settings[ex$settings$scenario == s, ]
  if (length(rows) > 0) rows <- c(rows, "\\midrule")
  for (j in seq_len(nrow(x))) {
    nv <- es[es$id == x$id[j] & es$method == "naive", ]
    pr <- es[es$id == x$id[j] & es$method == "spending", ]
    rows <- c(rows, row_tex(
      if (j == 1) s else "",
      if (j == 1) sprintf("$\\pm %d$, $\\pm %d$", st$interim_shift,
                          st$final_shift) else "",
      x$design[j], x$rule[j], nv$n, fmt(nv$type1_mean, 3),
      fmt(nv$type1_max, 3), fmt(nv$exceed_pct, 1), fmt(pr$type1_mean, 3),
      fmt(pr$type1_max, 3), fmt(pr$exceed_pct, 1)))
  }
}
writeLines(table_env(
  "Type I error rate over all schedules within a range of deviations",
  "tab:exhaustive", "llllrrrrrrr",
  c(paste(" & & & & & \\multicolumn{3}{c}{Naive} &",
          "\\multicolumn{3}{c}{Proposed} \\\\"),
    "\\cmidrule(lr){6-8}\\cmidrule(lr){9-11}",
    paste("Setting & Shifts & Design & Futility & Schedules & Mean & Max &",
          "$>\\alpha$ (\\%) & Mean & Max & $>\\alpha$ (\\%) \\\\")),
  rows,
  paste("All schedules obtained by shifting each planned interim sample size",
        "by an integer up to the first shift and the maximum sample size by",
        "an integer up to the second shift were evaluated, after merging",
        "analyses that coincide. Schedules: number of distinct schedules.",
        "Mean and Max are taken over schedules, and $>\\alpha$ is the",
        "percentage of schedules with a type I error rate above the nominal",
        "level. Settings and designs are given in Table~\\ref{tab:planned}.",
        "All operating characteristics are exact.")),
  file.path(tab_dir, "tableS3_exhaustive.tex"))

# Table S4: setting based on the TREND trial
tres <- tr$results
method_tab <- c(planned = "Planned", "planned counts" = "Planned counts",
                naive = "Naive", spending = "Proposed")
rows <- character(0)
prev <- ""
for (r in seq_len(nrow(tres))) {
  x <- tres[r, ]
  if (x$schedule != prev && length(rows) > 0) rows <- c(rows, "\\midrule")
  rows <- c(rows, row_tex(
    if (x$schedule != prev) {
      if (x$schedule == "planned") "Planned" else
        paste0("$", gsub(",", ", ", x$schedule), "$")
    } else "",
    method_tab[[x$method]], x$interim_m, x$interim_futility,
    x$interim_efficacy, x$final_m, x$final_efficacy, fmt(x$type1, 3),
    fmt(x$power, 3)))
  prev <- x$schedule
}
trd <- tr$design
writeLines(table_env(
  sprintf(paste("Boundaries and operating characteristics in a setting based",
                "on the TREND trial ($p_0 = %s$, $p_1 = %s$, planned analyses",
                "after %d and %d patients, $\\alpha = %s$)"),
          format(tr$settings$p0), format(tr$settings$p1), tr$settings$n[1],
          tr$settings$n[2], format(tr$settings$alpha)),
  "tab:trend", "llrrrrrrr",
  c(paste(" & & \\multicolumn{3}{c}{Interim analysis} &",
          "\\multicolumn{2}{c}{Final analysis} & & \\\\"),
    "\\cmidrule(lr){3-5}\\cmidrule(lr){6-7}",
    paste("Schedule & Method & Patients & Futility & Efficacy & Patients &",
          "Efficacy & Type I error & Power \\\\")),
  rows,
  sprintf(paste(
    "The planned BOP2-FE design with the power efficacy cutoff was",
    "calibrated by the exact grid search ($\\lambda = %s$, $\\gamma = %s$,",
    "$\\eta = %s$; type I error rate %s and power %s on the planned",
    "schedule). In the TREND trial, 44 of 53 enrolled patients were",
    "evaluable at the analysis; the schedules assume 44 evaluable patients",
    "at the interim analysis and 65 or 54 at the final analysis. Planned",
    "counts: the planned interim boundaries applied unchanged to the actual",
    "interim sample size, with the naive final boundary. Naive: the planned",
    "probability cutoffs evaluated at the actual sample sizes. Proposed:",
    "boundaries from the error spending function implied by the planned",
    "design. The trial stops for futility when the number of responses is at",
    "most the futility boundary and for efficacy, or rejects the null",
    "hypothesis at the final analysis, when it is at least the efficacy",
    "boundary. All operating characteristics are exact."),
    fmt(trd$lambda, 2), fmt(trd$gamma, 2), fmt(trd$eta, 1),
    fmt(trd$oc$type1, 3), fmt(trd$oc$power, 3))),
  file.path(tab_dir, "tableS4_trend_setting.tex"))

# Common settings of the figures
cols <- c(Naive = "#D55E00", Proposed = "#0072B2")
theme_article <- theme_bw(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(size = 7),
        strip.background = element_rect(fill = "grey92"))
scen_n <- vapply(scenarios$looks, function(x) max(parse_looks(x)),
                 integer(1), USE.NAMES = FALSE)
scen_lab <- stats::setNames(
  sprintf("%s: p0 = %.2f, p1 = %.2f\nN = %d, alpha = %.2f",
          scenarios$scenario, scenarios$p0, scenarios$p1, scen_n,
          scenarios$alpha),
  scenarios$scenario)
add_factors <- function(df) {
  df$scenario_f <- factor(unname(scen_lab[df$scenario]),
                          levels = unname(scen_lab))
  df$design_f <- factor(df$design, levels = design_levels)
  df$alpha_nom <- scenarios$alpha[match(df$scenario, scenarios$scenario)]
  df
}
save_figure <- function(plot, name, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), plot, width = width,
         height = height, units = "in", device = "pdf")
  ggsave(file.path(fig_dir, paste0(name, ".eps")), plot, width = width,
         height = height, units = "in", dpi = 800, device = "eps")
}

# Figure S1: spending functions
sp <- add_factors(sp)
sp_lab <- c(implied = "Implied", rho1 = "alpha t", rho2 = "alpha t^2",
            rho3 = "alpha t^3")
sp$spending_f <- factor(unname(sp_lab[sp$spending]), levels = unname(sp_lab))
sp_long <- rbind(
  data.frame(sp[, c("scenario_f", "design_f", "spending_f")],
             power = sp$planned_power, schedule = "Planned schedule"),
  data.frame(sp[, c("scenario_f", "design_f", "spending_f")],
             power = sp$power_mean,
             schedule = "Random schedules, moderate deviations (mean)"))
planned_power <- data.frame(
  sp[sp$spending == "implied", c("scenario_f", "design_f")],
  power = sp$planned_power[sp$spending == "implied"])
ps1 <- ggplot(sp_long, aes(x = spending_f, y = power, shape = schedule)) +
  geom_hline(data = planned_power, aes(yintercept = power),
             linetype = "dotted", colour = "grey40") +
  geom_point(size = 1.8) +
  facet_grid(design_f ~ scenario_f) +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "Spending function", y = "Power", shape = NULL) +
  theme_article +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(ps1, "figureS1_spending_functions", width = 10, height = 6.2)

# Figure S2: all interim analyses shifted by the same number of patients
sb <- add_factors(sh[sh$rule == "binding", ])
sb$method_f <- factor(ifelse(sb$method == "naive", "Naive", "Proposed"),
                      levels = c("Naive", "Proposed"))
ps2 <- ggplot(sb, aes(x = shift, y = type1 / alpha_nom, colour = method_f,
                      shape = method_f)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(design_f ~ scenario_f) +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = c(Naive = 17, Proposed = 15)) +
  labs(x = "Shift of every interim sample size (final analysis as planned)",
       y = "Type I error rate / nominal level", colour = NULL,
       shape = NULL) +
  theme_article
save_figure(ps2, "figureS2_interim_shift", width = 10, height = 6.2)

# Figure S3: power of the proposed boundaries relative to the oracle design
# over the random schedules with moderate deviations
po <- merge(random[random$rule == "binding" &
                     random$deviation == "moderate" &
                     random$method == "spending", ],
            oracle, by = c("id", "schedule"))
po$diff <- po$power - po$oracle_power
po <- add_factors(po[!is.na(po$diff), ])
ps3 <- ggplot(po, aes(x = design_f, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_boxplot(outlier.size = 0.3, width = 0.6, linewidth = 0.3,
               fill = cols[["Proposed"]]) +
  facet_grid(. ~ scenario_f) +
  labs(x = NULL, y = "Power minus power of the oracle design") +
  theme_article +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_figure(ps3, "figureS3_power_oracle_random", width = 10, height = 3.6)

message("Tables written to ", tab_dir)
message("Figures written to ", fig_dir)
