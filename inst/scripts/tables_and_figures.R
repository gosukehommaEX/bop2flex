# Tables and figures for the article on sample size deviations in the BOP2
# and BOP2-FE designs.
#
# Run from the package root after data_generation.R:
#   source(file.path("inst", "scripts", "tables_and_figures.R"))
#
# Tables are written to inst/scripts/output/tables as complete LaTeX table
# environments with caption, label and notes; they use the LaTeX packages
# booktabs and rotating. Figures are written to inst/scripts/output/figures
# as PDF and EPS files of the same plot. The ggplot2 package is required.

library(ggplot2)

data_dir <- file.path("inst", "scripts", "output", "data")
tab_dir <- file.path("inst", "scripts", "output", "tables")
fig_dir <- file.path("inst", "scripts", "output", "figures")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

pl <- readRDS(file.path(data_dir, "planned_designs.rds"))
worked <- readRDS(file.path(data_dir, "worked_example.rds"))
random <- readRDS(file.path(data_dir, "random_results.rds"))
final_dev <- readRDS(file.path(data_dir, "final_deviation.rds"))
oracle <- readRDS(file.path(data_dir, "oracle_results.rds"))
summ <- readRDS(file.path(data_dir, "summaries.rds"))
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

# Table 1: planned designs
rows <- character(0)
for (s in scenarios$scenario) {
  x <- ps[ps$scenario == s, ]
  if (length(rows) > 0) rows <- c(rows, "\\midrule")
  for (j in seq_len(nrow(x))) {
    r <- x[j, ]
    first <- j == 1
    rows <- c(rows, paste0(
      paste(if (first) r$scenario else "",
            if (first) fmt(r$p0, 2) else "",
            if (first) fmt(r$p1, 2) else "",
            if (first) fmt(r$alpha, 2) else "",
            if (first) looks_tex(r$looks) else "",
            r$design, r$rule, fmt(r$lambda, 2), fmt(r$gamma, 2),
            fmt(r$eta, 1), bound_tex(r$futility, r$looks, FALSE),
            bound_tex(r$efficacy, r$looks, TRUE), fmt(r$type1, 3),
            fmt(r$power, 3), fmt(r$en0, 1), sep = " & "),
      " \\\\"))
  }
}
table1 <- c(
  "\\begin{sidewaystable}",
  "\\centering",
  paste("\\caption{Planned BOP2 and BOP2-FE designs selected by the exact",
        "grid search}"),
  "\\label{tab:planned}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{lllllllrrrllrrr}",
  "\\toprule",
  paste("Setting & $p_0$ & $p_1$ & $\\alpha$ & Looks & Design & Futility &",
        "$\\lambda$ & $\\gamma$ & $\\eta$ & Futility boundaries &",
        "Efficacy boundaries & Type I error & Power & $E_0(N)$ \\\\"),
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\medskip",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\footnotesize",
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
        "futility boundaries applied. All values are exact."),
  "\\end{minipage}",
  "\\end{sidewaystable}"
)
writeLines(table1, file.path(tab_dir, "table1_planned_designs.tex"))

# Table 2: random schedules with moderate deviations
rs <- summ$random[summ$random$deviation == "moderate", ]
os <- summ$oracle[summ$oracle$method == "spending", ]
n_sim <- num("settings.n_sim")
n_oracle <- num("settings.n_oracle")
rows <- character(0)
for (s in scenarios$scenario) {
  x <- ps[ps$scenario == s, ]
  if (length(rows) > 0) rows <- c(rows, "\\midrule")
  for (j in seq_len(nrow(x))) {
    id <- x$id[j]
    nv <- rs[rs$id == id & rs$method == "naive", ]
    pr <- rs[rs$id == id & rs$method == "spending", ]
    od <- os$diff_mean[os$id == id]
    rows <- c(rows, paste0(
      paste(if (j == 1) s else "", x$design[j], x$rule[j],
            fmt(nv$type1_mean, 3), fmt(nv$type1_max, 3),
            fmt(nv$exceed_pct, 1), fmt(pr$type1_mean, 3),
            fmt(pr$type1_max, 3), fmt(pr$exceed_pct, 1),
            fmt(x$power[j], 3), fmt(nv$power_mean, 3),
            fmt(pr$power_mean, 3),
            if (length(od) == 1) fmt(od, 3) else "--", sep = " & "),
      " \\\\"))
  }
}
table2 <- c(
  "\\begin{sidewaystable}",
  "\\centering",
  sprintf(paste0("\\caption{Type I error rate and power over %d random ",
                 "schedules with moderate deviations from the planned ",
                 "sample sizes}"), n_sim),
  "\\label{tab:random}",
  "\\scriptsize",
  "\\begin{tabular}{lllrrrrrrrrrr}",
  "\\toprule",
  paste(" & & & \\multicolumn{3}{c}{Type I error, naive} &",
        "\\multicolumn{3}{c}{Type I error, proposed} &",
        "\\multicolumn{4}{c}{Power} \\\\"),
  "\\cmidrule(lr){4-6}\\cmidrule(lr){7-9}\\cmidrule(lr){10-13}",
  paste("Setting & Design & Futility & Mean & Max & $>\\alpha$ (\\%) &",
        "Mean & Max & $>\\alpha$ (\\%) & Planned & Naive & Proposed &",
        "Proposed $-$ oracle \\\\"),
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\medskip",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\footnotesize",
  sprintf(paste(
    "Settings and planned designs are given in Table~\\ref{tab:planned}.",
    "Each planned interim sample size was shifted by an integer drawn",
    "uniformly from $-%d$ to $%d$ and the maximum sample size by one drawn",
    "from $-%d$ to $%d$, independently of the data; the same schedules were",
    "used for all designs of a setting. Naive: the planned probability",
    "cutoffs evaluated at the actual sample sizes. Proposed: boundaries from",
    "the error spending function implied by the planned design, with the",
    "efficacy boundary not below the planned final posterior cutoff. Mean",
    "and Max are taken over schedules, and $>\\alpha$ is the percentage of",
    "schedules with a type I error rate above the nominal level. Planned is",
    "the power of the planned design on the planned schedule; Naive and",
    "Proposed are averaged over schedules. Proposed $-$ oracle is the mean",
    "difference in power between the proposed boundaries and the design",
    "calibrated on the actual schedule as if it had been known in advance,",
    "over the first %d schedules (binding futility only). All operating",
    "characteristics are exact."),
    num("settings.moderate.interim_shift"),
    num("settings.moderate.interim_shift"),
    num("settings.moderate.final_shift"),
    num("settings.moderate.final_shift"), n_oracle),
  "\\end{minipage}",
  "\\end{sidewaystable}"
)
writeLines(table2, file.path(tab_dir, "table2_random_deviations.tex"))

# Common settings of the figures
cols <- c(Planned = "black", Naive = "#D55E00", Proposed = "#0072B2")
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
  df$method_f <- factor(ifelse(df$method == "naive", "Naive", "Proposed"),
                        levels = c("Naive", "Proposed"))
  df
}
alpha_lines <- data.frame(
  scenario_f = factor(unname(scen_lab), levels = unname(scen_lab)),
  alpha = scenarios$alpha, N = scen_n)
save_figure <- function(plot, name, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), plot, width = width,
         height = height, units = "in", device = "pdf")
  ggsave(file.path(fig_dir, paste0(name, ".eps")), plot, width = width,
         height = height, units = "in", dpi = 800, device = "eps")
}

# Figure 1: worked example
d <- worked$design
bnd <- rbind(
  data.frame(m = d$n, futility = d$boundary$futility,
             efficacy = d$boundary$efficacy, method = "Planned"),
  data.frame(m = worked$naive$bounds$m,
             futility = worked$naive$bounds$futility,
             efficacy = worked$naive$bounds$efficacy, method = "Naive"),
  data.frame(m = worked$proposed$bounds$m,
             futility = worked$proposed$bounds$futility,
             efficacy = worked$proposed$bounds$efficacy, method = "Proposed")
)
panel_a <- "(a) Boundaries on the number of responses"
panel_b <- "(b) Cumulative type I error rate"
sp <- worked$spending
x_max <- max(c(worked$schedule, d$N))
fig1 <- rbind(
  data.frame(x = bnd$m, y = bnd$efficacy, method = bnd$method,
             quantity = "Efficacy boundary", panel = panel_a, pt = TRUE),
  data.frame(x = bnd$m, y = bnd$futility, method = bnd$method,
             quantity = "Futility boundary", panel = panel_a, pt = TRUE),
  data.frame(x = c(sp$n, x_max), y = c(sp$alpha_cum, max(sp$alpha_cum)),
             method = "Planned", quantity = "Type I error rate",
             panel = panel_b, pt = c(sp$n > 0, FALSE)),
  data.frame(x = worked$naive$bounds$m, y = worked$naive$bounds$alpha_spent,
             method = "Naive", quantity = "Type I error rate",
             panel = panel_b, pt = TRUE),
  data.frame(x = worked$proposed$bounds$m,
             y = worked$proposed$bounds$alpha_spent, method = "Proposed",
             quantity = "Type I error rate", panel = panel_b, pt = TRUE)
)
fig1$method <- factor(fig1$method, levels = names(cols))
fig1$quantity <- factor(fig1$quantity, levels = c(
  "Efficacy boundary", "Futility boundary", "Type I error rate"))
fig1$panel <- factor(fig1$panel, levels = c(panel_a, panel_b))
alpha1 <- data.frame(panel = factor(panel_b, levels = c(panel_a, panel_b)),
                     y = d$alpha)
p1 <- ggplot(fig1, aes(x = x, y = y, colour = method, shape = method,
                       linetype = quantity,
                       group = interaction(method, quantity))) +
  geom_hline(data = alpha1, aes(yintercept = y), linetype = "dotted",
             colour = "grey40", inherit.aes = FALSE) +
  geom_line() +
  geom_point(data = function(df) df[df$pt, ], size = 1.6) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_colour_manual(values = cols) +
  scale_shape_manual(values = c(Planned = 16, Naive = 17, Proposed = 15)) +
  scale_linetype_manual(values = c("Efficacy boundary" = "solid",
                                   "Futility boundary" = "dashed",
                                   "Type I error rate" = "solid"),
                        breaks = c("Efficacy boundary",
                                   "Futility boundary")) +
  scale_y_continuous(breaks = function(lim) {
    if (lim[2] > 1) seq(0, ceiling(lim[2]), by = 2) else pretty(lim)
  }) +
  labs(x = "Number of patients", y = NULL, colour = NULL, shape = NULL,
       linetype = NULL) +
  theme_article
save_figure(p1, "figure1_worked_example", width = 7, height = 3.4)

# Figure 2: type I error rate over random schedules, binding futility
rb <- add_factors(random[random$rule == "binding", ])
rb$deviation_f <- factor(ifelse(rb$deviation == "moderate", "Moderate",
                                "Large"), levels = c("Moderate", "Large"))
p2 <- ggplot(rb, aes(x = deviation_f, y = type1, fill = method_f)) +
  geom_hline(data = alpha_lines, aes(yintercept = alpha),
             linetype = "dashed", colour = "grey30") +
  geom_boxplot(outlier.size = 0.3, width = 0.7, linewidth = 0.3,
               position = position_dodge(width = 0.8)) +
  facet_grid(design_f ~ scenario_f) +
  scale_fill_manual(values = cols[c("Naive", "Proposed")]) +
  labs(x = "Size of the deviations from the planned sample sizes",
       y = "Type I error rate", fill = NULL) +
  theme_article
save_figure(p2, "figure2_random_type1", width = 10, height = 6.2)

# Figure 3: deviation of the final sample size only, binding futility
fb <- add_factors(final_dev[final_dev$rule == "binding", ])
p3 <- ggplot(fb, aes(x = final_n, y = type1, colour = method_f,
                     shape = method_f)) +
  geom_hline(data = alpha_lines, aes(yintercept = alpha),
             linetype = "dashed", colour = "grey30") +
  geom_vline(data = alpha_lines, aes(xintercept = N), linetype = "dotted",
             colour = "grey50") +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(design_f ~ scenario_f, scales = "free_x") +
  scale_colour_manual(values = cols[c("Naive", "Proposed")]) +
  scale_shape_manual(values = c(Naive = 17, Proposed = 15)) +
  labs(x = "Actual final sample size (interim analyses as planned)",
       y = "Type I error rate", colour = NULL, shape = NULL) +
  theme_article
save_figure(p3, "figure3_final_deviation", width = 10, height = 6.2)

# Figure 4: power relative to the oracle design, moderate deviations
po <- merge(random[random$rule == "binding" &
                     random$deviation == "moderate", ],
            oracle, by = c("id", "schedule"))
po$diff <- po$power - po$oracle_power
po <- add_factors(po[!is.na(po$diff), ])
p4 <- ggplot(po, aes(x = method_f, y = diff, fill = method_f)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_boxplot(outlier.size = 0.3, width = 0.6, linewidth = 0.3) +
  facet_grid(design_f ~ scenario_f) +
  scale_fill_manual(values = cols[c("Naive", "Proposed")]) +
  labs(x = NULL, y = "Power minus power of the oracle design", fill = NULL) +
  theme_article +
  theme(legend.position = "none")
save_figure(p4, "figure4_power_oracle", width = 10, height = 6.2)

message("Tables written to ", tab_dir)
message("Figures written to ", fig_dir)
