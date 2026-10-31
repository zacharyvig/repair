# Presentation-only summaries of frozen studies; no fitting or simulation.
# Called from the adjacent Quarto document, with the working directory dev/.
library(ggplot2)
profiles <- c("low_balanced", "low_negative", "small_short", "low_larger",
              "high_control", "low_unequal")
profile_labels <- c(low_balanced = "Low, balanced", low_negative = "Negative correlation",
                    small_short = "Small, short", low_larger = "Low, larger N",
                    high_control = "High reliability", low_unequal = "Unequal information")
old <- read.csv("influence-simulation/performance.csv", stringsAsFactors = FALSE)
fresh <- read.csv("leverage-simulation/performance.csv", stringsAsFactors = FALSE)
intervals <- readRDS("leverage-simulation/dataset-intervals.rds")
old_intervals <- readRDS("influence-simulation/dataset-intervals.rds")
out <- "repair-uncertainty-methods"
theme_methods <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 10), plot.caption = element_text(hjust = 0, size = 9),
        panel.spacing = grid::unit(1.2, "lines"), legend.position = "none")
save_methods <- function(p, name, height = 8.5) {
  ggsave(file.path(out, paste0(name, ".png")), p, width = 13, height = height, dpi = 180)
  ggsave(file.path(out, paste0(name, ".pdf")), p, width = 13, height = height)
}
coverage_methods <- function(d, labels, title, subtitle, name) {
  d <- d[d$method %in% names(labels), ]
  stopifnot(nrow(d) == 6 * length(labels), all(d$coverage_mc_low >= .5),
            all(d$coverage_mc_high <= 1), !anyNA(d$coverage))
  d$profile <- factor(d$profile, levels = profiles)
  d$method <- factor(d$method, levels = rev(names(labels)))
  write.csv(d, file.path(out, paste0(name, ".csv")), row.names = FALSE)
  p <- ggplot(d, aes(coverage, method, color = method)) +
    geom_vline(xintercept = .95, linetype = 2, color = "#555555") +
    geom_segment(aes(x = coverage_mc_low, xend = coverage_mc_high, yend = method), linewidth = .8) +
    geom_point(size = 2.6) +
    geom_text(aes(label = paste0("n=", available)), nudge_y = .27, hjust = 1,
              color = "#555555", size = 2.7) +
    facet_wrap(~profile, ncol = 3, labeller = labeller(profile = profile_labels)) +
    scale_y_discrete(labels = labels, expand = expansion(add = c(.45, .7))) +
    scale_color_manual(values = setNames(c("#355F8D", "#64856B", "#8956A0", "#B77435",
                                          "#187B65", "#B14F55", "#5F6871")[seq_along(labels)],
                                        names(labels))) +
    scale_x_continuous(limits = c(.5, 1.015), breaks = seq(.5, 1, .1),
                       labels = scales::label_percent(accuracy = 1)) +
    labs(title = title, subtitle = subtitle, x = "Coverage (displayed range: 50–100%)", y = NULL,
         caption = "Dashed line: nominal 95%. Bars: 95% Wilson Monte Carlo intervals; n: returned intervals.\nAll points and Monte Carlo bounds are visible. Eligibility varies by method; see matched comparisons in the text.") +
    theme_methods
  save_methods(p, name)
}
coverage_methods(old[old$arm == "predictor" & old$gamma == .4, ],
  c("Conditional M-MOM Wald" = "Conditional", "Stage2 influence Wald" = "Stage2 IF",
    "Full influence Wald" = "Full IF", "Frozen measurement percentile" = "Frozen percentile",
    "Full person percentile" = "Person percentile"),
  "Earlier screen: empirical resampling and measurement refitting",
  "Noisy predictors; truth = 0.40. Reused study inputs, separate from the fresh leverage experiment.",
  "earlier-predictor-coverage")
coverage_methods(old[old$arm == "outcome" & old$gamma == .4, ],
  c("Legacy OLS stacked HC0 (t)" = "Legacy HC0 / t", "Legacy OLS stacked HC1 (t)" = "Legacy HC1 / t",
    "Legacy OLS stacked HC2 (t)" = "Legacy HC2 / t", "Legacy OLS stacked HC3 (t)" = "Legacy HC3 / t",
    "Full influence Wald" = "REPAIR full IF / normal", "Full person percentile" = "REPAIR percentile"),
  "Earlier screen: the legacy stacked sandwich in its matching outcome model",
  "Noisy outcome, observed predictor; truth = 0.40. Unequal schedules: legacy OLS and REPAIR have different point estimates.",
  "legacy-outcome-coverage")
coverage_methods(fresh[fresh$gamma == .4, ],
  c("Conditional Wald" = "Conditional", "Stage2 IF Wald" = "Stage2 IF",
    "Frozen percentile" = "Frozen percentile", "Bootstrap-SD Wald" = "Bootstrap-SD Wald",
    "Person percentile" = "Person percentile"),
  "Fresh screen: variance estimation versus interval construction",
  "Noisy predictors; truth = 0.40. Bootstrap-SD Wald and percentile intervals use the same successful person draws.",
  "fresh-interval-construction")

# A paired, within-profile diagnostic; do not pool the two truths or studies.
shifts <- read.csv("leverage-simulation/leverage_exact_comparison.csv")
shifts <- shifts[shifts$gamma == .4, ]
shifts$transition <- ifelse(shifts$singular_deletion | shifts$floor_switch | shifts$final_branch_switch,
                           "At least one recorded transition", "No recorded transition")
shift_summary <- do.call(rbind, lapply(split(shifts, interaction(shifts$profile, shifts$transition)), function(d) {
  if (!nrow(d)) return(NULL)
  data.frame(profile = d$profile[1], transition = d$transition[1], n = nrow(d),
             median_relative_rmse = median(d$focal_relative_shift_rmse))
}))
write.csv(shift_summary, file.path(out, "deletion-transition-summary.csv"), row.names = FALSE)
shifts$profile <- factor(shifts$profile, levels = rev(profiles))
shift_summary$profile <- factor(shift_summary$profile, levels = rev(profiles))
p <- ggplot(shifts, aes(focal_relative_shift_rmse, profile)) +
  geom_point(position = position_jitter(width = 0, height = .12, seed = 20261031),
             color = "#6B7D86", alpha = .45, size = 1.2) +
  geom_point(data = shift_summary, aes(x = median_relative_rmse), color = "#A25232", size = 3) +
  geom_text(data = shift_summary, aes(x = median_relative_rmse, label = paste0("n=", n)),
            nudge_y = .29, size = 2.8) +
  facet_wrap(~transition, ncol = 2) + scale_x_log10() + scale_y_discrete(labels = profile_labels) +
  labs(title = "When does a local deletion approximation track actual refitting?",
       subtitle = "Fresh truth = 0.40; datasets with both analytic shifts and a complete exact jackknife.",
       x = "Relative focal deletion-shift RMSE (log scale; smaller is closer)", y = NULL,
       caption = "Dots: datasets; orange points: stratum medians. Denominator is the RMS exact focal deletion shift.\nTransition = any singular deleted fit, structural variance-floor change, or final correction branch change.\nThese are selected descriptive strata, not randomized comparisons of mechanisms.") + theme_methods
save_methods(p, "deletion-transitions", 6.8)

# Endpoint-based verification of all three new coverage figures, not just copies of CSV summaries.
validate_cells <- function(p, d, arm = NULL) {
  if (!is.null(arm)) d <- d[d$arm == arm, ]
  for (i in seq_len(nrow(p))) {
    cell <- d[d$profile == p$profile[i] & d$gamma == p$gamma[i] & d$method == p$method[i], ]
    usable <- cell[cell$available, ]
    n <- nrow(usable); k <- sum(usable$low <= p$gamma[i] & usable$high >= p$gamma[i])
    ci <- unname(suppressWarnings(prop.test(k, n, correct = FALSE)$conf.int))
    stopifnot(nrow(cell) == 100, n == p$available[i], abs(k/n - p$coverage[i]) < 1e-12,
              max(abs(ci - c(p$coverage_mc_low[i], p$coverage_mc_high[i]))) < 1e-10)
  }
}
validate_cells(read.csv(file.path(out, "earlier-predictor-coverage.csv")), old_intervals, "predictor")
validate_cells(read.csv(file.path(out, "legacy-outcome-coverage.csv")), old_intervals, "outcome")
validate_cells(read.csv(file.path(out, "fresh-interval-construction.csv")), intervals)
stopifnot(all(shifts$focal_relative_shift_rmse > 0), sum(shift_summary$n) == nrow(shifts))
