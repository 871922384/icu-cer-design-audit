required_formal_columns <- c(
  "trial_id",
  "publication_doi",
  "p0_assumed",
  "p1_assumed",
  "n0_observed",
  "x0_observed",
  "negative_primary",
  "design_recalculation_status",
  "cer_error_pp",
  "counterfactual_mde_scale",
  "counterfactual_mde_value",
  "mde_to_assumed_effect_ratio",
  "control_event_rate_compromised"
)

read_formal_extraction <- function(path) {
  formal <- read.csv(
    path,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  missing_columns <- setdiff(required_formal_columns, names(formal))
  if (length(missing_columns) > 0L) {
    stop(sprintf("missing required columns: %s", paste(missing_columns, collapse = ", ")))
  }
  if (nrow(formal) != 100L) stop("formal extraction must contain exactly 100 trials")
  if (anyDuplicated(formal$trial_id)) stop("trial_id values must be unique")
  formal
}

summarize_formal_analysis <- function(formal) {
  cer <- formal$cer_error_pp[!is.na(formal$cer_error_pp)]
  mde_ratio <- formal$mde_to_assumed_effect_ratio[
    !is.na(formal$mde_to_assumed_effect_ratio)
  ]
  negative_mde <- formal$negative_primary == "yes" &
    !is.na(formal$mde_to_assumed_effect_ratio)
  compromised <- formal$control_event_rate_compromised == "yes"

  list(
    formal_trials = nrow(formal),
    reproduced = sum(formal$design_recalculation_status == "reproduced"),
    partial = sum(formal$design_recalculation_status == "partial"),
    failed = sum(formal$design_recalculation_status == "failed"),
    cer_pairable = length(cer),
    cer_overestimated = sum(cer > 0),
    cer_underestimated = sum(cer < 0),
    cer_equal = sum(cer == 0),
    cer_error_median_pp = median(cer),
    cer_error_q1_pp = unname(quantile(cer, 0.25, type = 7)),
    cer_error_q3_pp = unname(quantile(cer, 0.75, type = 7)),
    cer_error_min_pp = min(cer),
    cer_error_max_pp = max(cer),
    mde_estimable = length(mde_ratio),
    mde_ratio_median = median(mde_ratio),
    mde_ratio_q1 = unname(quantile(mde_ratio, 0.25, type = 7)),
    mde_ratio_q3 = unname(quantile(mde_ratio, 0.75, type = 7)),
    mde_ratio_min = min(mde_ratio),
    mde_ratio_max = max(mde_ratio),
    negative_primary = sum(formal$negative_primary == "yes", na.rm = TRUE),
    negative_primary_mde_classifiable = sum(negative_mde, na.rm = TRUE),
    control_event_rate_compromised_yes = sum(compromised, na.rm = TRUE),
    compromised_fraction = sum(compromised, na.rm = TRUE) /
      sum(negative_mde, na.rm = TRUE)
  )
}

compromised_trial_table <- function(formal) {
  keep <- formal$control_event_rate_compromised == "yes"
  formal[keep, c(
    "trial_id",
    "publication_doi",
    "negative_primary",
    "p0_assumed",
    "cer_error_pp",
    "counterfactual_mde_scale",
    "counterfactual_mde_value",
    "mde_to_assumed_effect_ratio"
  )]
}

summary_table <- function(summary) {
  data.frame(
    section = c(
      rep("Analysis set", 4),
      rep("CER assumption error", 8),
      rep("Counterfactual MDE", 5),
      rep("Negative primary results", 4)
    ),
    measure = c(
      "Formal eligible trials",
      "Design recalculation reproduced",
      "Design recalculation partial",
      "Design recalculation failed",
      "CER-pairable trials",
      "CER overestimated",
      "CER underestimated",
      "CER equal",
      "CER signed error median (percentage points)",
      "CER signed error Q1 (percentage points)",
      "CER signed error Q3 (percentage points)",
      "CER signed error range (percentage points)",
      "MDE-estimable trials",
      "MDE/assumed effect median",
      "MDE/assumed effect Q1",
      "MDE/assumed effect Q3",
      "MDE/assumed effect range",
      "Primary result negative",
      "Negative primary result with classifiable MDE",
      "CER-compromised negative trials",
      "CER-compromised fraction among classifiable negative trials"
    ),
    value = c(
      summary$formal_trials,
      summary$reproduced,
      summary$partial,
      summary$failed,
      summary$cer_pairable,
      summary$cer_overestimated,
      summary$cer_underestimated,
      summary$cer_equal,
      sprintf("%.6f", summary$cer_error_median_pp),
      sprintf("%.6f", summary$cer_error_q1_pp),
      sprintf("%.6f", summary$cer_error_q3_pp),
      sprintf("%.6f to %.6f", summary$cer_error_min_pp, summary$cer_error_max_pp),
      summary$mde_estimable,
      sprintf("%.6f", summary$mde_ratio_median),
      sprintf("%.6f", summary$mde_ratio_q1),
      sprintf("%.6f", summary$mde_ratio_q3),
      sprintf("%.6f to %.6f", summary$mde_ratio_min, summary$mde_ratio_max),
      summary$negative_primary,
      summary$negative_primary_mde_classifiable,
      summary$control_event_rate_compromised_yes,
      sprintf("%d/%d (%.1f%%)",
        summary$control_event_rate_compromised_yes,
        summary$negative_primary_mde_classifiable,
        100 * summary$compromised_fraction
      )
    ),
    stringsAsFactors = FALSE
  )
}

publication_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E5E7EB", linewidth = 0.35),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(colour = "#4B5563", size = base_size - 1),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}

build_cer_figure <- function(formal) {
  cer <- formal[!is.na(formal$cer_error_pp), ]
  cer$p0_observed <- cer$x0_observed / cer$n0_observed
  cer$direction <- ifelse(cer$cer_error_pp > 0, "Assumed CER higher", "Assumed CER lower")
  cer$direction <- factor(
    cer$direction,
    levels = c("Assumed CER higher", "Assumed CER lower")
  )

  panel_a <- ggplot2::ggplot(
    cer,
    ggplot2::aes(x = p0_assumed, y = p0_observed, colour = direction)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#111827", linewidth = 0.55) +
    ggplot2::geom_point(size = 2.2, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c(
      "Assumed CER higher" = "#D55E00",
      "Assumed CER lower" = "#0072B2"
    )) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "A  Assumed versus observed control event rate",
      subtitle = "Points below the identity line indicate design-stage overestimation",
      x = "Assumed control event rate",
      y = "Observed control event rate"
    ) +
    publication_theme()

  cer <- cer[order(cer$cer_error_pp), ]
  cer$trial_order <- seq_len(nrow(cer))
  panel_b <- ggplot2::ggplot(
    cer,
    ggplot2::aes(x = trial_order, y = cer_error_pp, fill = direction)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#111827", linewidth = 0.5) +
    ggplot2::geom_col(width = 0.82) +
    ggplot2::scale_fill_manual(values = c(
      "Assumed CER higher" = "#D55E00",
      "Assumed CER lower" = "#0072B2"
    )) +
    ggplot2::labs(
      title = "B  Signed assumption error across 65 pairable trials",
      subtitle = "Signed error = 100 x (assumed CER - observed CER)",
      x = "Trials ordered by signed error",
      y = "Signed CER error (percentage points)"
    ) +
    publication_theme()

  gridExtra::arrangeGrob(panel_a, panel_b, ncol = 2, widths = c(1, 1.25))
}

build_mde_figure <- function(formal) {
  mde <- formal[!is.na(formal$mde_to_assumed_effect_ratio), ]
  mde <- mde[order(mde$mde_to_assumed_effect_ratio), ]
  mde$trial_label <- factor(mde$trial_id, levels = mde$trial_id)
  mde$diagnosis <- ifelse(
    mde$control_event_rate_compromised == "yes",
    "CER-compromised negative trial",
    "Other MDE-estimable trial"
  )

  ggplot2::ggplot(
    mde,
    ggplot2::aes(x = mde_to_assumed_effect_ratio, y = trial_label, colour = diagnosis)
  ) +
    ggplot2::geom_vline(xintercept = 1, colour = "#111827", linewidth = 0.55) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 1, xend = mde_to_assumed_effect_ratio, yend = trial_label),
      linewidth = 0.45,
      alpha = 0.65
    ) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_colour_manual(values = c(
      "CER-compromised negative trial" = "#C51B29",
      "Other MDE-estimable trial" = "#4B5563"
    )) +
    ggplot2::labs(
      title = "Counterfactual minimum detectable effect under the observed CER",
      subtitle = "Ratio >1 means the original assumed effect is smaller than the recalculated MDE",
      x = "MDE / original assumed effect",
      y = NULL
    ) +
    publication_theme(base_size = 9) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 6.5),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

save_plot_pair <- function(plot, stem, width, height) {
  ggplot2::ggsave(
    paste0(stem, ".png"),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  ggplot2::ggsave(
    paste0(stem, ".pdf"),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    bg = "white"
  )
}

write_formal_analysis_outputs <- function(
  formal,
  output_root = "output",
  analysis_root = "workspace/analysis"
) {
  figure_root <- file.path(output_root, "figures")
  table_root <- file.path(output_root, "tables")
  dir.create(figure_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(analysis_root, recursive = TRUE, showWarnings = FALSE)

  summary <- summarize_formal_analysis(formal)
  table_one <- summary_table(summary)
  table_two <- compromised_trial_table(formal)
  write.csv(
    table_one,
    file.path(analysis_root, "formal analysis summary.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  write.csv(
    table_one,
    file.path(table_root, "Table 1 - Formal analysis summary.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  write.csv(
    table_two,
    file.path(table_root, "Table 2 - CER-compromised negative trials.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  save_plot_pair(
    build_cer_figure(formal),
    file.path(figure_root, "Figure 1 - CER assumption error"),
    width = 11,
    height = 5.8
  )
  save_plot_pair(
    build_mde_figure(formal),
    file.path(figure_root, "Figure 2 - Counterfactual MDE ratio"),
    width = 8.5,
    height = 10.5
  )
  invisible(list(summary = summary, table_one = table_one, table_two = table_two))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  input_path <- if (length(args) >= 1L) args[[1L]] else "workspace/data/formal_extraction.csv"
  formal <- read_formal_extraction(input_path)
  write_formal_analysis_outputs(formal)
  cat("PASS: formal analysis outputs written\n")
}

if (sys.nframe() == 0L) main()
