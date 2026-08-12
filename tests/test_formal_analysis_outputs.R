source("scripts/build_formal_analysis_outputs.R")

expect_close <- function(actual, expected, tolerance = 1e-8) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(sprintf("expected %.10f, got %.10f", expected, actual))
  }
}

formal <- read_formal_extraction("workspace/data/formal_extraction.csv")
summary <- summarize_formal_analysis(formal)

stopifnot(summary$formal_trials == 100L)
stopifnot(summary$reproduced == 43L)
stopifnot(summary$partial == 45L)
stopifnot(summary$failed == 12L)
stopifnot(summary$cer_pairable == 65L)
stopifnot(summary$cer_overestimated == 47L)
stopifnot(summary$cer_underestimated == 18L)
expect_close(summary$cer_error_median_pp, 4.48717948717949)
expect_close(summary$cer_error_q1_pp, -0.533866217922)
expect_close(summary$cer_error_q3_pp, 12.951807228916)
stopifnot(summary$mde_estimable == 42L)
expect_close(summary$mde_ratio_median, 0.980554755787336)
expect_close(summary$mde_ratio_q1, 0.88725964738125)
expect_close(summary$mde_ratio_q3, 1.04724558647275)
stopifnot(summary$negative_primary == 69L)
stopifnot(summary$negative_primary_mde_classifiable == 31L)
stopifnot(summary$control_event_rate_compromised_yes == 8L)
expect_close(summary$compromised_fraction, 8 / 31)

compromised <- compromised_trial_table(formal)
stopifnot(
  identical(
    compromised$trial_id,
    c(
      "BICARICU2_NCT04010630",
      "NCT03477344",
      "CLASSIC-NCT03668236",
      "REST-NCT02654327",
      "PROFLO_ISRCTN54917435",
      "NONSEDA_NCT01967680",
      "PMID30772908",
      "PMID29089038"
    )
  )
)
stopifnot(all(compromised$negative_primary == "yes"))
stopifnot(all(compromised$cer_error_pp > 0))
stopifnot(all(compromised$mde_to_assumed_effect_ratio > 1))

temp_root <- tempfile("formal-analysis-")
dir.create(temp_root, recursive = TRUE)
write_formal_analysis_outputs(
  formal,
  output_root = temp_root,
  analysis_root = file.path(temp_root, "analysis")
)
expected_files <- c(
  file.path(temp_root, "analysis", "formal analysis summary.csv"),
  file.path(temp_root, "tables", "Table 1 - Formal analysis summary.csv"),
  file.path(temp_root, "tables", "Table 2 - CER-compromised negative trials.csv"),
  file.path(temp_root, "figures", "Figure 1 - CER assumption error.png"),
  file.path(temp_root, "figures", "Figure 1 - CER assumption error.pdf"),
  file.path(temp_root, "figures", "Figure 2 - Counterfactual MDE ratio.png"),
  file.path(temp_root, "figures", "Figure 2 - Counterfactual MDE ratio.pdf")
)
stopifnot(all(file.exists(expected_files)))
stopifnot(all(file.info(expected_files)$size > 0))

cat("PASS: formal analysis output contract\n")
