# Control event rate assumption errors in adult ICU randomized trials

This repository is the versioned reproducibility archive for the study
"Control event rate assumption errors and their counterfactual power consequences in randomized trials of adult intensive care: a design audit of public records."

## Contents

- `workspace/data/formal_extraction.csv`: 100-trial structured analysis dataset.
- `scripts/build_formal_analysis_outputs.R`: frozen analysis and figure generator.
- `tests/test_formal_analysis_outputs.R`: numerical and output contract.
- `protocols/`: frozen formal study protocol.
- `notes/data-dictionary.csv`: public-field definitions.
- `output/`: derived figures and summary tables.

The public dataset omits the two source-quotation fields retained in the private audit workspace. It contains no article full text, supplementary files, registry snapshots, patient-level data, private contact information, browser state, or credentials. Source identity is represented by DOI and registry identifiers.

## Reproduce the analysis

Use R 4.3 or later with `ggplot2`, `gridExtra`, and `scales` installed:

```bash
Rscript tests/test_formal_analysis_outputs.R
Rscript scripts/build_formal_analysis_outputs.R workspace/data/formal_extraction.csv
```

The contract checks the frozen denominators and results, including 100 eligible trials, 65 CER-pairable trials, 42 MDE-estimable trials, and 8 of 31 classifiable negative trials meeting the prespecified design-power diagnostic.

## Interpretation boundary

The `control_event_rate_compromised` flag is a design-power diagnostic. It does not establish that a treatment truly worked, that a beneficial effect was missed, or that improved care caused a decline in the control event rate.

## Licenses

- Analysis code: MIT License, see `LICENSE-CODE`.
- Original structured data and derived tables/figures: CC BY 4.0, see `LICENSE-DATA`.
- Third-party publications and source materials are not included and remain under their respective rights holders' terms.

## Citation

Use the metadata in `CITATION.cff`. The release DOI, when minted, is the preferred citation for this archive.
